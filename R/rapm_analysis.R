#' Prepare RAPM Matrix
#'
#' Converts player-game data into a RAPM (Regularized Adjusted Plus/Minus) matrix
#' where each row is a match and columns represent player contributions weighted
#' by time on ground.
#'
#' @param player_data Data frame with player-level data including match_id, player_id,
#'   home_away, time_on_ground_percentage, and margin
#'
#' @return List with:
#'   - rapm_matrix: Sparse matrix with player columns (Matrix package)
#'   - y_values: Vector of home margins
#'   - match_ids: Vector of match identifiers
#'   - player_mapping: Data frame mapping column indices to player info (includes n_games and latest_date)
#'
#' @importFrom dplyr filter select distinct arrange mutate left_join group_by ungroup summarise first n_distinct
#' @importFrom tidyr pivot_wider
#' @export
#'
#' @examples
#' \dontrun{
#' rapm_data <- prepare_rapm_matrix(enhanced_player_data)
#' }
prepare_rapm_matrix <- function(player_data) {

  # Validate inputs
  required_cols <- c("match_id", "player_id", "player_name", "home_away",
                     "time_on_ground_percentage", "margin")
  missing_cols <- setdiff(required_cols, names(player_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  cat("Preparing RAPM matrix...\n")

  # Filter to players with non-missing time_on_ground_percentage
  rapm_data <- player_data %>%
    filter(!is.na(time_on_ground_percentage)) %>%
    select(match_id, player_id, player_name, team, date, home_away,
           time_on_ground_percentage, margin)

  cat(sprintf("  Total player-games: %d\n", nrow(rapm_data)))
  cat(sprintf("  Unique matches: %d\n", length(unique(rapm_data$match_id))))
  cat(sprintf("  Unique players: %d\n", length(unique(rapm_data$player_id))))

  # Calculate player contribution values (positive for home, negative for away)
  rapm_data <- rapm_data %>%
    mutate(
      player_value = ifelse(home_away == "Home",
                           time_on_ground_percentage / 100,
                           -time_on_ground_percentage / 100)
    )

  # Get unique matches with home margins
  match_margins <- rapm_data %>%
    filter(home_away == "Home") %>%
    select(match_id, margin) %>%
    distinct()

  # Create player mapping (all unique players)
  player_mapping <- rapm_data %>%
    select(player_id, player_name, team) %>%
    distinct() %>%
    arrange(player_id) %>%
    mutate(col_index = row_number())

  cat(sprintf("  Players in matrix: %d\n", nrow(player_mapping)))

  # Pivot to wide format: each row is a match, each column is a player
  rapm_wide <- rapm_data %>%
    select(match_id, player_id, player_value) %>%
    pivot_wider(
      id_cols = match_id,
      names_from = player_id,
      values_from = player_value,
      values_fill = 0
    )

  # Join with match margins
  rapm_wide <- rapm_wide %>%
    left_join(match_margins, by = "match_id")

  # Extract components
  match_ids <- rapm_wide$match_id
  y_values <- rapm_wide$margin

  # Extract player columns (exclude match_id and margin)
  player_cols <- setdiff(names(rapm_wide), c("match_id", "margin"))
  X_matrix <- as.matrix(rapm_wide[, player_cols])

  # Convert to sparse matrix for efficiency
  if (requireNamespace("Matrix", quietly = TRUE)) {
    X_matrix <- Matrix::Matrix(X_matrix, sparse = TRUE)
    cat("  Using sparse matrix format\n")
  }

  # Update player mapping to match column order and add game stats
  player_mapping <- data.frame(
    player_id = player_cols,
    col_index = seq_along(player_cols),
    stringsAsFactors = FALSE
  ) %>%
    left_join(
      rapm_data %>%
        group_by(player_id) %>%
        summarise(
          player_name = first(player_name),
          team = first(team),
          n_games = n_distinct(match_id),
          latest_date = max(date, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "player_id"
    )

  cat(sprintf("\nRAPM Matrix Dimensions: %d matches x %d players\n",
              nrow(X_matrix), ncol(X_matrix)))
  cat(sprintf("Non-zero entries: %.1f%%\n",
              100 * sum(X_matrix != 0) / (nrow(X_matrix) * ncol(X_matrix))))

  return(list(
    rapm_matrix = X_matrix,
    y_values = y_values,
    match_ids = match_ids,
    player_mapping = player_mapping
  ))
}


#' Fit RAPM Model
#'
#' Fits a ridge regression model (alpha=0) to estimate player contributions
#' to team margin using cross-validation to select optimal regularization.
#'
#' @param rapm_data List from prepare_rapm_matrix() with rapm_matrix and y_values
#' @param nfolds Number of cross-validation folds (default: 10)
#' @param lambda_seq Optional sequence of lambda values to try
#'
#' @return List with:
#'   - model: Fitted cv.glmnet model object
#'   - lambda_min: Optimal lambda value
#'   - lambda_1se: Lambda within 1 SE of minimum
#'   - r2: In-sample R-squared at lambda.min
#'   - rmse: In-sample RMSE at lambda.min
#'
#' @importFrom glmnet cv.glmnet
#' @export
#'
#' @examples
#' \dontrun{
#' rapm_data <- prepare_rapm_matrix(player_data)
#' rapm_model <- fit_rapm_model(rapm_data, nfolds = 10)
#' }
fit_rapm_model <- function(rapm_data, nfolds = 10, lambda_seq = NULL) {

  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("glmnet package required for RAPM. Install with: install.packages('glmnet')")
  }

  cat("\nFitting RAPM model with ridge regression (alpha=0)...\n")

  X <- rapm_data$rapm_matrix
  y <- rapm_data$y_values

  # Fit cross-validated ridge regression
  set.seed(42)  # For reproducibility
  if (is.null(lambda_seq)) {
    cv_model <- glmnet::cv.glmnet(
      x = X,
      y = y,
      alpha = 0,  # Ridge regression
      nlambda = 400,                # denser grid
      lambda.min.ratio = 1e-6,      # was ~1e-4 by default; go smaller
      # relax = TRUE,
      nfolds = min(nfolds, nrow(X)),
      standardize = FALSE  # Already on same scale (time on ground %)
    )
  } else {
    cv_model <- glmnet::cv.glmnet(
      x = X,
      y = y,
      alpha = 0,
      nlambda = 400,                # denser grid
      lambda.min.ratio = 1e-6,      # was ~1e-4 by default; go smaller
      lambda = lambda_seq,
      nfolds = min(nfolds, nrow(X)),
      standardize = FALSE
    )
  }

  # Calculate in-sample performance at lambda.min
  predictions <- as.vector(predict(cv_model, newx = X, s = "lambda.min"))
  r2 <- 1 - sum((y - predictions)^2) / sum((y - mean(y))^2)
  rmse <- sqrt(mean((y - predictions)^2))

  cat(sprintf("\nCross-Validation Results:\n"))
  cat(sprintf("  Lambda (min):     %.6f\n", cv_model$lambda.min))
  cat(sprintf("  Lambda (1se):     %.6f\n", cv_model$lambda.1se))
  cat(sprintf("  CV MSE (min):     %.2f\n", min(cv_model$cvm)))
  cat(sprintf("\nIn-Sample Performance (lambda.min):\n"))
  cat(sprintf("  R²:               %.4f\n", r2))
  cat(sprintf("  RMSE:             %.2f points\n", rmse))

  return(list(
    model = cv_model,
    lambda_min = cv_model$lambda.min,
    lambda_1se = cv_model$lambda.1se,
    r2 = r2,
    rmse = rmse
  ))
}


#' Extract RAPM Ratings
#'
#' Extracts player coefficients from fitted RAPM model and formats as ratings.
#' Coefficients represent each player's contribution to team margin per 100%
#' time on ground, controlling for teammates and opponents.
#'
#' @param rapm_model List from fit_rapm_model() with model object
#' @param rapm_data List from prepare_rapm_matrix() with player_mapping
#' @param lambda Which lambda to use: "min" or "1se" (default: "min")
#'
#' @return Data frame with columns:
#'   - player_id: Player identifier
#'   - player_name: Player name
#'   - team: Team (most recent)
#'   - n_games: Number of games played
#'   - latest_date: Date of most recent game
#'   - rapm_rating: Player's contribution to margin (points per 100% TOG)
#'   - rapm_rating_per_game: Estimated contribution per full game
#'   - rank: Ranking (1 = best)
#'
#' @importFrom dplyr arrange mutate desc
#' @export
#'
#' @examples
#' \dontrun{
#' rapm_ratings <- extract_rapm_ratings(rapm_model, rapm_data)
#' }
extract_rapm_ratings <- function(rapm_model, rapm_data, lambda = "min") {

  # Extract coefficients at specified lambda
  lambda_val <- if (lambda == "min") rapm_model$lambda_min else rapm_model$lambda_1se
  coeffs <- as.vector(coef(rapm_model$model, s = lambda_val))

  # First coefficient is intercept, rest are player coefficients
  intercept <- coeffs[1]
  player_coeffs <- coeffs[-1]

  cat(sprintf("\nExtracting RAPM ratings (lambda = %s)...\n", lambda))
  cat(sprintf("  Intercept: %.2f\n", intercept))

  # Match coefficients to players
  rapm_ratings <- rapm_data$player_mapping
  rapm_ratings$rapm_rating <- player_coeffs

  # Add per-game estimates (assuming ~80% average TOG)
  rapm_ratings <- rapm_ratings %>%
    mutate(
      rapm_rating_per_game = rapm_rating * 1,
      rank = rank(-rapm_rating, ties.method = "min")
    ) %>%
    arrange(desc(rapm_rating))

  cat(sprintf("\nRAPM Rating Summary:\n"))
  cat(sprintf("  Players rated:    %d\n", nrow(rapm_ratings)))
  cat(sprintf("  Best rating:      %.2f\n", max(rapm_ratings$rapm_rating)))
  cat(sprintf("  Worst rating:     %.2f\n", min(rapm_ratings$rapm_rating)))
  cat(sprintf("  Mean rating:      %.2f\n", mean(rapm_ratings$rapm_rating)))
  cat(sprintf("  Median rating:    %.2f\n", median(rapm_ratings$rapm_rating)))

  return(rapm_ratings)
}


#' Aggregate RAPM Ratings to Team Level
#'
#' Summarizes player RAPM ratings by team to estimate overall team strength.
#'
#' @param rapm_ratings Data frame from extract_rapm_ratings()
#'
#' @return Data frame with team-level RAPM metrics
#'
#' @importFrom dplyr group_by summarise n arrange desc
#' @export
aggregate_rapm_by_team <- function(rapm_ratings) {

  team_rapm <- rapm_ratings %>%
    group_by(team) %>%
    summarise(
      n_players = n(),
      total_rapm = sum(rapm_rating, na.rm = TRUE),
      mean_rapm = mean(rapm_rating, na.rm = TRUE),
      median_rapm = median(rapm_rating, na.rm = TRUE),
      top_player_rapm = max(rapm_rating, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_rapm))

  return(team_rapm)
}
