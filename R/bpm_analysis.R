#' Calculate Player Career Statistics
#'
#' Aggregates player box score statistics across their career, weighting
#' each game by time on ground percentage to emphasize full games.
#'
#' @param player_data Data frame with player-level game data
#' @param stats_to_aggregate Vector of statistic column names to aggregate
#'
#' @return Data frame with one row per player containing:
#'   - player_id, player_name, team
#'   - n_games: Number of games played
#'   - latest_date: Date of most recent game
#'   - avg_time_on_ground: Average time on ground percentage
#'   - {stat}_avg: Career average for each statistic (TOG-weighted)
#'
#' @importFrom dplyr group_by summarise across n_distinct filter
#' @export
#'
#' @examples
#' \dontrun{
#' career_stats <- calculate_player_career_stats(
#'   enhanced_player_data,
#'   stats_to_aggregate = c("disposals", "goals", "marks", "tackles")
#' )
#' }
calculate_player_career_stats <- function(player_data,
                                          stats_to_aggregate = c(
                                            "disposals", "goals", "marks", "tackles",
                                            "hitouts", "contested_possessions",
                                            "uncontested_possessions", "effective_disposals",
                                            "metres_gained", "intercepts", "inside_fifties",
                                            "clangers", "frees_for", "frees_against"
                                          )) {

  cat("Calculating player career statistics...\n")

  # Filter to available stats
  available_stats <- intersect(stats_to_aggregate, names(player_data))
  missing_stats <- setdiff(stats_to_aggregate, names(player_data))

  if (length(missing_stats) > 0) {
    cat("  Note: Missing stats:", paste(missing_stats, collapse = ", "), "\n")
  }

  cat(sprintf("  Aggregating %d statistics\n", length(available_stats)))

  # Filter to games with non-missing TOG
  career_data <- player_data %>%
    filter(!is.na(time_on_ground_percentage))

  # Calculate weighted averages (weight by TOG)
  career_stats <- career_data %>%
    group_by(player_id, player_name, team) %>%
    summarise(
      n_games = n_distinct(match_id),
      latest_date = max(date, na.rm = TRUE),
      avg_time_on_ground = mean(time_on_ground_percentage, na.rm = TRUE),
      # Weighted mean for each stat: (stat * TOG) / mean(TOG)
      across(
        all_of(available_stats),
        ~ weighted.mean(.x, w = time_on_ground_percentage, na.rm = TRUE),
        .names = "{.col}_avg"
      ),
      .groups = "drop"
    )

  cat(sprintf("  Career stats calculated for %d players\n", nrow(career_stats)))
  cat(sprintf("  Games played range: %d to %d\n",
              min(career_stats$n_games), max(career_stats$n_games)))

  return(career_stats)
}


#' Prepare BPM Regression Data
#'
#' Joins player career statistics with RAPM ratings to create regression dataset
#' for learning BPM coefficients. RAPM serves as the target variable (y).
#'
#' @param career_stats Data frame from calculate_player_career_stats()
#' @param rapm_ratings Data frame with player RAPM ratings
#'
#' @return Data frame ready for BPM regression with:
#'   - Player info
#'   - RAPM rating (target)
#'   - Career average stats (predictors)
#'   - Regression weight (sqrt of games played)
#'
#' @importFrom dplyr inner_join mutate select
#' @export
#'
#' @examples
#' \dontrun{
#' bpm_data <- prepare_bpm_regression_data(career_stats, rapm_ratings)
#' }
prepare_bpm_regression_data <- function(career_stats, rapm_ratings) {

  cat("\nPreparing BPM regression data...\n")

  # Join career stats with RAPM ratings
  bpm_data <- career_stats %>%
    inner_join(
      rapm_ratings %>% select(player_id, rapm_rating),
      by = "player_id"
    ) %>%
    mutate(
      regression_weight = sqrt(n_games)
    )

  cat(sprintf("  Players with both stats and RAPM: %d\n", nrow(bpm_data)))
  cat(sprintf("  RAPM rating range: %.2f to %.2f\n",
              min(bpm_data$rapm_rating), max(bpm_data$rapm_rating)))

  return(bpm_data)
}


#' Fit BPM Model
#'
#' Fits an elastic net regression model to predict RAPM ratings from box score
#' statistics. The resulting coefficients form the BPM (Box Plus/Minus) formula.
#'
#' @param bpm_data Data frame from prepare_bpm_regression_data()
#' @param predictor_cols Vector of predictor column names (career stat averages)
#' @param alpha Elastic net mixing parameter (0=ridge, 1=lasso, default=0.5)
#' @param nfolds Number of cross-validation folds (default=10)
#'
#' @return List with:
#'   - model: Fitted cv.glmnet model
#'   - lambda_min: Optimal lambda
#'   - r2: In-sample R-squared
#'   - rmse: In-sample RMSE
#'   - predictions: In-sample predictions
#'
#' @importFrom glmnet cv.glmnet
#' @export
#'
#' @examples
#' \dontrun{
#' bpm_model <- fit_bpm_model(bpm_data, predictor_cols, alpha = 0.5)
#' }
fit_bpm_model <- function(bpm_data, predictor_cols, alpha = 0.5, nfolds = 10) {

  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("glmnet package required. Install with: install.packages('glmnet')")
  }

  cat("\nFitting BPM model with elastic net regression...\n")
  cat(sprintf("  Alpha: %.2f (0=ridge, 1=lasso)\n", alpha))

  # Prepare matrices
  X <- as.matrix(bpm_data[, predictor_cols])
  y <- bpm_data$rapm_rating
  weights <- bpm_data$regression_weight

  # Fit elastic net with cross-validation
  set.seed(42)
  cv_model <- glmnet::cv.glmnet(
    x = X,
    y = y,
    weights = weights,
    alpha = alpha,
    nfolds = min(nfolds, nrow(X)),
    standardize = TRUE  # Standardize predictors
  )

  # Calculate in-sample performance at lambda.min
  predictions <- as.vector(predict(cv_model, newx = X, s = "lambda.min"))
  r2 <- 1 - sum((y - predictions)^2) / sum((y - mean(y))^2)
  rmse <- sqrt(mean((y - predictions)^2))

  cat(sprintf("\nCross-Validation Results:\n"))
  cat(sprintf("  Lambda (min):     %.6f\n", cv_model$lambda.min))
  cat(sprintf("  Lambda (1se):     %.6f\n", cv_model$lambda.1se))
  cat(sprintf("  CV MSE (min):     %.2f\n", min(cv_model$cvm)))
  cat(sprintf("\nIn-Sample Performance:\n"))
  cat(sprintf("  R²:               %.4f\n", r2))
  cat(sprintf("  RMSE:             %.2f\n", rmse))
  cat(sprintf("  Correlation:      %.4f\n", cor(y, predictions)))

  return(list(
    model = cv_model,
    lambda_min = cv_model$lambda.min,
    lambda_1se = cv_model$lambda.1se,
    r2 = r2,
    rmse = rmse,
    predictions = predictions,
    y_actual = y
  ))
}


#' Extract BPM Coefficients
#'
#' Extracts the BPM formula coefficients from the fitted elastic net model.
#' These coefficients define how each box score statistic contributes to
#' overall player value (BPM rating).
#'
#' @param bpm_model List from fit_bpm_model()
#' @param predictor_cols Vector of predictor column names
#' @param lambda Which lambda to use: "min" or "1se" (default: "min")
#'
#' @return Data frame with columns:
#'   - statistic: Name of the box score stat
#'   - coefficient: BPM coefficient (weight)
#'   - abs_coefficient: Absolute value for ranking importance
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bpm_formula <- extract_bpm_coefficients(bpm_model, predictor_cols)
#' }
extract_bpm_coefficients <- function(bpm_model, predictor_cols, lambda = "min") {

  # Extract coefficients
  lambda_val <- if (lambda == "min") bpm_model$lambda_min else bpm_model$lambda_1se
  coeffs <- as.vector(coef(bpm_model$model, s = lambda_val))

  # First coefficient is intercept
  intercept <- coeffs[1]
  stat_coeffs <- coeffs[-1]

  cat("\nExtracting BPM formula coefficients...\n")
  cat(sprintf("  Intercept: %.4f\n", intercept))

  # Create coefficient data frame
  bpm_formula <- data.frame(
    statistic = gsub("_avg$", "", predictor_cols),
    coefficient = stat_coeffs,
    abs_coefficient = abs(stat_coeffs),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::arrange(dplyr::desc(abs_coefficient))

  # Count non-zero coefficients
  n_nonzero <- sum(stat_coeffs != 0)
  cat(sprintf("  Non-zero coefficients: %d / %d\n", n_nonzero, length(stat_coeffs)))

  cat("\nBPM Formula (sorted by importance):\n")
  print(bpm_formula, row.names = FALSE)

  return(list(
    intercept = intercept,
    coefficients = bpm_formula,
    formula_df = bpm_formula
  ))
}


#' Calculate BPM Ratings
#'
#' Applies the BPM formula to player statistics to generate Box Plus/Minus ratings.
#' BPM provides a simpler alternative to RAPM that only requires box score stats.
#'
#' @param career_stats Data frame with player career statistics
#' @param bpm_coefficients List from extract_bpm_coefficients()
#' @param predictor_cols Vector of predictor column names
#'
#' @return Data frame with player BPM ratings
#'
#' @importFrom dplyr mutate arrange desc select
#' @export
#'
#' @examples
#' \dontrun{
#' bpm_ratings <- calculate_bpm_ratings(career_stats, bpm_coefficients, predictor_cols)
#' }
calculate_bpm_ratings <- function(career_stats, bpm_coefficients, predictor_cols) {

  cat("\nCalculating BPM ratings for all players...\n")

  # Initialize BPM with intercept
  bpm_ratings <- career_stats %>%
    mutate(bpm_rating = bpm_coefficients$intercept)

  # Add contribution from each statistic
  coeff_df <- bpm_coefficients$coefficients

  for (i in seq_len(nrow(coeff_df))) {
    stat_name <- coeff_df$statistic[i]
    stat_col <- paste0(stat_name, "_avg")
    coeff_val <- coeff_df$coefficient[i]

    if (stat_col %in% names(bpm_ratings) && coeff_val != 0) {
      bpm_ratings <- bpm_ratings %>%
        mutate(
          bpm_rating = bpm_rating + !!rlang::sym(stat_col) * coeff_val
        )
    }
  }

  # Add rank and sort
  bpm_ratings <- bpm_ratings %>%
    mutate(
      bpm_rank = rank(-bpm_rating, ties.method = "min")
    ) %>%
    arrange(desc(bpm_rating))

  cat(sprintf("  BPM calculated for %d players\n", nrow(bpm_ratings)))
  cat(sprintf("  BPM range: %.2f to %.2f\n",
              min(bpm_ratings$bpm_rating), max(bpm_ratings$bpm_rating)))

  return(bpm_ratings)
}


#' Compare BPM and RAPM Ratings
#'
#' Compares Box Plus/Minus (BPM) ratings with Regularized Adjusted Plus/Minus (RAPM)
#' ratings to evaluate how well the simpler box score metric approximates the
#' more complex lineup-based metric.
#'
#' @param bpm_ratings Data frame with BPM ratings
#' @param rapm_ratings Data frame with RAPM ratings
#'
#' @return Data frame with both ratings for comparison
#'
#' @importFrom dplyr inner_join select mutate
#' @export
#'
#' @examples
#' \dontrun{
#' comparison <- compare_bpm_rapm(bpm_ratings, rapm_ratings)
#' }
compare_bpm_rapm <- function(bpm_ratings, rapm_ratings) {

  cat("\nComparing BPM and RAPM ratings...\n")

  comparison <- bpm_ratings %>%
    select(player_id, player_name, team, n_games, bpm_rating, bpm_rank) %>%
    inner_join(
      rapm_ratings %>% select(player_id, rapm_rating, rank),
      by = "player_id"
    ) %>%
    mutate(
      rating_diff = bpm_rating - rapm_rating,
      rank_diff = rank - bpm_rank
    ) %>%
    dplyr::rename(rapm_rank = rank)

  # Calculate correlation
  cor_val <- cor(comparison$bpm_rating, comparison$rapm_rating, use = "complete.obs")

  cat(sprintf("  Players in comparison: %d\n", nrow(comparison)))
  cat(sprintf("  Correlation: %.4f\n", cor_val))
  cat(sprintf("  Mean absolute difference: %.2f\n",
              mean(abs(comparison$rating_diff))))

  # Identify biggest discrepancies
  cat("\nBiggest BPM overestimates (BPM >> RAPM):\n")
  print(comparison %>%
          dplyr::arrange(dplyr::desc(rating_diff)) %>%
          dplyr::slice_head(n = 5) %>%
          dplyr::select(player_name, team, bpm_rating, rapm_rating, rating_diff))

  cat("\nBiggest BPM underestimates (BPM << RAPM):\n")
  print(comparison %>%
          dplyr::arrange(rating_diff) %>%
          dplyr::slice_head(n = 5) %>%
          dplyr::select(player_name, team, bpm_rating, rapm_rating, rating_diff))

  return(comparison)
}


#' Calculate Game-Level BPM
#'
#' Applies the BPM formula to individual player-game data to generate
#' game-by-game BPM ratings. This shows how player performance varies
#' from game to game.
#'
#' @param player_game_data Data frame with player-game level statistics
#' @param bpm_coefficients List from extract_bpm_coefficients() with formula
#'
#' @return Data frame with game-level BPM ratings, including:
#'   - All player/game identifiers
#'   - Game statistics
#'   - bpm_game: BPM rating for this specific game
#'
#' @importFrom dplyr mutate arrange desc select filter
#' @export
#'
#' @examples
#' \dontrun{
#' game_bpm <- calculate_game_level_bpm(enhanced_player_data, bpm_coefficients)
#' }
calculate_game_level_bpm <- function(player_game_data, bpm_coefficients) {

  cat("\nCalculating game-level BPM ratings...\n")

  # Get coefficient data frame
  coeff_df <- bpm_coefficients$coefficients
  intercept <- bpm_coefficients$intercept

  # Initialize with intercept
  game_bpm <- player_game_data %>%
    mutate(bpm_game = intercept)

  # Track which stats are used
  stats_used <- 0
  stats_missing <- c()

  # Add contribution from each statistic
  for (i in seq_len(nrow(coeff_df))) {
    stat_name <- coeff_df$statistic[i]
    coeff_val <- coeff_df$coefficient[i]

    # Skip if coefficient is zero
    if (coeff_val == 0) next

    # Check if stat exists in data
    if (stat_name %in% names(game_bpm)) {
      game_bpm <- game_bpm %>%
        mutate(
          bpm_game = bpm_game + !!rlang::sym(stat_name) * coeff_val
        )
      stats_used <- stats_used + 1
    } else {
      stats_missing <- c(stats_missing, stat_name)
    }
  }

  cat(sprintf("  Statistics used: %d\n", stats_used))
  if (length(stats_missing) > 0) {
    cat("  Missing stats:", paste(stats_missing, collapse = ", "), "\n")
  }

  cat(sprintf("  Game-level BPM calculated for %d player-games\n", nrow(game_bpm)))
  cat(sprintf("  BPM range: %.2f to %.2f\n",
              min(game_bpm$bpm_game, na.rm = TRUE),
              max(game_bpm$bpm_game, na.rm = TRUE)))

  return(game_bpm)
}


#' Calculate Player Consistency Metrics
#'
#' Analyzes game-to-game variance in player BPM ratings to identify
#' consistent vs volatile players.
#'
#' @param game_level_bpm Data frame from calculate_game_level_bpm()
#'
#' @return Data frame with player consistency metrics:
#'   - player_id, player_name, team
#'   - n_games: Number of games
#'   - mean_bpm: Average game-level BPM
#'   - sd_bpm: Standard deviation of BPM (consistency)
#'   - cv_bpm: Coefficient of variation (sd/mean)
#'   - min_bpm: Worst game
#'   - max_bpm: Best game
#'   - range_bpm: max - min
#'
#' @importFrom dplyr group_by summarise n arrange desc
#' @export
#'
#' @examples
#' \dontrun{
#' consistency <- calculate_player_consistency(game_level_bpm)
#' }
calculate_player_consistency <- function(game_level_bpm) {

  cat("\nCalculating player consistency metrics...\n")

  consistency <- game_level_bpm %>%
    filter(!is.na(bpm_game)) %>%
    group_by(player_id, player_name, team) %>%
    summarise(
      n_games = n(),
      mean_bpm = mean(bpm_game, na.rm = TRUE),
      sd_bpm = sd(bpm_game, na.rm = TRUE),
      min_bpm = min(bpm_game, na.rm = TRUE),
      max_bpm = max(bpm_game, na.rm = TRUE),
      range_bpm = max_bpm - min_bpm,
      .groups = "drop"
    ) %>%
    mutate(
      cv_bpm = sd_bpm / abs(mean_bpm)  # Coefficient of variation
    )

  cat(sprintf("  Consistency calculated for %d players\n", nrow(consistency)))

  return(consistency)
}
