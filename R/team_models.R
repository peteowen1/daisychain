#' Calculate Team Strength Ratings
#'
#' Calculate Elo-style team strength ratings based on match results.
#'
#' @param results_data Data frame with match results
#' @param initial_rating Initial rating for all teams (default: 1500)
#' @param k_factor K-factor for rating updates (default: 32)
#' @param home_advantage Home advantage in rating points (default: 50)
#' @return Data frame with team ratings over time
#' @export
#' @examples
#' \dontrun{
#' team_ratings <- calculate_team_elo_ratings(clean_results)
#' }
calculate_team_elo_ratings <- function(results_data,
                                       initial_rating = 1500,
                                       k_factor = 32,
                                       home_advantage = 50) {

  # Initialize team ratings
  teams <- unique(c(results_data$home_team, results_data$away_team))
  team_ratings <- data.frame(
    team = teams,
    rating = initial_rating,
    stringsAsFactors = FALSE
  )

  # Sort results by date
  sorted_results <- results_data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      home_rating_before = NA_real_,
      away_rating_before = NA_real_,
      home_rating_after = NA_real_,
      away_rating_after = NA_real_,
      expected_home_score = NA_real_
    )

  # Process each match
  for (i in 1:nrow(sorted_results)) {
    match <- sorted_results[i, ]

    # Get current ratings
    home_rating <- team_ratings$rating[team_ratings$team == match$home_team]
    away_rating <- team_ratings$rating[team_ratings$team == match$away_team]

    # Store ratings before match
    sorted_results$home_rating_before[i] <- home_rating
    sorted_results$away_rating_before[i] <- away_rating

    # Calculate expected outcome (with home advantage)
    expected_home <- 1 / (1 + 10^((away_rating - (home_rating + home_advantage)) / 400))
    sorted_results$expected_home_score[i] <- expected_home

    # Actual outcome
    if (match$margin > 0) {
      actual_home <- 1  # Home win
    } else if (match$margin < 0) {
      actual_home <- 0  # Away win
    } else {
      actual_home <- 0.5  # Draw
    }

    # Update ratings
    home_rating_new <- home_rating + k_factor * (actual_home - expected_home)
    away_rating_new <- away_rating + k_factor * ((1 - actual_home) - (1 - expected_home))

    # Store new ratings
    sorted_results$home_rating_after[i] <- home_rating_new
    sorted_results$away_rating_after[i] <- away_rating_new

    # Update team ratings table
    team_ratings$rating[team_ratings$team == match$home_team] <- home_rating_new
    team_ratings$rating[team_ratings$team == match$away_team] <- away_rating_new
  }

  # Create long format for easier analysis
  rating_history <- sorted_results %>%
    dplyr::select(date, season, round_number, match_id,
                  home_team, away_team, margin,
                  home_rating_before, away_rating_before,
                  home_rating_after, away_rating_after,
                  expected_home_score) %>%
    tidyr::pivot_longer(
      cols = c(home_team, away_team),
      names_to = "home_away",
      values_to = "team"
    ) %>%
    dplyr::mutate(
      home_away = dplyr::recode(home_away, "home_team" = "Home", "away_team" = "Away"),
      rating_before = ifelse(home_away == "Home", home_rating_before, away_rating_before),
      rating_after = ifelse(home_away == "Home", home_rating_after, away_rating_after),
      rating_change = rating_after - rating_before
    ) %>%
    dplyr::select(date, season, round_number, team, home_away,
                  rating_before, rating_after, rating_change, margin)

  return(rating_history)
}


#' Calculate Team Strength Ratings (Margin-Based)
#'
#' Calculate Elo-style team strength ratings using margin of victory.
#' Uses sqrt(margin) to moderate the impact of blowouts.
#'
#' @param results_data Data frame with match results
#' @param initial_rating Initial rating for all teams (default: 1500)
#' @param k_factor K-factor for rating updates (default: 32)
#' @param home_advantage Home advantage in rating points (default: 50)
#' @param margin_scale Scaling factor for margin impact (default: 20)
#' @return Data frame with team ratings over time
#' @export
#' @examples
#' \dontrun{
#' team_ratings <- calculate_margin_elo_ratings(clean_results)
#' }
calculate_margin_elo_ratings <- function(results_data,
                                          initial_rating = 1500,
                                          k_factor = 32,
                                          home_advantage = 50,
                                          margin_scale = 20) {

  # Initialize team ratings
  teams <- unique(c(results_data$home_team, results_data$away_team))
  team_ratings <- data.frame(
    team = teams,
    rating = initial_rating,
    stringsAsFactors = FALSE
  )

  # Sort results by date
  sorted_results <- results_data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      home_rating_before = NA_real_,
      away_rating_before = NA_real_,
      home_rating_after = NA_real_,
      away_rating_after = NA_real_,
      expected_home_score = NA_real_,
      actual_home_score = NA_real_
    )

  # Process each match
  for (i in 1:nrow(sorted_results)) {
    match <- sorted_results[i, ]

    # Get current ratings
    home_rating <- team_ratings$rating[team_ratings$team == match$home_team]
    away_rating <- team_ratings$rating[team_ratings$team == match$away_team]

    # Store ratings before match
    sorted_results$home_rating_before[i] <- home_rating
    sorted_results$away_rating_before[i] <- away_rating

    # Calculate expected outcome (with home advantage)
    expected_home <- 1 / (1 + 10^((away_rating - (home_rating + home_advantage)) / 400))
    sorted_results$expected_home_score[i] <- expected_home

    # Actual outcome based on margin (using sqrt to moderate blowouts)
    # Score ranges from 0 (big away win) to 1 (big home win), with 0.5 as draw
    margin_component <- sqrt(abs(match$margin)) * sign(match$margin)
    actual_home <- 0.5 + (margin_component / (2 * margin_scale))

    # Clamp to [0, 1] range
    actual_home <- pmax(0, pmin(1, actual_home))
    sorted_results$actual_home_score[i] <- actual_home

    # Update ratings - margin-based updates
    home_rating_new <- home_rating + k_factor * (actual_home - expected_home)
    away_rating_new <- away_rating + k_factor * ((1 - actual_home) - (1 - expected_home))

    # Store new ratings
    sorted_results$home_rating_after[i] <- home_rating_new
    sorted_results$away_rating_after[i] <- away_rating_new

    # Update team ratings table
    team_ratings$rating[team_ratings$team == match$home_team] <- home_rating_new
    team_ratings$rating[team_ratings$team == match$away_team] <- away_rating_new
  }

  # Create long format for easier analysis
  rating_history <- sorted_results %>%
    dplyr::select(date, season, round_number, match_id,
                  home_team, away_team, margin,
                  home_rating_before, away_rating_before,
                  home_rating_after, away_rating_after,
                  expected_home_score, actual_home_score) %>%
    tidyr::pivot_longer(
      cols = c(home_team, away_team),
      names_to = "home_away",
      values_to = "team"
    ) %>%
    dplyr::mutate(
      home_away = dplyr::recode(home_away, "home_team" = "Home", "away_team" = "Away"),
      rating_before = ifelse(home_away == "Home", home_rating_before, away_rating_before),
      rating_after = ifelse(home_away == "Home", home_rating_after, away_rating_after),
      rating_change = rating_after - rating_before
    ) %>%
    dplyr::select(date, season, round_number, team, home_away,
                  rating_before, rating_after, rating_change, margin)

  return(rating_history)
}


#' Build Team Performance Model
#'
#' Build a model to predict team performance based on various team metrics.
#'
#' @param team_data Data frame with team-level features and performance metrics
#' @param target_var Target variable to predict (default: "win_percentage")
#' @param predictor_vars Vector of predictor variables (if NULL, auto-select)
#' @param model_type Type of model ("lm", "glmnet", "rf")
#' @param include_season_effects Whether to include season as a factor
#' @return List containing trained model and performance metrics
#' @export
#' @examples
#' \dontrun{
#' team_model <- build_team_performance_model(team_strength_data)
#' }
build_team_performance_model <- function(team_data,
                                         target_var = "win_percentage",
                                         predictor_vars = NULL,
                                         model_type = "lm",
                                         include_season_effects = TRUE) {

  # Auto-select predictors if not specified
  if (is.null(predictor_vars)) {
    predictor_vars <- c(
      "avg_score_for", "avg_score_against", "avg_margin",
      "recent_form", "home_win_pct", "away_win_pct",
      "avg_team_rating", "depth_score", "star_power",
      "consistency", "rating_after"
    )
    # Keep only existing columns
    predictor_vars <- predictor_vars[predictor_vars %in% names(team_data)]
  }

  if (include_season_effects && "season" %in% names(team_data)) {
    predictor_vars <- c(predictor_vars, "season")
  }

  # Check we have enough predictors
  if (length(predictor_vars) < 2) {
    warning("Insufficient predictor variables available")
    return(NULL)
  }

  # Prepare modeling data
  modeling_data <- team_data %>%
    dplyr::select(dplyr::all_of(c(target_var, predictor_vars, "team"))) %>%
    tidyr::drop_na()

  if (nrow(modeling_data) < 20) {
    warning("Insufficient data for team modeling")
    return(NULL)
  }

  # Convert season to factor if included
  if ("season" %in% names(modeling_data)) {
    modeling_data$season <- as.factor(modeling_data$season)
  }

  # Prepare formula
  formula_str <- paste(target_var, "~", paste(predictor_vars, collapse = " + "))
  model_formula <- stats::as.formula(formula_str)

  # Split data (leave-one-season-out if multiple seasons)
  if ("season" %in% names(modeling_data) && length(unique(modeling_data$season)) > 1) {
    # Use latest season as test set
    seasons <- sort(unique(modeling_data$season))
    test_season <- seasons[length(seasons)]
    train_data <- modeling_data %>% dplyr::filter(season != test_season)
    test_data <- modeling_data %>% dplyr::filter(season == test_season)
  } else {
    # Random split
    set.seed(42)
    train_indices <- sample(nrow(modeling_data), round(nrow(modeling_data) * 0.8))
    train_data <- modeling_data[train_indices, ]
    test_data <- modeling_data[-train_indices, ]
  }

  # Fit model
  if (model_type == "lm") {
    model <- stats::lm(model_formula, data = train_data)
    predictions <- stats::predict(model, newdata = test_data)

  } else if (model_type == "glmnet" && requireNamespace("glmnet", quietly = TRUE)) {
    train_matrix <- stats::model.matrix(model_formula, data = train_data)[, -1]
    test_matrix <- stats::model.matrix(model_formula, data = test_data)[, -1]
    train_y <- train_data[[target_var]]

    cv_model <- glmnet::cv.glmnet(train_matrix, train_y, alpha = 0.5)
    model <- glmnet::glmnet(train_matrix, train_y, alpha = 0.5, lambda = cv_model$lambda.min)
    predictions <- as.numeric(stats::predict(model, newx = test_matrix, s = cv_model$lambda.min))

  } else if (model_type == "rf" && requireNamespace("randomForest", quietly = TRUE)) {
    model <- randomForest::randomForest(model_formula, data = train_data, ntree = 500)
    predictions <- stats::predict(model, newdata = test_data)
  }

  # Calculate performance metrics
  actual <- test_data[[target_var]]
  mae <- mean(abs(predictions - actual), na.rm = TRUE)
  rmse <- sqrt(mean((predictions - actual)^2, na.rm = TRUE))
  r_squared <- stats::cor(predictions, actual, use = "complete.obs")^2

  # Feature importance
  feature_importance <- NULL
  if (model_type == "lm") {
    coef_summary <- broom::tidy(model)
    feature_importance <- coef_summary %>%
      dplyr::filter(term != "(Intercept)") %>%
      dplyr::mutate(importance = abs(estimate)) %>%
      dplyr::select(variable = term, coefficient = estimate, importance, p_value = p.value) %>%
      dplyr::arrange(dplyr::desc(importance))
  } else if (model_type == "rf" && inherits(model, "randomForest")) {
    feature_importance <- data.frame(
      variable = names(model$importance),
      importance = as.numeric(model$importance)
    ) %>%
      dplyr::arrange(dplyr::desc(importance))
  }

  result <- list(
    model = model,
    model_type = model_type,
    target_var = target_var,
    predictor_vars = predictor_vars,
    performance = list(
      mae = mae,
      rmse = rmse,
      r_squared = r_squared,
      n_train = nrow(train_data),
      n_test = nrow(test_data)
    ),
    feature_importance = feature_importance,
    predictions = data.frame(
      team = test_data$team,
      actual = actual,
      predicted = predictions,
      residual = actual - predictions
    ),
    train_data = train_data,
    test_data = test_data
  )

  return(result)
}

#' Analyze Team Roster Strength
#'
#' Analyze team strength based on player ratings and roster composition.
#'
#' @param player_ratings Data frame with player ratings
#' @param lineup_data Data frame with team lineups (optional)
#' @param min_games Minimum games for player inclusion (default: 3)
#' @return Data frame with team roster strength metrics
#' @export
#' @examples
#' \dontrun{
#' roster_strength <- analyze_team_roster_strength(player_ratings)
#' }
analyze_team_roster_strength <- function(player_ratings,
                                         lineup_data = NULL,
                                         min_games = 3) {

  # Calculate player averages
  player_summary <- player_ratings %>%
    dplyr::group_by(player_id, player_name, team) %>%
    dplyr::summarise(
      games_played = dplyr::n(),
      avg_rating = mean(overall_rating, na.rm = TRUE),
      avg_margin_contribution = mean(signed_margin_contribution, na.rm = TRUE),
      consistency = sd(overall_rating, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(games_played >= min_games)

  # Team roster analysis
  team_roster_strength <- player_summary %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      roster_size = dplyr::n(),
      avg_team_rating = mean(avg_rating, na.rm = TRUE),
      median_team_rating = stats::median(avg_rating, na.rm = TRUE),

      # Top players
      top_player_rating = max(avg_rating, na.rm = TRUE),
      top_3_avg_rating = mean(sort(avg_rating, decreasing = TRUE)[1:min(3, length(avg_rating))]),
      top_5_avg_rating = mean(sort(avg_rating, decreasing = TRUE)[1:min(5, length(avg_rating))]),

      # Depth metrics
      depth_rating = quantile(avg_rating, 0.25, na.rm = TRUE),  # 25th percentile
      worst_regular = min(avg_rating, na.rm = TRUE),

      # Distribution metrics
      rating_range = max(avg_rating, na.rm = TRUE) - min(avg_rating, na.rm = TRUE),
      rating_sd = sd(avg_rating, na.rm = TRUE),

      # Quality tiers
      elite_players = sum(avg_rating >= 8, na.rm = TRUE),
      very_good_players = sum(avg_rating >= 7 & avg_rating < 8, na.rm = TRUE),
      good_players = sum(avg_rating >= 6 & avg_rating < 7, na.rm = TRUE),
      average_players = sum(avg_rating >= 5 & avg_rating < 6, na.rm = TRUE),
      below_average_players = sum(avg_rating < 5, na.rm = TRUE),

      # Margin contribution
      total_margin_contribution = sum(avg_margin_contribution, na.rm = TRUE),
      avg_margin_per_player = mean(avg_margin_contribution, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Composite strength scores
      star_power_score = (top_player_rating * 0.4 + top_3_avg_rating * 0.6),
      depth_score = depth_rating,
      balance_score = 10 - rating_sd,  # Lower SD = better balance

      # Overall roster strength (weighted combination)
      roster_strength = (
        0.4 * pmin(pmax((avg_team_rating - 4) / 4, 0), 1) +
        0.3 * pmin(pmax((star_power_score - 4) / 4, 0), 1) +
        0.2 * pmin(pmax((depth_score - 3) / 4, 0), 1) +
        0.1 * pmin(pmax((balance_score - 5) / 5, 0), 1)
      ) * 10,

      # Roster categories
      roster_category = dplyr::case_when(
        roster_strength >= 8.5 ~ "Elite",
        roster_strength >= 7.5 ~ "Very Strong",
        roster_strength >= 6.5 ~ "Strong",
        roster_strength >= 5.5 ~ "Average",
        roster_strength >= 4.5 ~ "Below Average",
        TRUE ~ "Weak"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(roster_strength))

  return(team_roster_strength)
}

#' Calculate Team Momentum
#'
#' Calculate team momentum based on recent results and performance trends.
#'
#' @param team_results Data frame with team results over time
#' @param window Number of recent games for momentum calculation (default: 5)
#' @return Data frame with momentum metrics
#' @export
#' @examples
#' \dontrun{
#' team_momentum <- calculate_team_momentum(team_results)
#' }
calculate_team_momentum <- function(team_results, window = 5) {

  momentum_data <- team_results %>%
    dplyr::arrange(team, date) %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(
      games_played = dplyr::row_number(),

      # Recent form metrics
      recent_wins = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(win, sum, na.rm = TRUE,
                          .before = window - 1, .complete = FALSE)
      } else {
        as.numeric(stats::filter(win, rep(1, window), sides = 1))
      },
      recent_win_pct = recent_wins / pmin(games_played, window),

      recent_avg_margin = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(margin, mean, na.rm = TRUE,
                          .before = window - 1, .complete = FALSE)
      } else {
        as.numeric(stats::filter(margin, rep(1/window, window), sides = 1))
      },

      recent_avg_score = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(score_for, mean, na.rm = TRUE,
                          .before = window - 1, .complete = FALSE)
      } else {
        as.numeric(stats::filter(score_for, rep(1/window, window), sides = 1))
      },

      # Momentum indicators
      margin_trend = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(seq_along(margin), function(idx) {
          if (length(idx) < 3) return(0)
          recent_margins <- margin[idx]
          if (all(is.na(recent_margins))) return(0)
          tryCatch({
            trend_lm <- stats::lm(recent_margins ~ seq_along(recent_margins))
            return(stats::coef(trend_lm)[2])
          }, error = function(e) 0)
        }, .before = window - 1, .complete = FALSE)
      } else {
        0  # Fallback - no trend calculation
      },

      performance_trend = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(seq_along(win), function(idx) {
          if (length(idx) < 3) return(0)
          recent_wins <- win[idx]
          if (all(is.na(recent_wins))) return(0)
          return(mean(recent_wins[max(1, length(recent_wins)-2):length(recent_wins)]) -
                 mean(recent_wins[1:min(3, length(recent_wins))]))
        }, .before = window - 1, .complete = FALSE)
      } else {
        0  # Fallback - no trend calculation
      }
    ) %>%
    dplyr::mutate(
      # Composite momentum score
      momentum_score = (
        0.4 * pmin(pmax((recent_win_pct - 0.3) / 0.4, 0), 1) +
        0.3 * pmin(pmax((recent_avg_margin + 20) / 40, 0), 1) +
        0.2 * pmin(pmax((margin_trend + 5) / 10, 0), 1) +
        0.1 * pmin(pmax((performance_trend + 0.5) / 1, 0), 1)
      ) * 10,

      # Momentum categories
      momentum_category = dplyr::case_when(
        games_played < window ~ "Insufficient Data",
        momentum_score >= 8 ~ "Very Hot",
        momentum_score >= 6.5 ~ "Hot",
        momentum_score >= 5 ~ "Warm",
        momentum_score >= 3.5 ~ "Cool",
        momentum_score >= 2 ~ "Cold",
        TRUE ~ "Very Cold"
      )
    ) %>%
    dplyr::ungroup()

  return(momentum_data)
}

#' Predict Team Matchup Outcomes
#'
#' Predict outcomes for team matchups based on team strength and context.
#'
#' @param team1 Name of first team
#' @param team2 Name of second team
#' @param team_ratings Data frame with current team ratings/strength
#' @param venue Venue for the match (optional)
#' @param home_team Which team is playing at home
#' @param context_factors Additional context factors (list)
#' @return List with prediction details
#' @export
#' @examples
#' \dontrun{
#' prediction <- predict_team_matchup("Adelaide", "Carlton",
#'                                   team_ratings, home_team = "Adelaide")
#' }
predict_team_matchup <- function(team1,
                                 team2,
                                 team_ratings,
                                 venue = NULL,
                                 home_team = team1,
                                 context_factors = list()) {

  # Get team strength metrics
  team1_data <- team_ratings %>% dplyr::filter(team == team1)
  team2_data <- team_ratings %>% dplyr::filter(team == team2)

  if (nrow(team1_data) == 0 || nrow(team2_data) == 0) {
    stop("Team data not found for one or both teams")
  }

  # Basic strength comparison
  if ("rating_after" %in% names(team_ratings)) {
    team1_rating <- team1_data$rating_after[1]
    team2_rating <- team2_data$rating_after[1]
  } else if ("avg_team_rating" %in% names(team_ratings)) {
    team1_rating <- team1_data$avg_team_rating[1]
    team2_rating <- team2_data$avg_team_rating[1]
  } else {
    # Use arbitrary rating if not available
    team1_rating <- 1500
    team2_rating <- 1500
  }

  # Home advantage
  home_advantage <- 50  # Default home advantage in rating points
  if (home_team == team1) {
    effective_team1_rating <- team1_rating + home_advantage
    effective_team2_rating <- team2_rating
  } else if (home_team == team2) {
    effective_team1_rating <- team1_rating
    effective_team2_rating <- team2_rating + home_advantage
  } else {
    # Neutral venue
    effective_team1_rating <- team1_rating
    effective_team2_rating <- team2_rating
  }

  # Calculate win probabilities (Elo-style)
  team1_win_prob <- 1 / (1 + 10^((effective_team2_rating - effective_team1_rating) / 400))
  team2_win_prob <- 1 - team1_win_prob

  # Estimate margin (based on rating difference)
  rating_diff <- effective_team1_rating - effective_team2_rating
  expected_margin <- rating_diff * 0.1  # Scale factor

  # Estimate total score (based on average team scoring)
  if ("avg_score_for" %in% names(team_ratings)) {
    team1_avg_score <- team1_data$avg_score_for[1]
    team2_avg_score <- team2_data$avg_score_for[1]
    estimated_total <- team1_avg_score + team2_avg_score
  } else {
    estimated_total <- 100  # Default total if no scoring data
  }

  # Adjust for context factors
  confidence_adj <- 1.0
  if ("recent_form" %in% names(team_ratings)) {
    team1_form <- team1_data$recent_form[1]
    team2_form <- team2_data$recent_form[1]
    form_adj <- (team1_form - team2_form) * 20  # Adjust probabilities based on form
    team1_win_prob <- pmin(pmax(team1_win_prob + form_adj/100, 0.05), 0.95)
    team2_win_prob <- 1 - team1_win_prob
  }

  # Uncertainty based on team consistency
  if ("consistency" %in% names(team_ratings)) {
    avg_consistency <- mean(c(team1_data$consistency[1], team2_data$consistency[1]), na.rm = TRUE)
    confidence_adj <- pmax(0.5, 1 - (avg_consistency / 5))  # Lower confidence for inconsistent teams
  }

  prediction_result <- list(
    matchup = paste(team1, "vs", team2),
    venue = venue,
    home_team = home_team,
    team1_win_probability = round(team1_win_prob, 3),
    team2_win_probability = round(team2_win_prob, 3),
    expected_margin = round(expected_margin, 1),
    expected_total = round(estimated_total, 1),
    predicted_winner = ifelse(team1_win_prob > 0.5, team1, team2),
    confidence = round(confidence_adj, 2),
    team_ratings = list(
      team1 = list(name = team1, rating = team1_rating, effective_rating = effective_team1_rating),
      team2 = list(name = team2, rating = team2_rating, effective_rating = effective_team2_rating)
    )
  )

  return(prediction_result)
}