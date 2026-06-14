#' Optimize Decay Parameters for Player Statistics
#'
#' Optimizes exponential decay parameters (beta) for each statistic using DARKO-style weighting.
#' The function finds the optimal beta value that minimizes RMSE when predicting future performance
#' based on weighted historical data.
#'
#' @param player_data Data frame with player statistics including player_id, date, and the statistic
#' @param stats_to_optimize Character vector of statistic names to optimize
#' @param beta_range Numeric vector of length 2 specifying min and max beta values (default: c(0.98, 0.9999))
#' @param min_games Minimum number of games required per player (default: 5)
#' @param min_observations Minimum total observations needed for optimization (default: 50)
#'
#' @return Named list of optimal beta values for each statistic
#' @export
#'
#' @examples
#' \dontrun{
#' decay_params <- optimize_all_decay_parameters(
#'   player_data,
#'   stats_to_optimize = c("disposals", "goals", "marks")
#' )
#' }
optimize_all_decay_parameters <- function(player_data,
                                          stats_to_optimize,
                                          beta_range = c(0.98, 0.9999),
                                          min_games = 5,
                                          min_observations = 50) {

  # Helper: Calculate exponential decay average
  calculate_exp_decay_avg <- function(values, dates, current_date, beta) {
    past_games <- which(dates < current_date)
    if (length(past_games) == 0) return(NA)

    values_past <- values[past_games]
    dates_past <- dates[past_games]
    days_ago <- as.numeric(current_date - dates_past)
    weights <- beta^days_ago

    sum(values_past * weights) / sum(weights)
  }

  # Helper: Calculate RMSE for a given beta
  calculate_rmse_for_beta <- function(beta, modeling_data) {
    unique_dates <- sort(unique(modeling_data$date))

    all_predictions <- purrr::map_dfr(unique_dates, function(current_date) {
      current_games <- modeling_data %>%
        dplyr::filter(date == current_date, game_number >= 3)

      if (nrow(current_games) == 0) return(tibble::tibble())

      historical_games <- modeling_data %>%
        dplyr::filter(date < current_date)

      if (nrow(historical_games) == 0) return(tibble::tibble())

      current_games %>%
        dplyr::left_join(
          historical_games %>%
            dplyr::mutate(
              days_ago = as.numeric(current_date - date),
              weight = beta^days_ago,
              weighted_value = target_value * weight
            ) %>%
            dplyr::group_by(player_id) %>%
            dplyr::summarise(
              predicted_value = sum(weighted_value, na.rm = TRUE) / sum(weight, na.rm = TRUE),
              .groups = "drop"
            ),
          by = "player_id"
        ) %>%
        dplyr::filter(!is.na(predicted_value))
    })

    if (nrow(all_predictions) > 0) {
      return(sqrt(mean((all_predictions$target_value - all_predictions$predicted_value)^2, na.rm = TRUE)))
    } else {
      return(Inf)
    }
  }

  # Helper: Optimize single statistic
  optimize_single_stat <- function(player_data, stat_col) {
    cat(sprintf("Optimizing decay parameter for %s...\n", stat_col))

    modeling_data <- player_data %>%
      dplyr::filter(!is.na(.data[[stat_col]])) %>%
      dplyr::group_by(player_id) %>%
      dplyr::filter(dplyr::n() >= min_games) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        game_number = dplyr::row_number(),
        target_value = .data[[stat_col]]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(game_number >= 3) %>%
      dplyr::arrange(player_id, date)

    if (nrow(modeling_data) < min_observations) {
      cat(sprintf("Insufficient data for %s optimization (%d observations)\n",
                  stat_col, nrow(modeling_data)))
      return(0.99)
    }

    cat(sprintf("Optimizing on %d predictions...\n", nrow(modeling_data)))

    opt_result <- optimize(
      f = function(beta) calculate_rmse_for_beta(beta, modeling_data),
      interval = beta_range,
      tol = 0.0001
    )

    best_beta <- opt_result$minimum
    best_rmse <- opt_result$objective

    cat(sprintf("Best beta for %s: %.4f (RMSE: %.4f)\n", stat_col, best_beta, best_rmse))
    cat(sprintf("  Half-life: %.1f days\n", -log(2) / log(best_beta)))

    return(best_beta)
  }

  # Optimize all statistics
  decay_params <- list()
  cat(sprintf("\nOptimizing decay parameters for %d statistics...\n", length(stats_to_optimize)))

  for (stat in stats_to_optimize) {
    decay_params[[stat]] <- optimize_single_stat(player_data, stat)
  }

  return(decay_params)
}


#' Calculate Exponential Decay Features
#'
#' Calculates DARKO-style exponential decay weighted averages for player statistics.
#'
#' @param player_data Data frame with player statistics
#' @param stats_list Character vector of statistics to calculate decay features for
#' @param decay_params Named list of beta values for each statistic
#'
#' @return Data frame with exponential decay features added
#' @export
#'
#' @examples
#' \dontrun{
#' data_with_decay <- calculate_decay_features(
#'   player_data,
#'   stats_list = c("disposals", "goals"),
#'   decay_params = list(disposals = 0.995, goals = 0.992)
#' )
#' }
calculate_decay_features <- function(player_data, stats_list, decay_params) {

  # Vectorized function for exponential decay
  calculate_exp_decay_vectorized <- function(values, dates, beta) {
    n <- length(values)
    result <- rep(NA_real_, n)

    if (n < 2) return(result)

    for (i in 2:n) {
      prev_vals <- values[1:(i-1)]
      prev_dates <- dates[1:(i-1)]

      valid_idx <- !is.na(prev_vals)
      if (!any(valid_idx)) next

      prev_vals <- prev_vals[valid_idx]
      prev_dates <- prev_dates[valid_idx]

      days_ago <- as.numeric(dates[i] - prev_dates)
      weights <- beta^days_ago
      result[i] <- sum(prev_vals * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
    }

    return(result)
  }

  result_data <- player_data

  cat("Calculating exponential decay features...\n")

  for (i in seq_along(stats_list)) {
    stat <- stats_list[i]
    beta <- decay_params[[stat]]

    cat(sprintf("  [%d/%d] Processing %s (beta=%.4f)...\n",
                i, length(stats_list), stat, beta))

    result_data <- result_data %>%
      dplyr::arrange(player_id, date) %>%
      dplyr::group_by(player_id) %>%
      dplyr::mutate(
        !!paste0(stat, "_exp_avg") := calculate_exp_decay_vectorized(
          .data[[stat]],
          date,
          beta
        )
      ) %>%
      dplyr::ungroup()

    cat(sprintf("    Completed %s\n", stat))
  }

  return(result_data)
}


#' Train XGBoost Model for Player Statistic Prediction
#'
#' Trains an XGBoost model to predict a player statistic with cross-validation
#' to find optimal number of rounds.
#'
#' @param data Data frame with player features and target statistic
#' @param stat_name Name of the statistic to predict
#' @param feature_columns Character vector of feature column names
#' @param xgb_params List of XGBoost parameters
#' @param max_rounds Maximum number of boosting rounds for CV (default: 500)
#' @param early_stopping_rounds Early stopping rounds for CV (default: 20)
#' @param cv_folds Number of cross-validation folds (default: 5)
#' @param train_proportion Proportion of data for training (default: 0.8)
#'
#' @return List containing trained model and performance metrics
#' @export
#'
#' @examples
#' \dontrun{
#' model <- train_xgb_stat_model(
#'   data = player_data,
#'   stat_name = "disposals",
#'   feature_columns = c("disposals_exp_avg", "home_away_num"),
#'   xgb_params = list(eta = 0.05, max_depth = 6)
#' )
#' }
train_xgb_stat_model <- function(data,
                                  stat_name,
                                  feature_columns,
                                  xgb_params = list(
                                    max_depth = 6,
                                    eta = 0.05,
                                    subsample = 0.8,
                                    colsample_bytree = 0.8,
                                    objective = "reg:squarederror",
                                    eval_metric = "rmse"
                                  ),
                                  max_rounds = 500,
                                  early_stopping_rounds = 20,
                                  cv_folds = 5,
                                  train_proportion = 0.8) {

  cat(sprintf("Training model for %s...\n", stat_name))

  # Prepare data
  model_data <- data %>%
    dplyr::filter(!is.na(.data[[stat_name]])) %>%
    dplyr::select(dplyr::all_of(c(stat_name, feature_columns, "date"))) %>%
    tidyr::drop_na()

  if (nrow(model_data) < 100) {
    cat(sprintf("Insufficient data for %s model\n", stat_name))
    return(NULL)
  }

  # Time-based split
  sorted_dates <- sort(unique(model_data$date))
  split_idx <- floor(length(sorted_dates) * train_proportion)
  split_date <- sorted_dates[split_idx]
  train_data <- model_data %>% dplyr::filter(date <= split_date)
  test_data <- model_data %>% dplyr::filter(date > split_date)

  # Prepare matrices
  train_matrix <- as.matrix(train_data[, feature_columns])
  train_target <- train_data[[stat_name]]
  test_matrix <- as.matrix(test_data[, feature_columns])
  test_target <- test_data[[stat_name]]

  # Create DMatrix for XGBoost
  dtrain <- xgboost::xgb.DMatrix(data = train_matrix, label = train_target)

  # Cross-validation to find optimal rounds
  cv_result <- xgboost::xgb.cv(
    params = xgb_params,
    data = dtrain,
    nrounds = max_rounds,
    nfold = cv_folds,
    early_stopping_rounds = early_stopping_rounds,
    verbose = 0,
    print_every_n = 0
  )

  best_nrounds <- cv_result$best_iteration
  cv_rmse <- cv_result$evaluation_log$test_rmse_mean[best_nrounds]

  cat(sprintf("  CV: optimal rounds = %d, CV RMSE = %.3f\n", best_nrounds, cv_rmse))

  # Train final model
  xgb_model <- xgboost::xgboost(
    data = train_matrix,
    label = train_target,
    params = xgb_params,
    nrounds = best_nrounds,
    verbose = 0
  )

  # Calculate feature importance
  importance_matrix <- xgboost::xgb.importance(
    feature_names = feature_columns,
    model = xgb_model
  )

  # Get top 3 features
  top_3_features <- importance_matrix %>%
    dplyr::slice_head(n = 3) %>%
    dplyr::pull(Feature)

  top_3_importance <- importance_matrix %>%
    dplyr::slice_head(n = 3) %>%
    dplyr::pull(Gain)

  # Evaluate
  train_pred <- predict(xgb_model, train_matrix)
  test_pred <- predict(xgb_model, test_matrix)

  train_mae <- mean(abs(train_pred - train_target))
  test_mae <- mean(abs(test_pred - test_target))
  train_rmse <- sqrt(mean((train_pred - train_target)^2))
  test_rmse <- sqrt(mean((test_pred - test_target)^2))
  train_r2 <- stats::cor(train_pred, train_target)^2
  test_r2 <- stats::cor(test_pred, test_target)^2

  cat(sprintf("%s - Train RMSE: %.3f, Test RMSE: %.3f, Train R²: %.3f, Test R²: %.3f\n",
              stat_name, train_rmse, test_rmse, train_r2, test_r2))
  cat(sprintf("  Top features: %s\n", paste(top_3_features, collapse = ", ")))

  return(list(
    model = xgb_model,
    features = feature_columns,
    importance = importance_matrix,
    top_3_features = top_3_features,
    top_3_importance = top_3_importance,
    train_mae = train_mae,
    test_mae = test_mae,
    train_rmse = train_rmse,
    test_rmse = test_rmse,
    train_r2 = train_r2,
    test_r2 = test_r2,
    cv_rounds = best_nrounds,
    cv_rmse = cv_rmse
  ))
}


#' Calculate Position-Based First Game Averages
#'
#' Calculates average first-game performance for each position to use as baseline
#' predictions for players without historical data.
#'
#' @param player_data Data frame with player statistics and positions
#' @param stats_list Character vector of statistics to calculate averages for
#' @param position_col Name of the position column (default: "player_player_position")
#'
#' @return Data frame with position-based averages
#' @export
#'
#' @examples
#' \dontrun{
#' position_averages <- calculate_position_first_game_averages(
#'   player_data,
#'   stats_list = c("disposals", "goals", "marks")
#' )
#' }
calculate_position_first_game_averages <- function(player_data,
                                                    stats_list,
                                                    position_col = "player_player_position") {

  if (!position_col %in% names(player_data)) {
    warning(sprintf("Position column '%s' not found in data", position_col))
    return(NULL)
  }

  # Filter to first games only
  first_games <- player_data %>%
    dplyr::arrange(player_id, date) %>%
    dplyr::group_by(player_id) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  # Calculate averages by position
  position_averages <- first_games %>%
    dplyr::filter(!is.na(.data[[position_col]])) %>%
    dplyr::group_by(!!rlang::sym(position_col)) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(stats_list),
        ~ mean(.x, na.rm = TRUE),
        .names = "{.col}_avg"
      ),
      n_players = dplyr::n(),
      .groups = "drop"
    )

  cat("\nPosition-based first game averages calculated:\n")
  print(position_averages)

  return(position_averages)
}


#' Generate Player Performance Predictions (Hybrid)
#'
#' Generates predictions for player statistics using trained XGBoost models for
#' experienced players, and position-based averages for first-game players.
#'
#' @param current_data Data frame with current player features
#' @param models Named list of trained models (from train_xgb_stat_model)
#' @param position_averages Data frame with position-based averages (optional)
#' @param include_player_info Include player_id, player_name, team, date in output (default: TRUE)
#'
#' @return Data frame with predictions for each statistic
#' @export
#'
#' @examples
#' \dontrun{
#' predictions <- generate_player_predictions(
#'   current_data = latest_player_data,
#'   models = stat_models,
#'   position_averages = pos_avgs
#' )
#' }
generate_player_predictions <- function(current_data,
                                        models,
                                        position_averages = NULL,
                                        include_player_info = TRUE) {

  if (include_player_info) {
    id_cols <- intersect(c("player_id", "player_name", "team", "date", "player_player_position"),
                         names(current_data))
    predictions <- current_data %>%
      dplyr::select(dplyr::all_of(id_cols))
  } else {
    predictions <- tibble::tibble(.rows = nrow(current_data))
  }

  # Get all unique features needed
  all_features <- unique(unlist(lapply(models, function(m) m$features)))

  # Identify which rows have complete features (experienced players)
  feature_check <- current_data %>%
    dplyr::select(dplyr::all_of(all_features))

  has_features <- complete.cases(feature_check)

  cat(sprintf("Predictions: %d with XGBoost, %d need position averages\n",
              sum(has_features), sum(!has_features)))

  # Generate XGBoost predictions for experienced players
  if (sum(has_features) > 0) {
    pred_data <- current_data[has_features, ] %>%
      dplyr::select(dplyr::all_of(all_features))

    for (stat in names(models)) {
      if (!is.null(models[[stat]])) {
        pred_matrix <- as.matrix(pred_data[, models[[stat]]$features])
        predictions[[paste0(stat, "_predicted")]][has_features] <- predict(models[[stat]]$model, pred_matrix)
      }
    }
  }

  # Use position averages for first-game players
  if (sum(!has_features) > 0 && !is.null(position_averages) &&
      "player_player_position" %in% names(current_data)) {

    first_game_data <- current_data[!has_features, ]

    for (i in seq_len(nrow(first_game_data))) {
      player_pos <- first_game_data$player_player_position[i]

      if (!is.na(player_pos)) {
        # Find matching position in averages
        pos_match <- position_averages %>%
          dplyr::filter(player_player_position == player_pos)

        if (nrow(pos_match) > 0) {
          # Apply position averages for each stat
          for (stat in names(models)) {
            avg_col <- paste0(stat, "_avg")
            pred_col <- paste0(stat, "_predicted")

            if (avg_col %in% names(pos_match)) {
              idx <- which(!has_features)[i]
              predictions[[pred_col]][idx] <- pos_match[[avg_col]][1]
            }
          }
        }
      }
    }
  } else if (sum(!has_features) > 0) {
    cat("Warning: Position averages not provided - first-game predictions will be NA\n")
  }

  # Add prediction method indicator
  predictions$prediction_method <- ifelse(has_features, "xgboost", "position_avg")

  return(predictions)
}
