#' Build Comprehensive Match Prediction Model
#'
#' Build a model that combines team and player metrics to predict match outcomes.
#'
#' @param match_data Data frame with match-level features
#' @param target_vars Vector of target variables to predict (margin, total_score, etc.)
#' @param model_type Type of model ("lm", "glmnet", "ensemble")
#' @param include_player_features Whether to include aggregated player features
#' @param validation_method Validation approach ("temporal", "random", "cv")
#' @return List containing trained models for each target variable
#' @export
#' @examples
#' \dontrun{
#' match_models <- build_match_prediction_model(
#'   match_features,
#'   target_vars = c("margin", "total_score"),
#'   model_type = "ensemble"
#' )
#' }
build_match_prediction_model <- function(match_data,
                                         target_vars = c("margin", "total_score"),
                                         model_type = "lm",
                                         include_player_features = TRUE,
                                         validation_method = "temporal") {

  # Define predictor variables
  team_predictors <- c(
    "home_rating_before", "away_rating_before", "rating_diff",
    "home_recent_form", "away_recent_form", "form_diff",
    "h2h_win_pct_home", "avg_h2h_margin_home", "h2h_games",
    "venue_advantage", "home_win_pct_venue"
  )

  player_predictors <- c(
    "home_avg_team_rating", "away_avg_team_rating", "team_rating_diff",
    "home_star_power", "away_star_power", "star_power_diff",
    "home_depth_score", "away_depth_score", "depth_diff",
    "home_players_above_7", "away_players_above_7"
  )

  context_predictors <- c(
    "round_number", "season_pct", "weekend_game", "familiar_venue"
  )

  # Select available predictors
  all_predictors <- c(team_predictors, context_predictors)
  if (include_player_features) {
    all_predictors <- c(all_predictors, player_predictors)
  }

  available_predictors <- all_predictors[all_predictors %in% names(match_data)]

  if (length(available_predictors) < 5) {
    warning("Insufficient predictor variables available for match modeling")
    return(NULL)
  }

  # Prepare data for modeling
  modeling_data <- match_data %>%
    dplyr::select(dplyr::all_of(c(target_vars, available_predictors, "date", "season"))) %>%
    tidyr::drop_na()

  if (nrow(modeling_data) < 50) {
    warning("Insufficient data for match prediction modeling")
    return(NULL)
  }

  # Split data based on validation method
  if (validation_method == "temporal") {
    # Use earlier data for training, later for testing
    split_date <- quantile(modeling_data$date, 0.8, na.rm = TRUE)
    train_data <- modeling_data %>% dplyr::filter(date <= split_date)
    test_data <- modeling_data %>% dplyr::filter(date > split_date)
  } else if (validation_method == "random") {
    set.seed(42)
    train_indices <- sample(nrow(modeling_data), round(nrow(modeling_data) * 0.8))
    train_data <- modeling_data[train_indices, ]
    test_data <- modeling_data[-train_indices, ]
  }

  # Build models for each target variable
  models <- list()

  for (target in target_vars) {
    if (!target %in% names(modeling_data)) {
      warning(paste("Target variable", target, "not found in data"))
      next
    }

    cat("Building model for", target, "...\n")

    # Create formula
    formula_str <- paste(target, "~", paste(available_predictors, collapse = " + "))
    model_formula <- stats::as.formula(formula_str)

    if (model_type == "lm") {
      # Linear regression
      model <- stats::lm(model_formula, data = train_data)
      predictions <- stats::predict(model, newdata = test_data)

    } else if (model_type == "glmnet" && requireNamespace("glmnet", quietly = TRUE)) {
      # Elastic net regression
      train_matrix <- stats::model.matrix(model_formula, data = train_data)[, -1]
      test_matrix <- stats::model.matrix(model_formula, data = test_data)[, -1]
      train_y <- train_data[[target]]

      cv_model <- glmnet::cv.glmnet(train_matrix, train_y, alpha = 0.5)
      model <- glmnet::glmnet(train_matrix, train_y, alpha = 0.5, lambda = cv_model$lambda.min)
      predictions <- as.numeric(stats::predict(model, newx = test_matrix, s = cv_model$lambda.min))

    } else if (model_type == "ensemble") {
      # Ensemble of multiple models
      # Linear model
      lm_model <- stats::lm(model_formula, data = train_data)
      lm_pred <- stats::predict(lm_model, newdata = test_data)

      # Random forest (if available)
      if (requireNamespace("randomForest", quietly = TRUE)) {
        rf_model <- randomForest::randomForest(model_formula, data = train_data, ntree = 300)
        rf_pred <- stats::predict(rf_model, newdata = test_data)

        # Simple ensemble (average)
        predictions <- (lm_pred + rf_pred) / 2
        model <- list(lm = lm_model, rf = rf_model, type = "ensemble")
      } else {
        predictions <- lm_pred
        model <- lm_model
      }
    }

    # Calculate performance metrics
    actual <- test_data[[target]]
    mae <- mean(abs(predictions - actual), na.rm = TRUE)
    rmse <- sqrt(mean((predictions - actual)^2, na.rm = TRUE))
    r_squared <- stats::cor(predictions, actual, use = "complete.obs")^2

    # Feature importance
    feature_importance <- NULL
    if (model_type == "lm" || (model_type == "ensemble" && !is.list(model))) {
      base_model <- if (is.list(model)) model$lm else model
      coef_summary <- broom::tidy(base_model)
      feature_importance <- coef_summary %>%
        dplyr::filter(term != "(Intercept)") %>%
        dplyr::mutate(importance = abs(estimate)) %>%
        dplyr::select(variable = term, coefficient = estimate, importance, p_value = p.value) %>%
        dplyr::arrange(dplyr::desc(importance))
    }

    models[[target]] <- list(
      model = model,
      target_var = target,
      predictor_vars = available_predictors,
      performance = list(
        mae = mae,
        rmse = rmse,
        r_squared = r_squared,
        n_train = nrow(train_data),
        n_test = nrow(test_data)
      ),
      feature_importance = feature_importance,
      predictions = data.frame(
        actual = actual,
        predicted = predictions,
        residual = actual - predictions
      )
    )
  }

  # Return combined model object
  result <- list(
    models = models,
    model_type = model_type,
    target_vars = target_vars,
    predictor_vars = available_predictors,
    validation_method = validation_method,
    train_data = train_data,
    test_data = test_data
  )

  return(result)
}

#' Predict Match Outcome
#'
#' Use trained models to predict comprehensive match outcomes.
#'
#' @param model_object Trained match prediction model
#' @param home_team Name of home team
#' @param away_team Name of away team
#' @param match_features Data frame with match features for prediction
#' @param confidence_level Confidence level for prediction intervals
#' @return List with comprehensive match predictions
#' @export
#' @examples
#' \dontrun{
#' prediction <- predict_match_outcome(
#'   match_models,
#'   "Adelaide", "Carlton",
#'   match_features
#' )
#' }
predict_match_outcome <- function(model_object,
                                  home_team,
                                  away_team,
                                  match_features,
                                  confidence_level = 0.9) {

  if (is.null(model_object) || is.null(model_object$models)) {
    stop("Invalid model object provided")
  }

  # Check that required features are present
  required_vars <- model_object$predictor_vars
  missing_vars <- required_vars[!required_vars %in% names(match_features)]
  if (length(missing_vars) > 0) {
    stop(paste("Missing required features:", paste(missing_vars, collapse = ", ")))
  }

  # Prepare prediction data
  pred_data <- match_features %>%
    dplyr::select(dplyr::all_of(required_vars)) %>%
    tidyr::drop_na()

  if (nrow(pred_data) == 0) {
    stop("No complete cases available for prediction")
  }

  predictions <- list()

  # Make predictions for each target variable
  for (target in names(model_object$models)) {
    model_info <- model_object$models[[target]]
    model <- model_info$model

    if (model_object$model_type == "lm") {
      pred_result <- stats::predict(
        model,
        newdata = pred_data,
        interval = "prediction",
        level = confidence_level
      )
      predictions[[target]] <- list(
        predicted = as.numeric(pred_result[, "fit"]),
        lower_ci = as.numeric(pred_result[, "lwr"]),
        upper_ci = as.numeric(pred_result[, "upr"])
      )

    } else if (model_object$model_type == "glmnet") {
      pred_matrix <- stats::model.matrix(
        stats::as.formula(paste("~", paste(required_vars, collapse = " + "))),
        data = pred_data
      )[, -1]

      predicted_values <- as.numeric(
        stats::predict(model, newx = pred_matrix, s = "lambda.min")
      )

      # Approximate confidence intervals
      residual_se <- sd(model_info$predictions$residual, na.rm = TRUE)
      alpha <- 1 - confidence_level
      z_value <- stats::qnorm(1 - alpha/2)

      predictions[[target]] <- list(
        predicted = predicted_values,
        lower_ci = predicted_values - z_value * residual_se,
        upper_ci = predicted_values + z_value * residual_se
      )

    } else if (model_object$model_type == "ensemble" && is.list(model)) {
      # Ensemble prediction
      lm_pred <- stats::predict(model$lm, newdata = pred_data)
      rf_pred <- stats::predict(model$rf, newdata = pred_data)
      ensemble_pred <- (lm_pred + rf_pred) / 2

      residual_se <- sd(model_info$predictions$residual, na.rm = TRUE)
      alpha <- 1 - confidence_level
      z_value <- stats::qnorm(1 - alpha/2)

      predictions[[target]] <- list(
        predicted = ensemble_pred,
        lower_ci = ensemble_pred - z_value * residual_se,
        upper_ci = ensemble_pred + z_value * residual_se
      )
    }
  }

  # Calculate derived predictions
  margin_pred <- if ("margin" %in% names(predictions)) predictions$margin$predicted[1] else 0
  total_pred <- if ("total_score" %in% names(predictions)) predictions$total_score$predicted[1] else 100

  # Win probabilities based on margin
  if ("margin" %in% names(predictions)) {
    margin_se <- (predictions$margin$upper_ci[1] - predictions$margin$lower_ci[1]) / (2 * 1.96)
    home_win_prob <- stats::pnorm(0, mean = -margin_pred, sd = margin_se, lower.tail = FALSE)
    away_win_prob <- 1 - home_win_prob
  } else {
    # Default probabilities if no margin model
    home_win_prob <- 0.5
    away_win_prob <- 0.5
  }

  # Predicted scores
  if ("margin" %in% names(predictions) && "total_score" %in% names(predictions)) {
    home_score <- (total_pred + margin_pred) / 2
    away_score <- (total_pred - margin_pred) / 2
  } else {
    home_score <- total_pred / 2
    away_score <- total_pred / 2
  }

  # Compile comprehensive prediction
  result <- list(
    matchup = paste(home_team, "vs", away_team),
    predictions = list(
      margin = list(
        value = round(margin_pred, 1),
        confidence_interval = if ("margin" %in% names(predictions)) {
          c(round(predictions$margin$lower_ci[1], 1),
            round(predictions$margin$upper_ci[1], 1))
        } else NULL
      ),
      total_score = list(
        value = round(total_pred, 1),
        confidence_interval = if ("total_score" %in% names(predictions)) {
          c(round(predictions$total_score$lower_ci[1], 1),
            round(predictions$total_score$upper_ci[1], 1))
        } else NULL
      ),
      home_score = round(home_score, 1),
      away_score = round(away_score, 1),
      home_win_probability = round(home_win_prob, 3),
      away_win_probability = round(away_win_prob, 3),
      predicted_winner = ifelse(home_win_prob > 0.5, home_team, away_team)
    ),
    confidence_level = confidence_level,
    model_performance = lapply(model_object$models, function(x) x$performance)
  )

  return(result)
}

#' Evaluate Match Prediction Performance
#'
#' Evaluate the performance of match prediction models against actual results.
#'
#' @param model_object Trained match prediction model
#' @param evaluation_data Data frame with actual match results and features
#' @return List with detailed performance evaluation
#' @export
#' @examples
#' \dontrun{
#' evaluation <- evaluate_match_predictions(match_models, test_matches)
#' }
evaluate_match_predictions <- function(model_object, evaluation_data) {

  if (is.null(model_object) || is.null(model_object$models)) {
    stop("Invalid model object provided")
  }

  results <- list()

  for (target in names(model_object$models)) {
    if (!target %in% names(evaluation_data)) {
      warning(paste("Target variable", target, "not found in evaluation data"))
      next
    }

    model_info <- model_object$models[[target]]

    # Make predictions
    pred_data <- evaluation_data %>%
      dplyr::select(dplyr::all_of(model_info$predictor_vars)) %>%
      tidyr::drop_na()

    if (nrow(pred_data) == 0) {
      warning(paste("No complete cases for evaluating", target))
      next
    }

    # Get predictions based on model type
    if (model_object$model_type == "lm") {
      predictions <- stats::predict(model_info$model, newdata = pred_data)
    } else if (model_object$model_type == "glmnet") {
      pred_matrix <- stats::model.matrix(
        stats::as.formula(paste("~", paste(model_info$predictor_vars, collapse = " + "))),
        data = pred_data
      )[, -1]
      predictions <- as.numeric(
        stats::predict(model_info$model, newx = pred_matrix, s = "lambda.min")
      )
    } else if (model_object$model_type == "ensemble") {
      if (is.list(model_info$model)) {
        lm_pred <- stats::predict(model_info$model$lm, newdata = pred_data)
        rf_pred <- stats::predict(model_info$model$rf, newdata = pred_data)
        predictions <- (lm_pred + rf_pred) / 2
      } else {
        predictions <- stats::predict(model_info$model, newdata = pred_data)
      }
    }

    # Calculate performance metrics
    actual <- evaluation_data[[target]][1:length(predictions)]
    residuals <- actual - predictions

    mae <- mean(abs(residuals), na.rm = TRUE)
    rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
    mape <- mean(abs(residuals / actual) * 100, na.rm = TRUE)
    r_squared <- stats::cor(actual, predictions, use = "complete.obs")^2

    # Directional accuracy (for margin predictions)
    if (target == "margin") {
      correct_direction <- sum((actual > 0 & predictions > 0) | (actual < 0 & predictions < 0))
      directional_accuracy <- correct_direction / length(actual)
    } else {
      directional_accuracy <- NA
    }

    results[[target]] <- list(
      n_predictions = length(predictions),
      mae = mae,
      rmse = rmse,
      mape = mape,
      r_squared = r_squared,
      directional_accuracy = directional_accuracy,
      predictions_vs_actual = data.frame(
        actual = actual,
        predicted = predictions,
        residual = residuals
      )
    )
  }

  # Overall model summary
  overall_summary <- list(
    model_type = model_object$model_type,
    targets_evaluated = names(results),
    avg_r_squared = mean(sapply(results, function(x) x$r_squared), na.rm = TRUE),
    avg_mae = mean(sapply(results, function(x) x$mae), na.rm = TRUE)
  )

  evaluation_result <- list(
    target_results = results,
    overall_summary = overall_summary,
    model_info = model_object
  )

  return(evaluation_result)
}

#' Generate Match Prediction Report
#'
#' Generate a comprehensive report for match predictions.
#'
#' @param prediction_result Result from predict_match_outcome
#' @param include_confidence Whether to include confidence intervals
#' @return Character vector with formatted report
#' @export
#' @examples
#' \dontrun{
#' report <- generate_prediction_report(match_prediction)
#' cat(report, sep = "\n")
#' }
generate_prediction_report <- function(prediction_result, include_confidence = TRUE) {

  report <- character()
  pred <- prediction_result$predictions

  # Header
  report <- c(report, paste("=== MATCH PREDICTION:", prediction_result$matchup, "==="))
  report <- c(report, paste("Generated:", Sys.time()))
  report <- c(report, "")

  # Main predictions
  report <- c(report, "PREDICTED OUTCOME:")
  report <- c(report, paste("  Winner:", pred$predicted_winner))
  report <- c(report, paste("  Home Team Win Probability:", scales::percent(pred$home_win_probability)))
  report <- c(report, paste("  Away Team Win Probability:", scales::percent(pred$away_win_probability)))
  report <- c(report, "")

  # Score predictions
  report <- c(report, "SCORE PREDICTIONS:")
  report <- c(report, paste("  Home Team Score:", pred$home_score))
  report <- c(report, paste("  Away Team Score:", pred$away_score))
  report <- c(report, paste("  Predicted Margin:", pred$margin$value, "(home team perspective)"))
  report <- c(report, paste("  Predicted Total:", pred$total_score$value))
  report <- c(report, "")

  # Confidence intervals
  if (include_confidence && !is.null(pred$margin$confidence_interval)) {
    report <- c(report, paste("CONFIDENCE INTERVALS (", scales::percent(prediction_result$confidence_level), "):", sep = ""))
    report <- c(report, paste("  Margin:", pred$margin$confidence_interval[1], "to", pred$margin$confidence_interval[2]))
    if (!is.null(pred$total_score$confidence_interval)) {
      report <- c(report, paste("  Total Score:", pred$total_score$confidence_interval[1], "to", pred$total_score$confidence_interval[2]))
    }
    report <- c(report, "")
  }

  # Model performance context
  if (!is.null(prediction_result$model_performance)) {
    report <- c(report, "MODEL PERFORMANCE CONTEXT:")
    for (target in names(prediction_result$model_performance)) {
      perf <- prediction_result$model_performance[[target]]
      report <- c(report, paste("  ", tools::toTitleCase(target), "Model R\\u00b2:", round(perf$r_squared, 3)))
      report <- c(report, paste("  ", tools::toTitleCase(target), "Model MAE:", round(perf$mae, 2)))
    }
  }

  return(report)
}