#' Build Player Performance Prediction Model
#'
#' Build a model to predict individual player performance in upcoming games.
#'
#' @param player_data Data frame with player statistics and features
#' @param target_var Target variable to predict (default: "overall_rating")
#' @param predictor_vars Vector of predictor variables (if NULL, auto-select)
#' @param model_type Type of model ("lm", "glmnet", "rf")
#' @param train_prop Proportion of data for training (default: 0.8)
#' @param validation_method Validation method ("holdout" or "cv")
#' @return List containing trained model and performance metrics
#' @export
#' @examples
#' \dontrun{
#' performance_model <- build_player_performance_model(
#'   player_data,
#'   target_var = "overall_rating",
#'   model_type = "glmnet"
#' )
#' }
build_player_performance_model <- function(player_data,
                                           target_var = "overall_rating",
                                           predictor_vars = NULL,
                                           model_type = "lm",
                                           train_prop = 0.8,
                                           validation_method = "holdout") {

  # Prepare data
  model_data <- player_data %>%
    dplyr::filter(!is.na(!!rlang::sym(target_var)))

  # Auto-select predictors if not specified
  if (is.null(predictor_vars)) {
    predictor_vars <- c(
      "recent_rating", "recent_goals", "recent_disposals", "recent_efficiency",
      "rating_consistency", "possession_rating", "scoring_rating",
      "defensive_rating", "home_away", "team_result", "venue_advantage",
      "season_pct", "games_played"
    )
    # Keep only existing columns
    predictor_vars <- predictor_vars[predictor_vars %in% names(model_data)]
  }

  # Check if we have enough predictors
  if (length(predictor_vars) < 3) {
    warning("Insufficient predictor variables available")
    return(NULL)
  }

  # Prepare modeling dataset
  modeling_data <- model_data %>%
    dplyr::select(dplyr::all_of(c(target_var, predictor_vars, "player_id", "date"))) %>%
    tidyr::drop_na()

  if (nrow(modeling_data) < 50) {
    warning("Insufficient data for modeling (< 50 complete cases)")
    return(NULL)
  }

  # Split data
  if (validation_method == "holdout") {
    # Time-based split (more realistic for time series)
    sorted_dates <- sort(unique(modeling_data$date))
    split_idx <- floor(length(sorted_dates) * train_prop)
    split_date <- sorted_dates[split_idx]
    train_data <- modeling_data %>% dplyr::filter(date <= split_date)
    test_data <- modeling_data %>% dplyr::filter(date > split_date)
  } else {
    # Random split
    set.seed(42)
    train_indices <- sample(nrow(modeling_data), round(nrow(modeling_data) * train_prop))
    train_data <- modeling_data[train_indices, ]
    test_data <- modeling_data[-train_indices, ]
  }

  # Prepare formula
  formula_str <- paste(target_var, "~", paste(predictor_vars, collapse = " + "))
  model_formula <- stats::as.formula(formula_str)

  # Fit model
  if (model_type == "lm") {
    model <- stats::lm(model_formula, data = train_data)
    predictions <- stats::predict(model, newdata = test_data)

  } else if (model_type == "glmnet") {
    # Prepare matrices for glmnet
    train_matrix <- stats::model.matrix(model_formula, data = train_data)[, -1]  # Remove intercept
    test_matrix <- stats::model.matrix(model_formula, data = test_data)[, -1]
    train_y <- train_data[[target_var]]

    # Fit elastic net with cross-validation
    if (requireNamespace("glmnet", quietly = TRUE)) {
      cv_model <- glmnet::cv.glmnet(train_matrix, train_y, alpha = 0.5)
      model <- glmnet::glmnet(train_matrix, train_y, alpha = 0.5, lambda = cv_model$lambda.min)
      predictions <- as.numeric(stats::predict(model, newx = test_matrix, s = cv_model$lambda.min))
    } else {
      warning("glmnet package not available, falling back to lm")
      model <- stats::lm(model_formula, data = train_data)
      predictions <- stats::predict(model, newdata = test_data)
    }

  } else if (model_type == "rf") {
    if (requireNamespace("randomForest", quietly = TRUE)) {
      model <- randomForest::randomForest(model_formula, data = train_data, ntree = 500)
      predictions <- stats::predict(model, newdata = test_data)
    } else {
      warning("randomForest package not available, falling back to lm")
      model <- stats::lm(model_formula, data = train_data)
      predictions <- stats::predict(model, newdata = test_data)
    }
  }

  # Calculate performance metrics
  actual <- test_data[[target_var]]
  mae <- mean(abs(predictions - actual), na.rm = TRUE)
  rmse <- sqrt(mean((predictions - actual)^2, na.rm = TRUE))
  r_squared <- stats::cor(predictions, actual, use = "complete.obs")^2

  # Feature importance (for applicable models)
  feature_importance <- NULL
  if (model_type == "rf" && exists("model") && inherits(model, "randomForest")) {
    feature_importance <- data.frame(
      variable = names(model$importance),
      importance = as.numeric(model$importance),
      stringsAsFactors = FALSE
    ) %>%
      dplyr::arrange(dplyr::desc(importance))
  } else if (model_type == "lm") {
    # Use coefficient magnitudes as proxy for importance
    coef_summary <- broom::tidy(model)
    feature_importance <- coef_summary %>%
      dplyr::filter(term != "(Intercept)") %>%
      dplyr::mutate(importance = abs(estimate)) %>%
      dplyr::select(variable = term, importance) %>%
      dplyr::arrange(dplyr::desc(importance))
  }

  # Return model object and metrics
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
      actual = actual,
      predicted = predictions,
      residual = actual - predictions
    ),
    train_data = train_data,
    test_data = test_data
  )

  return(result)
}

#' Model Player Impact on Team Margin
#'
#' Build a model to quantify how individual player performances impact team margins.
#'
#' @param player_data Data frame with player statistics and match results
#' @param aggregation_level How to aggregate player data ("game" or "team_game")
#' @param model_type Type of model to use
#' @return List containing margin impact model and analysis
#' @export
#' @examples
#' \dontrun{
#' margin_model <- model_player_margin_impact(player_data)
#' }
model_player_margin_impact <- function(player_data,
                                       aggregation_level = "team_game",
                                       model_type = "lm") {

  if (aggregation_level == "team_game") {
    # Aggregate player stats by team per game
    team_game_data <- player_data %>%
      dplyr::group_by(match_id, team, home_away) %>%
      dplyr::summarise(
        margin = dplyr::first(team_margin),
        total_disposals = sum(disposals, na.rm = TRUE),
        total_goals = sum(goals, na.rm = TRUE),
        total_marks = sum(marks, na.rm = TRUE),
        total_tackles = sum(tackles, na.rm = TRUE),
        avg_efficiency = mean(disposal_efficiency, na.rm = TRUE),
        avg_rating = mean(overall_rating, na.rm = TRUE),
        top_player_rating = max(overall_rating, na.rm = TRUE),
        worst_player_rating = min(overall_rating, na.rm = TRUE),
        rating_range = top_player_rating - worst_player_rating,
        players_above_7 = sum(overall_rating >= 7, na.rm = TRUE),
        players_below_4 = sum(overall_rating <= 4, na.rm = TRUE),
        .groups = "drop"
      )

    modeling_data <- team_game_data
    target_var <- "margin"

  } else {
    # Individual player level
    modeling_data <- player_data %>%
      dplyr::filter(!is.na(signed_margin_contribution))
    target_var <- "signed_margin_contribution"
  }

  # Define predictors
  predictor_vars <- c(
    "total_disposals", "total_goals", "total_marks", "total_tackles",
    "avg_efficiency", "avg_rating", "rating_range", "players_above_7"
  )
  predictor_vars <- predictor_vars[predictor_vars %in% names(modeling_data)]

  if (length(predictor_vars) < 3) {
    warning("Insufficient predictor variables for margin modeling")
    return(NULL)
  }

  # Build model
  formula_str <- paste(target_var, "~", paste(predictor_vars, collapse = " + "))
  model_formula <- stats::as.formula(formula_str)

  # Fit model with cross-validation
  model_data <- modeling_data %>%
    dplyr::select(dplyr::all_of(c(target_var, predictor_vars))) %>%
    tidyr::drop_na()

  if (nrow(model_data) < 30) {
    warning("Insufficient data for margin modeling")
    return(NULL)
  }

  if (model_type == "lm") {
    model <- stats::lm(model_formula, data = model_data)
    model_summary <- broom::tidy(model)
    model_performance <- broom::glance(model)

  } else if (model_type == "glmnet" && requireNamespace("glmnet", quietly = TRUE)) {
    model_matrix <- stats::model.matrix(model_formula, data = model_data)[, -1]
    target_values <- model_data[[target_var]]

    cv_model <- glmnet::cv.glmnet(model_matrix, target_values, alpha = 0.5)
    model <- glmnet::glmnet(model_matrix, target_values, alpha = 0.5, lambda = cv_model$lambda.min)

    # Extract coefficients
    coef_matrix <- stats::coef(model, s = cv_model$lambda.min)
    model_summary <- data.frame(
      term = rownames(coef_matrix),
      estimate = as.numeric(coef_matrix),
      stringsAsFactors = FALSE
    ) %>%
      dplyr::filter(estimate != 0)

    model_performance <- data.frame(
      lambda = cv_model$lambda.min,
      cvm = min(cv_model$cvm)
    )
  }

  # Calculate feature importance
  if (model_type == "lm") {
    feature_importance <- model_summary %>%
      dplyr::filter(term != "(Intercept)") %>%
      dplyr::mutate(
        abs_estimate = abs(estimate),
        importance_rank = dplyr::row_number(dplyr::desc(abs_estimate))
      ) %>%
      dplyr::arrange(importance_rank) %>%
      dplyr::select(variable = term, coefficient = estimate, importance = abs_estimate,
                   p_value = p.value, rank = importance_rank)
  } else {
    feature_importance <- model_summary %>%
      dplyr::filter(term != "(Intercept)") %>%
      dplyr::mutate(
        abs_estimate = abs(estimate),
        importance_rank = dplyr::row_number(dplyr::desc(abs_estimate))
      ) %>%
      dplyr::arrange(importance_rank) %>%
      dplyr::select(variable = term, coefficient = estimate, importance = abs_estimate,
                   rank = importance_rank)
  }

  result <- list(
    model = model,
    model_type = model_type,
    aggregation_level = aggregation_level,
    target_var = target_var,
    feature_importance = feature_importance,
    model_summary = model_summary,
    model_performance = model_performance,
    data = modeling_data
  )

  return(result)
}

#' Predict Player Performance
#'
#' Use trained model to predict player performance for future games.
#'
#' @param model_object Trained model object from build_player_performance_model
#' @param new_data Data frame with new data for prediction
#' @param confidence_level Confidence level for prediction intervals
#' @return Data frame with predictions and confidence intervals
#' @export
#' @examples
#' \dontrun{
#' predictions <- predict_player_performance(performance_model, new_player_data)
#' }
predict_player_performance <- function(model_object,
                                       new_data,
                                       confidence_level = 0.95) {

  if (is.null(model_object) || is.null(model_object$model)) {
    stop("Invalid model object provided")
  }

  # Check that required predictors are present
  required_vars <- model_object$predictor_vars
  missing_vars <- required_vars[!required_vars %in% names(new_data)]
  if (length(missing_vars) > 0) {
    stop(paste("Missing required variables:", paste(missing_vars, collapse = ", ")))
  }

  # Prepare prediction data
  pred_data <- new_data %>%
    dplyr::select(dplyr::all_of(required_vars)) %>%
    tidyr::drop_na()

  if (nrow(pred_data) == 0) {
    warning("No complete cases available for prediction")
    return(data.frame())
  }

  # Make predictions based on model type
  if (model_object$model_type == "lm") {
    # Linear model with prediction intervals
    pred_result <- stats::predict(
      model_object$model,
      newdata = pred_data,
      interval = "prediction",
      level = confidence_level
    )

    predictions <- data.frame(
      predicted = pred_result[, "fit"],
      lower_ci = pred_result[, "lwr"],
      upper_ci = pred_result[, "upr"]
    )

  } else if (model_object$model_type == "glmnet") {
    # Elastic net predictions (no built-in prediction intervals)
    pred_matrix <- stats::model.matrix(
      stats::as.formula(paste("~", paste(required_vars, collapse = " + "))),
      data = pred_data
    )[, -1]

    predicted_values <- as.numeric(
      stats::predict(model_object$model, newx = pred_matrix, s = "lambda.min")
    )

    # Approximate prediction intervals using residual standard error
    residual_se <- sd(model_object$predictions$residual, na.rm = TRUE)
    alpha <- 1 - confidence_level
    t_value <- stats::qt(1 - alpha/2, df = nrow(model_object$predictions) - length(required_vars))

    predictions <- data.frame(
      predicted = predicted_values,
      lower_ci = predicted_values - t_value * residual_se,
      upper_ci = predicted_values + t_value * residual_se
    )

  } else if (model_object$model_type == "rf") {
    # Random forest predictions
    predicted_values <- stats::predict(model_object$model, newdata = pred_data)

    # Use quantile regression for prediction intervals (approximate)
    residual_se <- sd(model_object$predictions$residual, na.rm = TRUE)
    alpha <- 1 - confidence_level

    predictions <- data.frame(
      predicted = predicted_values,
      lower_ci = predicted_values - stats::qnorm(1 - alpha/2) * residual_se,
      upper_ci = predicted_values + stats::qnorm(1 - alpha/2) * residual_se
    )
  }

  # Add input data back to predictions
  result <- dplyr::bind_cols(new_data[1:nrow(predictions), ], predictions)

  return(result)
}

#' Evaluate Model Performance
#'
#' Comprehensive evaluation of player performance models.
#'
#' @param model_object Trained model object
#' @param plot_diagnostics Whether to generate diagnostic plots
#' @return List with detailed performance evaluation
#' @export
#' @examples
#' \dontrun{
#' evaluation <- evaluate_model_performance(performance_model)
#' }
evaluate_model_performance <- function(model_object, plot_diagnostics = FALSE) {

  if (is.null(model_object) || is.null(model_object$predictions)) {
    stop("Invalid model object or missing predictions")
  }

  pred_data <- model_object$predictions
  actual <- pred_data$actual
  predicted <- pred_data$predicted
  residuals <- pred_data$residual

  # Calculate metrics
  n <- length(actual)
  mae <- mean(abs(residuals), na.rm = TRUE)
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  mape <- mean(abs(residuals / actual) * 100, na.rm = TRUE)
  r_squared <- stats::cor(actual, predicted, use = "complete.obs")^2

  # Additional metrics
  mean_actual <- mean(actual, na.rm = TRUE)
  baseline_mae <- mean(abs(actual - mean_actual), na.rm = TRUE)
  improvement_mae <- (baseline_mae - mae) / baseline_mae * 100

  # Residual analysis
  residual_stats <- list(
    mean = mean(residuals, na.rm = TRUE),
    median = stats::median(residuals, na.rm = TRUE),
    sd = sd(residuals, na.rm = TRUE),
    q25 = stats::quantile(residuals, 0.25, na.rm = TRUE),
    q75 = stats::quantile(residuals, 0.75, na.rm = TRUE)
  )

  # Prediction accuracy by bins
  pred_data$actual_bin <- cut(actual, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
  accuracy_by_bin <- pred_data %>%
    dplyr::group_by(actual_bin) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_actual = mean(actual, na.rm = TRUE),
      mean_predicted = mean(predicted, na.rm = TRUE),
      mae = mean(abs(residual), na.rm = TRUE),
      rmse = sqrt(mean(residual^2, na.rm = TRUE)),
      .groups = "drop"
    )

  evaluation_result <- list(
    overall_metrics = list(
      n_observations = n,
      mae = mae,
      rmse = rmse,
      mape = mape,
      r_squared = r_squared,
      improvement_over_baseline = improvement_mae
    ),
    residual_analysis = residual_stats,
    accuracy_by_performance_level = accuracy_by_bin,
    feature_importance = model_object$feature_importance,
    model_info = list(
      model_type = model_object$model_type,
      target_var = model_object$target_var,
      n_predictors = length(model_object$predictor_vars),
      training_size = model_object$performance$n_train,
      test_size = model_object$performance$n_test
    )
  )

  return(evaluation_result)
}