#' Prepare Game Margins Data
#'
#' Extracts game margins from enhanced player data with team perspective
#'
#' @param player_data Enhanced player data with margin and home_away columns
#'
#' @return Data frame with team, date, and team_margin
#'
#' @importFrom dplyr select distinct mutate
#' @export
prepare_game_margins <- function(player_data) {
  player_data %>%
    select(team, date, margin, home_away) %>%
    distinct() %>%
    mutate(team_margin = ifelse(home_away == "Home", margin, -margin))
}


#' Fit Ridge Regression for Margin Prediction
#'
#' Fits a ridge regression model using cross-validation and extracts coefficients
#'
#' @param regression_data Data frame with predictor columns and team_margin
#' @param predictor_cols Vector of predictor column names
#' @param nfolds Number of CV folds
#'
#' @return List with model, weights data frame, predictions, R2, and RMSE
#'
#' @importFrom glmnet cv.glmnet coef.glmnet
#' @export
fit_margin_ridge_regression <- function(regression_data, predictor_cols, nfolds = 10) {

  # Prepare matrices
  X <- as.matrix(regression_data[, predictor_cols])
  y <- regression_data$team_margin

  # Fit ridge regression
  cv_model <- glmnet::cv.glmnet(
    X, y,
    alpha = 0,  # Ridge regression (L2)
    nfolds = min(nfolds, nrow(regression_data))
  )

  # Extract coefficients at lambda.min (first element is intercept)
  optimal_coeffs <- coef(cv_model, s = "lambda.min")
  intercept <- as.numeric(optimal_coeffs[1])
  stat_weights <- as.vector(optimal_coeffs[-1])
  names(stat_weights) <- gsub("_predicted", "", predictor_cols)

  # Create weights data frame (include intercept as first row)
  weights_df <- data.frame(
    statistic = c("(Intercept)", names(stat_weights)),
    weight    = c(intercept,        stat_weights),
    abs_weight = abs(c(intercept,   stat_weights)),
    stringsAsFactors = FALSE
  )
  weights_df <- weights_df[order(weights_df$abs_weight, decreasing = TRUE), ]

  # In-sample predictions & metrics at lambda.min
  predictions <- as.vector(stats::predict(cv_model, X, s = "lambda.min"))
  r2 <- 1 - sum((y - predictions)^2) / sum((y - mean(y))^2)
  rmse <- sqrt(mean((y - predictions)^2))

  list(
    model = cv_model,
    intercept = intercept,
    weights = stat_weights,
    weights_df = weights_df,
    predictions = predictions,
    r2 = r2,
    rmse = rmse,
    n_obs = nrow(regression_data)
  )
}


#' Prepare Team-Date Regression Data
#'
#' Aggregates predicted player stats to team-date level and joins with margins
#'
#' @param predicted_stats Player-level predicted statistics
#' @param game_margins Game margins with team_margin column
#' @param pred_stat_cols Vector of predicted stat column names
#'
#' @return Data frame ready for regression
#'
#' @importFrom dplyr group_by summarise across inner_join select
#' @importFrom tidyr drop_na
#' @export
prepare_team_date_regression <- function(predicted_stats, game_margins, pred_stat_cols) {

  # Sum predicted stats by team and date
  team_date_predictions <- predicted_stats %>%
    group_by(team, date) %>%
    summarise(
      across(all_of(pred_stat_cols), ~ sum(.x, na.rm = TRUE)),
      n_players = n(),
      .groups = "drop"
    )

  # Match with actual margins
  team_date_predictions %>%
    inner_join(game_margins, by = c("team", "date")) %>%
    select(team, date, team_margin, all_of(pred_stat_cols), n_players) %>%
    drop_na()
}


#' Prepare Player-Date Regression Data
#'
#' Joins player predicted stats with game margins
#'
#' @param predicted_stats Player-level predicted statistics
#' @param game_margins Game margins with team_margin column
#' @param pred_stat_cols Vector of predicted stat column names
#'
#' @return Data frame ready for regression
#'
#' @importFrom dplyr inner_join select
#' @importFrom tidyr drop_na
#' @export
prepare_player_date_regression <- function(predicted_stats, game_margins, pred_stat_cols) {

  predicted_stats %>%
    inner_join(game_margins, by = c("team", "date")) %>%
    select(player_id, player_name, team, date, team_margin, all_of(pred_stat_cols)) %>%
    drop_na()
}


#' Generate Margin Predictions from Ridge Model
#'
#' Generates predictions using a fitted ridge regression model
#'
#' @param model_result Result from fit_margin_ridge_regression
#' @param prediction_data Data frame with predictor columns
#' @param predictor_cols Vector of predictor column names
#'
#' @return Vector of predictions
#'
#' @export
predict_from_ridge <- function(model_result, prediction_data, predictor_cols) {

  X_pred <- as.matrix(prediction_data[, predictor_cols])
  as.vector(predict(model_result$model, X_pred, s = "lambda.min"))
}
