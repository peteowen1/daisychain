#' Calculate Player Match Value
#'
#' Calculates expected match value for each player using data-driven statistical weights.
#' Can use either predicted statistics or recent performance averages.
#'
#' @param player_data Data frame with player statistics or predictions
#' @param stat_weights Named numeric vector of weights for each statistic
#' @param prediction_results Optional list from player prediction model (default: NULL)
#' @param use_time_on_ground Weight by time_on_ground_percentage if available (default: TRUE)
#'
#' @return Data frame with player match values
#' @export
#'
#' @examples
#' \dontrun{
#' player_values <- calculate_player_match_value(
#'   player_data,
#'   stat_weights = c(goals = 0.4, disposals = 0.2, marks = 0.15),
#'   prediction_results = player_prediction_results
#' )
#' }
calculate_player_match_value <- function(player_data,
                                          stat_weights,
                                          prediction_results = NULL,
                                          use_time_on_ground = TRUE) {

  # Use predictions if available, otherwise use recent averages
  if (!is.null(prediction_results) && !is.null(prediction_results$latest_predictions)) {
    value_data <- prediction_results$latest_predictions %>%
      dplyr::select(player_id, player_name, team, dplyr::contains("_predicted")) %>%
      dplyr::rename_with(~ gsub("_predicted", "", .x), dplyr::contains("_predicted"))

  } else {
    # Fall back to recent performance averages
    value_data <- player_data %>%
      dplyr::group_by(player_id, player_name, team) %>%
      dplyr::summarise(
        dplyr::across(names(stat_weights), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # Initialize match value
  value_data <- value_data %>%
    dplyr::mutate(predicted_match_value = 0)

  # Add time on ground weight if requested
  if (use_time_on_ground && "time_on_ground_percentage" %in% names(value_data)) {
    value_data <- value_data %>%
      dplyr::mutate(time_on_ground_weight = time_on_ground_percentage)
  } else {
    value_data <- value_data %>%
      dplyr::mutate(time_on_ground_weight = 1)
  }

  # Apply weights to calculate total value
  for (stat in names(stat_weights)) {
    if (stat %in% names(value_data) && !is.na(stat_weights[stat])) {
      value_data <- value_data %>%
        dplyr::mutate(
          predicted_match_value = predicted_match_value +
            (!!rlang::sym(stat)) * stat_weights[stat] * time_on_ground_weight
        )
    }
  }

  # Sort by value
  value_data <- value_data %>%
    dplyr::arrange(dplyr::desc(predicted_match_value))

  return(value_data)
}


#' Aggregate Player Values to Team Level
#'
#' Aggregates individual player match values to team-level metrics.
#'
#' @param player_values Data frame with player match values (from calculate_player_match_value)
#'
#' @return Data frame with team-level value metrics
#' @export
#'
#' @examples
#' \dontrun{
#' team_values <- aggregate_team_values(player_match_values)
#' }
aggregate_team_values <- function(player_values) {

  team_values <- player_values %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      total_team_value = sum(predicted_match_value, na.rm = TRUE),
      avg_player_value = mean(predicted_match_value, na.rm = TRUE),
      top_player_value = max(predicted_match_value, na.rm = TRUE),
      value_depth = stats::quantile(predicted_match_value, 0.8, na.rm = TRUE),
      players = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(total_team_value))

  return(team_values)
}


#' Learn Statistical Weights from Data
#'
#' Uses regularized regression (elastic net) to learn statistical weights that
#' best predict team margins from player statistics.
#'
#' @param player_data Data frame with player statistics and match margins
#' @param stats_to_weight Character vector of statistics to include
#' @param alpha Elastic net mixing parameter (0=ridge, 1=lasso, default=0.5)
#' @param cv_folds Number of cross-validation folds (default: 5)
#'
#' @return List with learned weights and model diagnostics
#' @export
#'
#' @examples
#' \dontrun{
#' learned_weights <- learn_stat_weights_from_data(
#'   player_data,
#'   stats_to_weight = c("disposals", "goals", "marks", "tackles")
#' )
#' }
learn_stat_weights_from_data <- function(player_data,
                                          stats_to_weight,
                                          alpha = 0.5,
                                          cv_folds = 5) {

  if (!requireNamespace("glmnet", quietly = TRUE)) {
    warning("glmnet package not available - cannot learn weights")
    return(NULL)
  }

  # Prepare player-level regression data
  regression_data <- player_data %>%
    dplyr::filter(!is.na(margin)) %>%
    dplyr::mutate(
      team_margin = ifelse(home_away == "Home", margin, -margin),
      weight = ifelse(is.na(time_on_ground_percentage), 0.75, time_on_ground_percentage)
    ) %>%
    dplyr::select(player_id, team_margin, weight, dplyr::all_of(stats_to_weight)) %>%
    tidyr::drop_na()

  if (nrow(regression_data) < 50) {
    warning("Insufficient data for weight learning")
    return(NULL)
  }

  # Prepare matrices
  X_matrix <- as.matrix(regression_data[, stats_to_weight])
  y_vector <- regression_data$team_margin
  weights_vector <- regression_data$weight

  cat("Fitting elastic net regression to learn statistical weights...\n")

  # Fit elastic net with cross-validation
  cv_model <- glmnet::cv.glmnet(
    X_matrix, y_vector,
    weights = weights_vector,
    alpha = alpha,
    nfolds = cv_folds
  )

  # Extract coefficients at optimal lambda
  optimal_coeffs <- stats::coef(cv_model, s = "lambda.min")
  stat_weights <- as.vector(optimal_coeffs[-1])  # Remove intercept
  names(stat_weights) <- stats_to_weight

  # Create summary data frame
  weights_df <- data.frame(
    statistic = names(stat_weights),
    weight = stat_weights,
    abs_weight = abs(stat_weights),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::arrange(dplyr::desc(abs_weight))

  cat("\nLearned Statistical Weights:\n")
  print(weights_df)

  return(list(
    weights = stat_weights,
    weights_df = weights_df,
    cv_model = cv_model,
    lambda_min = cv_model$lambda.min,
    cvm = min(cv_model$cvm)
  ))
}
