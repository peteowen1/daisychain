#' Add ELO Features to Match Data
#'
#' Joins ELO ratings to match data for both home and away teams.
#' For historical matches, uses rating_before from that date.
#' For future matches, uses current ratings.
#'
#' @param match_data Match-level data frame with home_team, away_team, date
#' @param team_elo_ratings ELO ratings time series with team, date, rating_before, rating_after
#' @param current_elo Optional data frame with team and current_rating for future predictions
#'
#' @return Match data with added home_elo, away_elo, and elo_diff columns
#'
#' @importFrom dplyr left_join rename mutate select coalesce
#' @export
add_elo_features_to_matches <- function(match_data, team_elo_ratings, current_elo = NULL) {

  # Join home team ELO (historical)
  result <- match_data %>%
    left_join(
      team_elo_ratings %>% select(team, date, rating_before),
      by = c("home_team" = "team", "date" = "date")
    ) %>%
    rename(home_elo_historical = rating_before)

  # Join away team ELO (historical)
  result <- result %>%
    left_join(
      team_elo_ratings %>% select(team, date, rating_before),
      by = c("away_team" = "team", "date" = "date")
    ) %>%
    rename(away_elo_historical = rating_before)

  # If current ratings provided, join and use as fallback for missing dates
  if (!is.null(current_elo)) {
    result <- result %>%
      left_join(
        current_elo %>% select(team, current_rating),
        by = c("home_team" = "team")
      ) %>%
      rename(home_elo_current = current_rating) %>%
      left_join(
        current_elo %>% select(team, current_rating),
        by = c("away_team" = "team")
      ) %>%
      rename(away_elo_current = current_rating)

    # Use historical if available, otherwise current
    result <- result %>%
      mutate(
        home_elo = coalesce(home_elo_historical, home_elo_current),
        away_elo = coalesce(away_elo_historical, away_elo_current)
      ) %>%
      select(-home_elo_historical, -away_elo_historical, -home_elo_current, -away_elo_current)
  } else {
    result <- result %>%
      rename(home_elo = home_elo_historical, away_elo = away_elo_historical)
  }

  # Calculate ELO difference
  result <- result %>%
    mutate(elo_diff = home_elo - away_elo)

  return(result)
}


#' Calculate ELO Momentum
#'
#' Calculates rolling average of ELO rating changes to measure team momentum.
#'
#' @param team_elo_ratings ELO ratings time series with team, date, rating_change
#' @param window Number of games for rolling window (default 5)
#'
#' @return Data frame with team, date, and elo_momentum
#'
#' @importFrom dplyr group_by arrange mutate ungroup select
#' @export
calculate_elo_momentum <- function(team_elo_ratings, window = 5) {

  # Check if slider package is available for rolling calculations
  if (requireNamespace("slider", quietly = TRUE)) {
    result <- team_elo_ratings %>%
      group_by(team) %>%
      arrange(date) %>%
      mutate(
        elo_momentum = slider::slide_dbl(
          rating_change,
          mean,
          .before = window - 1,
          .complete = FALSE
        )
      ) %>%
      ungroup() %>%
      select(team, date, elo_momentum)
  } else {
    # Fallback to base R rolling mean
    result <- team_elo_ratings %>%
      group_by(team) %>%
      arrange(date) %>%
      mutate(
        elo_momentum = sapply(
          seq_along(rating_change),
          function(i) {
            start_idx <- max(1, i - window + 1)
            mean(rating_change[start_idx:i], na.rm = TRUE)
          }
        )
      ) %>%
      ungroup() %>%
      select(team, date, elo_momentum)
  }

  return(result)
}


#' Get Team ELO at Date
#'
#' Retrieves a team's ELO rating at or immediately before a specific date.
#' Useful for looking up ratings for prediction on arbitrary dates.
#'
#' @param team Team name
#' @param date Date to look up rating (Date object or string)
#' @param team_elo_ratings ELO ratings time series
#' @param current_elo Optional current ratings if date is after all historical data
#'
#' @return Numeric ELO rating, or NA if not found
#'
#' @importFrom dplyr filter arrange slice_tail pull
#' @export
get_team_elo_at_date <- function(team, date, team_elo_ratings, current_elo = NULL) {

  # Convert date if string
  if (is.character(date)) {
    date <- as.Date(date)
  }

  # Get team's ELO history up to the date
  team_history <- team_elo_ratings %>%
    filter(team == !!team, date <= !!date) %>%
    arrange(date)

  # If we have history, return the last rating_after
  if (nrow(team_history) > 0) {
    return(team_history %>% slice_tail(n = 1) %>% pull(rating_after))
  }

  # If no history but current_elo provided, use that
  if (!is.null(current_elo)) {
    current_rating <- current_elo %>%
      filter(team == !!team) %>%
      pull(current_rating)

    if (length(current_rating) > 0) {
      return(current_rating[1])
    }
  }

  # No rating found
  return(NA_real_)
}


#' Add ELO Momentum Features to Match Data
#'
#' Convenience function to add both ELO ratings and momentum to match data.
#'
#' @param match_data Match-level data frame
#' @param team_elo_ratings ELO ratings time series
#' @param elo_momentum ELO momentum data frame from calculate_elo_momentum()
#' @param current_elo Optional current ratings for future predictions
#'
#' @return Match data with ELO and momentum features
#'
#' @importFrom dplyr left_join
#' @export
add_elo_momentum_features <- function(match_data, team_elo_ratings, elo_momentum, current_elo = NULL) {

  # Add basic ELO features
  result <- add_elo_features_to_matches(match_data, team_elo_ratings, current_elo)

  # Add momentum for home team
  result <- result %>%
    left_join(
      elo_momentum %>% select(team, date, elo_momentum),
      by = c("home_team" = "team", "date" = "date")
    ) %>%
    rename(home_elo_momentum = elo_momentum)

  # Add momentum for away team
  result <- result %>%
    left_join(
      elo_momentum %>% select(team, date, elo_momentum),
      by = c("away_team" = "team", "date" = "date")
    ) %>%
    rename(away_elo_momentum = elo_momentum)

  # Calculate momentum difference
  result <- result %>%
    mutate(elo_momentum_diff = home_elo_momentum - away_elo_momentum)

  return(result)
}
