#' Calculate Rolling Player Statistics
#'
#' Calculate rolling averages and other time-based statistics for players.
#'
#' @param player_data Data frame with player statistics including date column
#' @param player_col Column name for player identifier
#' @param date_col Column name for date
#' @param stats_cols Vector of statistic column names to calculate rolling averages for
#' @param window Number of games to include in rolling window (default: 5)
#' @param min_games Minimum number of games required for calculation (default: 3)
#' @return Data frame with additional rolling statistic columns
#' @export
#' @examples
#' \dontrun{
#' rolling_stats <- calculate_rolling_stats(
#'   player_data = merged_data,
#'   stats_cols = c("disposals", "goals", "tackles"),
#'   window = 5
#' )
#' }
calculate_rolling_stats <- function(player_data,
                                    player_col = "player_id",
                                    date_col = "date",
                                    stats_cols = c("disposals", "goals", "marks", "tackles"),
                                    window = 5,
                                    min_games = 3) {

  # Check required columns exist
  required_cols <- c(player_col, date_col, stats_cols)
  missing_cols <- required_cols[!required_cols %in% names(player_data)]
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Ensure data is sorted by player and date
  player_data <- player_data %>%
    dplyr::arrange(!!rlang::sym(player_col), !!rlang::sym(date_col))

  # Calculate rolling statistics for each player
  rolling_data <- player_data %>%
    dplyr::group_by(!!rlang::sym(player_col)) %>%
    dplyr::mutate(
      games_played = dplyr::row_number()
    )

  # Add rolling statistics
  for (stat in stats_cols) {
    if (stat %in% names(rolling_data)) {
      if (requireNamespace("slider", quietly = TRUE)) {
        # Rolling mean
        rolling_data[[paste0(stat, "_roll_mean_", window)]] <-
          slider::slide_dbl(rolling_data[[stat]], mean, na.rm = TRUE,
                           .before = window - 1, .complete = FALSE)

        # Rolling standard deviation
        rolling_data[[paste0(stat, "_roll_sd_", window)]] <-
          slider::slide_dbl(rolling_data[[stat]], sd, na.rm = TRUE,
                           .before = window - 1, .complete = FALSE)
      } else {
        # Fallback: simple rolling calculation using zoo or manual method
        rolling_data[[paste0(stat, "_roll_mean_", window)]] <-
          stats::filter(rolling_data[[stat]], rep(1/window, window), sides = 1)
        rolling_data[[paste0(stat, "_roll_sd_", window)]] <- NA_real_
      }

      # Set to NA if insufficient games
      insufficient_games <- rolling_data$games_played < min_games
      rolling_data[[paste0(stat, "_roll_mean_", window)]][insufficient_games] <- NA_real_
      rolling_data[[paste0(stat, "_roll_sd_", window)]][insufficient_games] <- NA_real_
    }
  }

  rolling_data <- rolling_data %>%
    dplyr::ungroup()

  return(rolling_data)
}

#' Calculate Team Strength Metrics
#'
#' Calculate various team strength and performance metrics.
#'
#' @param results_data Data frame with match results
#' @param window Number of recent games to consider for form (default: 5)
#' @return Data frame with team strength metrics
#' @export
#' @examples
#' \dontrun{
#' team_strength <- calculate_team_strength(clean_results)
#' }
calculate_team_strength <- function(results_data, window = 5) {

  # Calculate win percentage and recent form for each team
  team_metrics <- results_data %>%
    dplyr::arrange(date) %>%
    # Create home team records
    dplyr::mutate(home_away = "Home") %>%
    dplyr::select(team = home_team, date, season, round_number,
                  score_for = home_score, score_against = away_score,
                  opponent = away_team, home_away,
                  margin, winner) %>%
    dplyr::bind_rows(
      # Add away team records
      results_data %>%
        dplyr::mutate(home_away = "Away") %>%
        dplyr::select(team = away_team, date, season, round_number,
                      score_for = away_score, score_against = home_score,
                      opponent = home_team, home_away,
                      margin, winner) %>%
        dplyr::mutate(margin = -margin)  # Flip margin for away team
    ) %>%
    dplyr::arrange(team, date) %>%
    dplyr::group_by(team) %>%
    dplyr::mutate(
      # Basic results
      win = ifelse(team == winner, 1, ifelse(winner == "Draw", 0.5, 0)),
      games_played = dplyr::row_number(),

      # Cumulative statistics
      cumulative_wins = cumsum(win),
      cumulative_points_for = cumsum(score_for),
      cumulative_points_against = cumsum(score_against),

      # Win percentage
      win_percentage = cumulative_wins / games_played,

      # Average scores
      avg_score_for = cumulative_points_for / games_played,
      avg_score_against = cumulative_points_against / games_played,
      avg_margin = (cumulative_points_for - cumulative_points_against) / games_played,

      # Recent form (rolling)
      recent_form = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(win, mean, na.rm = TRUE, .before = window - 1, .complete = FALSE)
      } else {
        as.numeric(stats::filter(win, rep(1/window, window), sides = 1))
      },
      recent_avg_margin = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(margin, mean, na.rm = TRUE, .before = window - 1, .complete = FALSE)
      } else {
        as.numeric(stats::filter(margin, rep(1/window, window), sides = 1))
      },

      # Home/Away splits
      home_games = cumsum(home_away == "Home"),
      away_games = cumsum(home_away == "Away"),
      home_wins = cumsum(win * (home_away == "Home")),
      away_wins = cumsum(win * (home_away == "Away"))
    ) %>%
    dplyr::mutate(
      home_win_pct = ifelse(home_games > 0, home_wins / home_games, NA_real_),
      away_win_pct = ifelse(away_games > 0, away_wins / away_games, NA_real_)
    ) %>%
    dplyr::ungroup()

  return(team_metrics)
}

#' Calculate Player Impact Metrics
#'
#' Calculate advanced player impact metrics including efficiency ratings.
#'
#' @param player_data Data frame with player statistics
#' @return Data frame with additional impact metrics
#' @export
#' @examples
#' \dontrun{
#' player_impact <- calculate_player_impact(clean_player_stats)
#' }
calculate_player_impact <- function(player_data) {

  impact_data <- player_data %>%
    dplyr::mutate(
      # Efficiency metrics
      # disposal_efficiency = ifelse(disposals > 0, effective_disposals / disposals * 100, NA_real_),
      kicking_efficiency = ifelse(kicks > 0,
                                  (kicks - clangers) / kicks * 100, NA_real_),

      # Impact per touch
      score_per_disposal = ifelse(disposals > 0, total_score / disposals, 0),
      metres_per_disposal = ifelse(disposals > 0, metres_gained / disposals, 0),

      # Contested vs uncontested balance
      contested_percentage = ifelse(disposals > 0,
                                   contested_possessions / disposals * 100, NA_real_),

      # Forward metrics
      score_accuracy = ifelse(scoring_shots > 0, goals / scoring_shots * 100, NA_real_),
      score_per_inside50 = ifelse(inside50s > 0, total_score / inside50s, 0),

      # Defensive metrics
      intercept_rate = ifelse(disposals > 0, intercepts / disposals * 100, 0),
      tackle_efficiency = ifelse(contested_possessions > 0,
                                tackles / contested_possessions * 100, 0),

      # Overall impact score (weighted combination)
      impact_score = (
        0.3 * pmin(pmax((disposals - 10) / 15, 0), 1) +  # Possession volume (0-1)
        0.2 * pmin(pmax((disposal_efficiency - 60) / 30, 0), 1) +  # Efficiency (0-1)
        0.25 * pmin(total_score / 12, 1) +  # Scoring (0-1, max at 2 goals)
        0.15 * pmin(tackles / 8, 1) +  # Tackling (0-1, max at 8 tackles)
        0.1 * pmin(metres_gained / 300, 1)  # Territory gain (0-1, max at 300m)
      ) * 100
    )

  return(impact_data)
}

#' Create Match Context Features
#'
#' Create features that capture match context (venue, weather, importance).
#'
#' @param match_data Data frame with match information
#' @return Data frame with additional context features
#' @export
#' @examples
#' \dontrun{
#' context_features <- create_match_context(results_data)
#' }
create_match_context <- function(match_data) {

  context_data <- match_data %>%
    dplyr::mutate(
      # Venue advantages
      venue_clean = trimws(tolower(venue)),

      # Round importance
      round_type = dplyr::case_when(
        round_number <= 3 ~ "Early",
        round_number >= max(round_number, na.rm = TRUE) - 2 ~ "Late",
        TRUE ~ "Mid"
      ),

      # Season progression
      season_pct = round_number / max(round_number, na.rm = TRUE),

      # Day of week (if available)
      day_of_week = lubridate::wday(date, label = TRUE),
      weekend_game = day_of_week %in% c("Sat", "Sun"),

      # Month effects
      month = lubridate::month(date),
      season_period = dplyr::case_when(
        month %in% c(1, 2, 3) ~ "Summer",
        month %in% c(4, 5, 6) ~ "Autumn",
        month %in% c(7, 8, 9) ~ "Winter",
        month %in% c(10, 11, 12) ~ "Spring"
      )
    )

  # Calculate venue advantages for home teams
  venue_stats <- context_data %>%
    dplyr::group_by(venue_clean, home_team) %>%
    dplyr::summarise(
      home_games_at_venue = dplyr::n(),
      home_wins_at_venue = sum(margin > 0, na.rm = TRUE),
      home_win_pct_venue = home_wins_at_venue / home_games_at_venue,
      avg_home_margin_venue = mean(margin, na.rm = TRUE),
      .groups = "drop"
    )

  # Add venue advantage back to main data
  context_data <- context_data %>%
    dplyr::left_join(venue_stats, by = c("venue_clean", "home_team")) %>%
    dplyr::mutate(
      venue_advantage = ifelse(home_games_at_venue >= 3, avg_home_margin_venue, 0),
      familiar_venue = home_games_at_venue >= 5
    )

  return(context_data)
}

#' Calculate Head-to-Head Statistics
#'
#' Calculate historical head-to-head statistics between teams.
#'
#' @param results_data Data frame with match results
#' @param lookback_games Number of recent H2H games to consider (default: 10)
#' @return Data frame with H2H statistics added
#' @export
#' @examples
#' \dontrun{
#' h2h_data <- calculate_h2h_stats(results_data)
#' }
calculate_h2h_stats <- function(results_data, lookback_games = 10) {

  # Sort by date
  results_sorted <- results_data %>%
    dplyr::arrange(date)

  # Calculate head-to-head for each match
  h2h_data <- results_sorted %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # Get historical matchups between these teams before this game
      h2h_data = list({
        prior_games <- results_sorted %>%
          dplyr::filter(
            date < .data$date,
            ((home_team == .data$home_team & away_team == .data$away_team) |
             (home_team == .data$away_team & away_team == .data$home_team))
          ) %>%
          dplyr::arrange(dplyr::desc(date)) %>%
          dplyr::slice_head(n = lookback_games)

        if (nrow(prior_games) == 0) {
          list(
            h2h_games = 0,
            home_team_h2h_wins = 0,
            away_team_h2h_wins = 0,
            h2h_win_pct_home = 0.5,
            avg_h2h_margin_home = 0,
            avg_h2h_total = 0
          )
        } else {

        # Calculate statistics from home team perspective
        home_wins <- sum(
          (prior_games$home_team == .data$home_team & prior_games$margin > 0) |
          (prior_games$away_team == .data$home_team & prior_games$margin < 0)
        )

        away_wins <- sum(
          (prior_games$home_team == .data$away_team & prior_games$margin > 0) |
          (prior_games$away_team == .data$away_team & prior_games$margin < 0)
        )

        # Adjust margins from home team perspective
        adjusted_margins <- ifelse(
          prior_games$home_team == .data$home_team,
          prior_games$margin,
          -prior_games$margin
        )

        list(
          h2h_games = nrow(prior_games),
          home_team_h2h_wins = home_wins,
          away_team_h2h_wins = away_wins,
          h2h_win_pct_home = ifelse(nrow(prior_games) > 0, home_wins / nrow(prior_games), 0.5),
          avg_h2h_margin_home = mean(adjusted_margins, na.rm = TRUE),
          avg_h2h_total = mean(prior_games$total_score, na.rm = TRUE)
        )
        }
      })
    ) %>%
    dplyr::ungroup()

  # Expand the nested list into columns
  h2h_expanded <- h2h_data %>%
    dplyr::mutate(
      h2h_games = purrr::map_dbl(h2h_data, ~ .x$h2h_games),
      home_team_h2h_wins = purrr::map_dbl(h2h_data, ~ .x$home_team_h2h_wins),
      away_team_h2h_wins = purrr::map_dbl(h2h_data, ~ .x$away_team_h2h_wins),
      h2h_win_pct_home = purrr::map_dbl(h2h_data, ~ .x$h2h_win_pct_home),
      avg_h2h_margin_home = purrr::map_dbl(h2h_data, ~ .x$avg_h2h_margin_home),
      avg_h2h_total = purrr::map_dbl(h2h_data, ~ .x$avg_h2h_total)
    ) %>%
    dplyr::select(-h2h_data)

  return(h2h_expanded)
}

#' Create Lag Features
#'
#' Create lagged versions of key metrics for modeling.
#'
#' @param data Data frame with time series data
#' @param group_col Column to group by (e.g., team, player)
#' @param date_col Date column for ordering
#' @param feature_cols Vector of columns to create lags for
#' @param lags Vector of lag periods (default: c(1, 3, 5))
#' @return Data frame with lagged features
#' @export
#' @examples
#' \dontrun{
#' lagged_data <- create_lag_features(
#'   team_data,
#'   group_col = "team",
#'   feature_cols = c("win_percentage", "avg_margin")
#' )
#' }
create_lag_features <- function(data,
                                group_col,
                                date_col = "date",
                                feature_cols,
                                lags = c(1, 3, 5)) {

  lagged_data <- data %>%
    dplyr::arrange(!!rlang::sym(group_col), !!rlang::sym(date_col)) %>%
    dplyr::group_by(!!rlang::sym(group_col))

  # Create lagged features
  for (col in feature_cols) {
    if (col %in% names(data)) {
      for (lag_period in lags) {
        lag_col_name <- paste0(col, "_lag", lag_period)
        lagged_data <- lagged_data %>%
          dplyr::mutate(
            !!lag_col_name := dplyr::lag(!!rlang::sym(col), n = lag_period)
          )
      }
    }
  }

  lagged_data <- lagged_data %>%
    dplyr::ungroup()

  return(lagged_data)
}
