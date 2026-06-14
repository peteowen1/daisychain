#' Prepare Match-Level Data from Player Data
#'
#' Converts player-level data to match-level data with home/away team structure.
#'
#' @param player_data Player-level data frame with home_away indicator
#' @param team_strength Optional data frame with team and team_strength columns
#'
#' @return Data frame with one row per match
#'
#' @importFrom dplyr filter select distinct inner_join left_join rename mutate arrange drop_na
#' @importFrom tidyr drop_na
#' @export
prepare_match_data_from_player <- function(player_data, team_strength = NULL) {

  # Get home team data
  matches_home <- player_data %>%
    filter(home_away == "Home") %>%
    select(round_number, date, match_id, team, margin, home_score, venue) %>%
    distinct(match_id, .keep_all = TRUE) %>%
    rename(home_team = team, home_margin = margin)

  # Get away team data
  matches_away <- player_data %>%
    filter(home_away == "Away") %>%
    select(match_id, team, away_score) %>%
    distinct(match_id, .keep_all = TRUE) %>%
    rename(away_team = team)

  # Join to get both teams in same row
  match_data <- matches_home %>%
    inner_join(matches_away, by = "match_id") %>%
    select(round_number, date, match_id, venue, home_team, away_team, home_margin, home_score, away_score) %>%
    drop_na(home_team, away_team, home_margin) %>%
    arrange(date)

  # Add team strengths if provided
  if (!is.null(team_strength)) {
    match_data <- match_data %>%
      left_join(team_strength, by = c("home_team" = "team")) %>%
      rename(home_strength = team_strength) %>%
      left_join(team_strength, by = c("away_team" = "team")) %>%
      rename(away_strength = team_strength)
  }

  return(match_data)
}


#' Add Team Stat Differences to Match Data
#'
#' Aggregates player-level predicted stats to team level and adds home-away
#' differences to match data.
#'
#' @param match_data Match-level data frame
#' @param predicted_stats Player-level predicted statistics
#' @param stat_cols Vector of statistic column names to aggregate
#'
#' @return Match data with added stat difference columns
#'
#' @importFrom dplyr group_by summarise across left_join
#' @export
add_team_stat_differences <- function(match_data, predicted_stats, stat_cols) {

  # Sum predicted stats by team and date
  team_date_stats <- predicted_stats %>%
    group_by(team, date) %>%
    summarise(
      across(all_of(stat_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # Join home team stats
  match_data <- match_data %>%
    left_join(
      team_date_stats,
      by = c("home_team" = "team", "date" = "date"),
      suffix = c("", "_home")
    )

  # Rename home stats
  for (stat in stat_cols) {
    names(match_data)[names(match_data) == stat] <- paste0("home_", stat)
  }

  # Join away team stats
  match_data <- match_data %>%
    left_join(
      team_date_stats,
      by = c("away_team" = "team", "date" = "date"),
      suffix = c("", "_away")
    )

  # Rename away stats
  for (stat in stat_cols) {
    names(match_data)[names(match_data) == stat] <- paste0("away_", stat)
  }

  # Calculate differences (home - away) and sums (home + away)
  stat_diff_cols <- c()
  stat_sum_cols <- c()
  for (stat in stat_cols) {
    stat_name <- gsub("_predicted$", "", stat)

    # Difference
    diff_col <- paste0("pred_", stat_name, "_diff")
    match_data[[diff_col]] <- match_data[[paste0("home_", stat)]] - match_data[[paste0("away_", stat)]]
    stat_diff_cols <- c(stat_diff_cols, diff_col)

    # Sum
    sum_col <- paste0("pred_", stat_name, "_sum")
    match_data[[sum_col]] <- match_data[[paste0("home_", stat)]] + match_data[[paste0("away_", stat)]]
    stat_sum_cols <- c(stat_sum_cols, sum_col)
  }

  # Add stat columns as attributes for later use
  attr(match_data, "stat_diff_cols") <- stat_diff_cols
  attr(match_data, "stat_sum_cols") <- stat_sum_cols

  return(match_data)
}


#' Prepare Data for GAM Models
#'
#' Prepares match data for GAM modeling with proper factor levels.
#'
#' @param data Match data to prepare
#' @param stat_diff_cols Vector of stat difference column names
#' @param target Target variable name (e.g., "home_margin", "home_win"). Can be NULL for prediction data.
#' @param reference_levels Optional list with names: home_team_season, away_team_season, venue
#'   containing factor levels from training data
#'
#' @return Prepared data frame with factors
#'
#' @importFrom dplyr select mutate all_of
#' @export
prepare_gam_data <- function(data, stat_diff_cols, target = NULL,
                              reference_levels = NULL) {

  # Select relevant columns (including all ELO features)
  elo_features <- c(
    "winloss_elo_diff", "winloss_home_momentum", "winloss_away_momentum",
    "margin_elo_diff", "margin_home_momentum", "margin_away_momentum"
  )

  # Check which ELO features exist in data
  available_elo <- elo_features[elo_features %in% names(data)]

  select_cols <- c(
    "home_strength", "away_strength", "strength_diff",
    available_elo,
    stat_diff_cols,
    "home_team_season", "away_team_season", "venue", "round_number"
  )

  # Add target if provided
  if (!is.null(target)) {
    select_cols <- c(target, select_cols)
  }

  gam_data <- data %>%
    select(all_of(select_cols))

  # Convert to factors
  if (is.null(reference_levels)) {
    # Create new factor levels (for training data)
    gam_data <- gam_data %>%
      mutate(
        home_team_season = factor(home_team_season),
        away_team_season = factor(away_team_season),
        venue = factor(venue)
      )
  } else {
    # Use existing factor levels (for test data)
    gam_data <- gam_data %>%
      mutate(
        home_team_season = factor(home_team_season,
                                   levels = reference_levels$home_team_season),
        away_team_season = factor(away_team_season,
                                   levels = reference_levels$away_team_season),
        venue = factor(venue, levels = reference_levels$venue)
      )
  }

  return(gam_data)
}


#' Prepare Upcoming Matchups for Prediction
#'
#' Creates all possible team matchups and prepares features for prediction.
#'
#' @param match_data Historical match data for extracting teams
#' @param team_strength Data frame with team and team_strength columns
#' @param latest_team_stats Latest team-level statistics
#' @param pred_stat_cols Vector of predicted stat column names
#' @param gam_factor_levels Optional list with factor levels for GAM predictions
#' @param latest_season Latest season year for team-season interactions
#'
#' @return Data frame of all possible matchups with features
#'
#' @importFrom dplyr filter mutate left_join rename select group_by slice_max ungroup
#' @importFrom tidyr drop_na
#' @export
prepare_upcoming_matchups <- function(match_data, team_strength, latest_team_stats,
                                       pred_stat_cols, gam_factor_levels = NULL,
                                       latest_season = NULL) {

  # Determine latest season if not provided
  if (is.null(latest_season)) {
    latest_season <- max(lubridate::year(match_data$date))
  }

  # Create all possible matchups
  teams <- unique(c(match_data$home_team, match_data$away_team))
  upcoming_matchups <- expand.grid(
    home_team = teams,
    away_team = teams,
    stringsAsFactors = FALSE
  ) %>%
    filter(home_team != away_team)

  # Add team strengths
  upcoming_matchups <- upcoming_matchups %>%
    left_join(team_strength, by = c("home_team" = "team")) %>%
    rename(home_strength = team_strength) %>%
    left_join(team_strength, by = c("away_team" = "team")) %>%
    rename(away_strength = team_strength) %>%
    mutate(strength_diff = home_strength - away_strength) %>%
    drop_na()

  # Join home team latest stats
  upcoming_matchups <- upcoming_matchups %>%
    left_join(latest_team_stats, by = c("home_team" = "team"))

  for (stat in pred_stat_cols) {
    names(upcoming_matchups)[names(upcoming_matchups) == stat] <- paste0("home_", stat)
  }

  # Join away team latest stats
  upcoming_matchups <- upcoming_matchups %>%
    left_join(latest_team_stats, by = c("away_team" = "team"))

  for (stat in pred_stat_cols) {
    names(upcoming_matchups)[names(upcoming_matchups) == stat] <- paste0("away_", stat)
  }

  # Calculate stat differences
  stat_diff_cols <- c()
  for (stat in pred_stat_cols) {
    stat_name <- gsub("_predicted$", "", stat)
    diff_col <- paste0("pred_", stat_name, "_diff")
    upcoming_matchups[[diff_col]] <- upcoming_matchups[[paste0("home_", stat)]] -
      upcoming_matchups[[paste0("away_", stat)]]
    stat_diff_cols <- c(stat_diff_cols, diff_col)
  }

  # Add variables needed for GAM if factor levels provided
  if (!is.null(gam_factor_levels)) {
    upcoming_matchups <- upcoming_matchups %>%
      mutate(
        venue = names(which.max(table(match_data$venue))),
        round_number = median(match_data$round_number, na.rm = TRUE),
        season = latest_season,
        home_team_season = paste0(home_team, "_", season),
        away_team_season = paste0(away_team, "_", season)
      )
  }

  # Store stat_diff_cols as attribute
  attr(upcoming_matchups, "stat_diff_cols") <- stat_diff_cols

  return(upcoming_matchups)
}
