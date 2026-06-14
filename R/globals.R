#' @importFrom magrittr %>%
#' @importFrom stats complete.cases quantile sd
#' @importFrom rlang :=
#' @importFrom dplyr filter select mutate group_by ungroup summarise arrange desc slice_head slice_tail bind_rows left_join
#' @importFrom scales percent
NULL

# Global variables used in dplyr operations to avoid R CMD check NOTEs
utils::globalVariables(c(
  "player_id", "player_name", "team", "season", "round_number", "match_id",
  "home_team", "away_team", "home_score", "away_score", "margin", "winner",
  "date", "venue", "home_away", "opponent", "year_season", "team_season", "opponent_season",
  "disposals", "goals", "marks", "tackles",
  "disposal_efficiency", "overall_rating", "signed_margin_contribution",
  "recent_rating", "recent_form", "games_played", "avg_rating", "avg_margin_contribution",
  "top_player_rating", "depth_rating", "rating_sd", "avg_team_rating",
  "star_power_score", "depth_score", "balance_score", "roster_strength",
  "rating_before", "rating_after", "rating_change", "term", "estimate",
  "importance", "p.value", "abs_estimate", "importance_rank", "goals_z",
  "disposals_z", "marks_z", "tackles_z", "disposal_efficiency_z",
  "contribution_score", "team_margin", "margin_contribution", "behinds",
  "inside_fifties", "intercepts", "contested_possessions", "hitouts",
  "goal_accuracy", "margin_impact_rating", "possession_rating",
  "scoring_rating", "defensive_rating", "ball_winning_rating",
  "efficiency_rating", "pvar_game", "replacement_rating", "home_rating_before",
  "away_rating_before", "home_rating_after", "away_rating_after",
  "expected_home_score", "total_team_rating", "min_player_rating",
  "max_player_rating", "players_above_7", "players_below_4", "win",
  "recent_wins", "score_for", "recent_avg_margin", "margin_trend",
  "performance_trend", "score_against", "cumulative_wins",
  "cumulative_points_for", "cumulative_points_against", "home_games",
  "home_wins", "away_games", "away_wins", "venue_clean", "day_of_week",
  "home_games_at_venue", "home_wins_at_venue", "avg_home_margin_venue",
  ".data", "h2h_games", "h2h_win_pct_home", "avg_h2h_margin_home",
  "avg_h2h_total", "impact_score", "actual_bin", "residual", "avg_metric",
  "kicks", "clangers", "total_score", "scoring_shots", "effective_disposals",
  "metres_gained", "kicking_efficiency", "score_per_disposal",
  "metres_per_disposal", "contested_percentage", "score_accuracy",
  "score_per_inside50", "intercept_rate", "tackle_efficiency",
  "top_3_avg_rating", "games", "recent_win_pct", "worst_player_rating"
))