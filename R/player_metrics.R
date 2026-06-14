#' Calculate Player Contribution to Team Margin
#'
#' Calculate how much each player's performance contributes to their team's margin.
#' This uses a weighted combination of statistics to estimate impact on winning.
#'
#' @param player_data Data frame with player statistics and match results
#' @param margin_col Column name containing match margin (default: "margin")
#' @param weight_goals Weight for goals in margin contribution (default: 0.4)
#' @param weight_disposals Weight for disposals in margin contribution (default: 0.2)
#' @param weight_marks Weight for marks in margin contribution (default: 0.15)
#' @param weight_tackles Weight for tackles in margin contribution (default: 0.15)
#' @param weight_efficiency Weight for disposal efficiency in margin contribution (default: 0.1)
#' @return Data frame with player margin contribution metrics
#' @export
#' @examples
#' \dontrun{
#' player_margins <- calculate_player_margin_contribution(merged_data)
#' }
calculate_player_margin_contribution <- function(player_data,
                                                 margin_col = "margin",
                                                 weight_goals = 0.4,
                                                 weight_disposals = 0.2,
                                                 weight_marks = 0.15,
                                                 weight_tackles = 0.15,
                                                 weight_efficiency = 0.1) {

  # Ensure required columns exist
  required_cols <- c("player_id", "team", margin_col, "goals", "disposals", "marks", "tackles")
  missing_cols <- required_cols[!required_cols %in% names(player_data)]
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Calculate standardized statistics within each match for relative comparison
  margin_data <- player_data %>%
    dplyr::group_by(match_id) %>%
    dplyr::mutate(
      # Standardize statistics within each match (z-scores)
      goals_z = scale(goals)[,1],
      disposals_z = scale(disposals)[,1],
      marks_z = scale(marks)[,1],
      tackles_z = scale(tackles)[,1],
      disposal_efficiency_z = scale(disposal_efficiency)[,1],

      # Handle NAs from standardization (when all players have same value)
      goals_z = ifelse(is.na(goals_z), 0, goals_z),
      disposals_z = ifelse(is.na(disposals_z), 0, disposals_z),
      marks_z = ifelse(is.na(marks_z), 0, marks_z),
      tackles_z = ifelse(is.na(tackles_z), 0, tackles_z),
      disposal_efficiency_z = ifelse(is.na(disposal_efficiency_z), 0, disposal_efficiency_z)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Calculate weighted contribution score
      contribution_score = (
        weight_goals * goals_z +
        weight_disposals * disposals_z +
        weight_marks * marks_z +
        weight_tackles * tackles_z +
        weight_efficiency * disposal_efficiency_z
      ),

      # Adjust contribution based on team result and home/away
      team_margin = ifelse(home_away == "Home", !!rlang::sym(margin_col), -!!rlang::sym(margin_col)),

      # Player's estimated contribution to margin
      margin_contribution = contribution_score * abs(team_margin) / 10,  # Scale factor

      # Positive/negative based on team performance
      signed_margin_contribution = ifelse(team_margin > 0,
                                         margin_contribution,
                                         -margin_contribution)
    )

  return(margin_data)
}

#' Calculate Player Rating System
#'
#' Create a comprehensive player rating system based on multiple performance metrics.
#'
#' @param player_data Data frame with player statistics
#' @param include_margin_impact Logical, whether to include margin contribution in rating
#' @return Data frame with player ratings
#' @export
#' @examples
#' \dontrun{
#' player_ratings <- calculate_player_ratings(player_data)
#' }
calculate_player_ratings <- function(player_data, include_margin_impact = TRUE) {

  # Calculate base ratings for different aspects of play
  rating_data <- player_data %>%
    dplyr::mutate(
      # Possession rating (0-10 scale)
      possession_rating = pmin(10, pmax(0,
        2 + (disposals - 15) * 0.3 + (disposal_efficiency - 70) * 0.05
      )),

      # Scoring rating (0-10 scale)
      scoring_rating = pmin(10, pmax(0,
        goals * 2 + behinds * 0.5 + inside50s * 0.2
      )),

      # Defensive rating (0-10 scale)
      defensive_rating = pmin(10, pmax(0,
        tackles * 0.4 + intercepts * 0.3 + (contested_possessions / disposals * 10)
      )),

      # Ball winning rating (0-10 scale)
      ball_winning_rating = pmin(10, pmax(0,
        contested_possessions * 0.3 + marks * 0.4 + hitouts * 0.1
      )),

      # Efficiency rating (0-10 scale)
      efficiency_rating = pmin(10, pmax(0,
        (disposal_efficiency - 50) * 0.1 + (goal_accuracy - 30) * 0.05
      ))
    )

  # Calculate composite rating
  if (include_margin_impact && "signed_margin_contribution" %in% names(rating_data)) {
    # Include margin impact in overall rating
    rating_data <- rating_data %>%
      dplyr::group_by(player_id) %>%
      dplyr::mutate(
        avg_margin_contribution = mean(signed_margin_contribution, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        margin_impact_rating = pmin(10, pmax(0, 5 + avg_margin_contribution * 0.5)),

        # Overall rating (weighted average)
        overall_rating = (
          0.25 * possession_rating +
          0.20 * scoring_rating +
          0.20 * defensive_rating +
          0.15 * ball_winning_rating +
          0.10 * efficiency_rating +
          0.10 * margin_impact_rating
        )
      )
  } else {
    # Overall rating without margin impact
    rating_data <- rating_data %>%
      dplyr::mutate(
        overall_rating = (
          0.30 * possession_rating +
          0.25 * scoring_rating +
          0.20 * defensive_rating +
          0.15 * ball_winning_rating +
          0.10 * efficiency_rating
        )
      )
  }

  return(rating_data)
}

#' Calculate Player Form Metrics
#'
#' Calculate recent form and consistency metrics for players.
#'
#' @param player_data Data frame with player statistics sorted by date
#' @param form_window Number of recent games for form calculation (default: 5)
#' @param consistency_window Number of games for consistency calculation (default: 10)
#' @return Data frame with form metrics added
#' @export
#' @examples
#' \dontrun{
#' player_form <- calculate_player_form(player_data)
#' }
calculate_player_form <- function(player_data,
                                  form_window = 5,
                                  consistency_window = 10) {

  form_data <- player_data %>%
    dplyr::arrange(player_id, date) %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(
      games_played = dplyr::row_number(),

      # Recent form (rolling averages)
      recent_rating = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(overall_rating, mean, na.rm = TRUE,
                          .before = form_window - 1, .complete = FALSE)
      } else {
        as.numeric(stats::filter(overall_rating, rep(1/form_window, form_window), sides = 1))
      },

      recent_goals = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(goals, mean, na.rm = TRUE,
                          .before = form_window - 1, .complete = FALSE)
      } else {
        as.numeric(stats::filter(goals, rep(1/form_window, form_window), sides = 1))
      },

      recent_disposals = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(disposals, mean, na.rm = TRUE,
                          .before = form_window - 1, .complete = FALSE)
      } else {
        as.numeric(stats::filter(disposals, rep(1/form_window, form_window), sides = 1))
      },

      recent_efficiency = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(disposal_efficiency, mean, na.rm = TRUE,
                          .before = form_window - 1, .complete = FALSE)
      } else {
        as.numeric(stats::filter(disposal_efficiency, rep(1/form_window, form_window), sides = 1))
      },

      # Consistency metrics (standard deviations over longer period)
      rating_consistency = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(overall_rating, sd, na.rm = TRUE,
                          .before = consistency_window - 1, .complete = FALSE)
      } else {
        NA_real_  # Fallback - would need more complex calculation
      },

      performance_consistency = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(impact_score, sd, na.rm = TRUE,
                          .before = consistency_window - 1, .complete = FALSE)
      } else {
        NA_real_  # Fallback - would need more complex calculation
      },

      # Trend indicators (slope of recent performances)
      rating_trend = if (requireNamespace("slider", quietly = TRUE)) {
        slider::slide_dbl(seq_along(overall_rating), function(x) {
          if (length(x) < 3) return(0)
          y <- overall_rating[x]
          if (all(is.na(y))) return(0)
          lm_result <- tryCatch(
            stats::lm(y ~ x),
            error = function(e) NULL
          )
          if (is.null(lm_result)) return(0)
          return(stats::coef(lm_result)[2])
        }, .before = form_window - 1, .complete = FALSE)
      } else {
        0  # Fallback - no trend calculation without slider
      }
    ) %>%
    dplyr::mutate(
      # Form categories
      form_category = dplyr::case_when(
        games_played < form_window ~ "Insufficient Data",
        recent_rating >= 7.5 ~ "Excellent Form",
        recent_rating >= 6.5 ~ "Good Form",
        recent_rating >= 5.5 ~ "Average Form",
        recent_rating >= 4.5 ~ "Poor Form",
        TRUE ~ "Very Poor Form"
      ),

      # Consistency categories
      consistency_category = dplyr::case_when(
        games_played < consistency_window ~ "Insufficient Data",
        rating_consistency <= 1.0 ~ "Very Consistent",
        rating_consistency <= 1.5 ~ "Consistent",
        rating_consistency <= 2.0 ~ "Moderate",
        rating_consistency <= 2.5 ~ "Inconsistent",
        TRUE ~ "Very Inconsistent"
      )
    ) %>%
    dplyr::ungroup()

  return(form_data)
}

#' Calculate Player Value Above Replacement (PVAR)
#'
#' Calculate a player's value compared to a replacement-level player.
#'
#' @param player_data Data frame with player statistics and ratings
#' @param position_col Column containing player positions (optional)
#' @return Data frame with PVAR metrics
#' @export
#' @examples
#' \dontrun{
#' player_pvar <- calculate_pvar(player_data)
#' }
calculate_pvar <- function(player_data, position_col = NULL) {

  # Define replacement level as 25th percentile of regular players
  # (players with at least 50% of average games)
  min_games_threshold <- quantile(
    table(player_data$player_id), 0.5, na.rm = TRUE
  )

  regular_players <- player_data %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(
      games = dplyr::n(),
      avg_rating = mean(overall_rating, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(games >= min_games_threshold)

  # Calculate replacement level
  if (!is.null(position_col) && position_col %in% names(player_data)) {
    # Position-specific replacement levels
    replacement_levels <- player_data %>%
      dplyr::filter(player_id %in% regular_players$player_id) %>%
      dplyr::group_by(!!rlang::sym(position_col)) %>%
      dplyr::summarise(
        replacement_rating = quantile(overall_rating, 0.25, na.rm = TRUE),
        .groups = "drop"
      )

    pvar_data <- player_data %>%
      dplyr::left_join(replacement_levels, by = position_col) %>%
      dplyr::mutate(
        replacement_rating = ifelse(is.na(replacement_rating),
                                   quantile(regular_players$avg_rating, 0.25, na.rm = TRUE),
                                   replacement_rating)
      )
  } else {
    # Overall replacement level
    overall_replacement <- quantile(regular_players$avg_rating, 0.25, na.rm = TRUE)
    pvar_data <- player_data %>%
      dplyr::mutate(replacement_rating = overall_replacement)
  }

  # Calculate PVAR
  pvar_data <- pvar_data %>%
    dplyr::mutate(
      pvar_game = overall_rating - replacement_rating,
      pvar_per_game = pvar_game
    ) %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(
      cumulative_pvar = cumsum(pvar_game),
      avg_pvar_per_game = mean(pvar_game, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      pvar_category = dplyr::case_when(
        avg_pvar_per_game >= 2.0 ~ "Elite",
        avg_pvar_per_game >= 1.0 ~ "Very Good",
        avg_pvar_per_game >= 0.5 ~ "Above Average",
        avg_pvar_per_game >= -0.5 ~ "Average",
        avg_pvar_per_game >= -1.0 ~ "Below Average",
        TRUE ~ "Poor"
      )
    )

  return(pvar_data)
}

#' Identify Top Performers by Metric
#'
#' Identify top performing players across different metrics.
#'
#' @param player_data Data frame with player ratings and statistics
#' @param metric Column name to rank by (default: "overall_rating")
#' @param min_games Minimum games played to be eligible (default: 5)
#' @param top_n Number of top players to return (default: 20)
#' @param season_filter Optional season to filter by
#' @return Data frame with top performers
#' @export
#' @examples
#' \dontrun{
#' top_players <- identify_top_performers(player_data, "overall_rating")
#' margin_leaders <- identify_top_performers(player_data, "avg_margin_contribution")
#' }
identify_top_performers <- function(player_data,
                                    metric = "overall_rating",
                                    min_games = 5,
                                    top_n = 20,
                                    season_filter = NULL) {

  # Filter by season if specified
  if (!is.null(season_filter)) {
    player_data <- player_data %>%
      dplyr::filter(season %in% season_filter)
  }

  # Calculate player summaries
  top_performers <- player_data %>%
    dplyr::group_by(player_id, player_name, team) %>%
    dplyr::summarise(
      games_played = dplyr::n(),
      avg_metric = mean(!!rlang::sym(metric), na.rm = TRUE),
      total_metric = sum(!!rlang::sym(metric), na.rm = TRUE),
      best_game = max(!!rlang::sym(metric), na.rm = TRUE),
      worst_game = min(!!rlang::sym(metric), na.rm = TRUE),
      consistency = sd(!!rlang::sym(metric), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(games_played >= min_games) %>%
    dplyr::arrange(dplyr::desc(avg_metric)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(
      rank = dplyr::row_number(),
      metric_name = metric
    )

  return(top_performers)
}

#' Calculate Team Impact Distribution
#'
#' Calculate how player ratings are distributed within teams.
#'
#' @param player_data Data frame with player ratings
#' @param season_filter Optional season filter
#' @return Data frame with team impact distribution
#' @export
#' @examples
#' \dontrun{
#' team_impact <- calculate_team_impact_distribution(player_data)
#' }
calculate_team_impact_distribution <- function(player_data, season_filter = NULL) {

  if (!is.null(season_filter)) {
    player_data <- player_data %>%
      dplyr::filter(season %in% season_filter)
  }

  team_impact <- player_data %>%
    dplyr::group_by(team, match_id) %>%
    dplyr::summarise(
      total_team_rating = sum(overall_rating, na.rm = TRUE),
      avg_team_rating = mean(overall_rating, na.rm = TRUE),
      max_player_rating = max(overall_rating, na.rm = TRUE),
      min_player_rating = min(overall_rating, na.rm = TRUE),
      rating_range = max_player_rating - min_player_rating,
      rating_sd = sd(overall_rating, na.rm = TRUE),
      players_above_7 = sum(overall_rating >= 7, na.rm = TRUE),
      players_below_4 = sum(overall_rating <= 4, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      games = dplyr::n(),
      avg_total_rating = mean(total_team_rating, na.rm = TRUE),
      avg_team_rating = mean(avg_team_rating, na.rm = TRUE),
      consistency = mean(rating_sd, na.rm = TRUE),
      depth_score = mean(min_player_rating, na.rm = TRUE),  # How good is worst player
      star_power = mean(max_player_rating, na.rm = TRUE),   # How good is best player
      avg_players_above_7 = mean(players_above_7, na.rm = TRUE),
      avg_players_below_4 = mean(players_below_4, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(avg_team_rating))

  return(team_impact)
}
