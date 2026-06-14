# AFLW Feature Engineering Analysis Script
library(tidyverse)
devtools::load_all()

# CACHE SETUP ----
cache_file <- "analysis/cache/02_feature_engineering.rds"

# Load from cache with fallback to previous scripts
load_with_fallback(
  object_name = "enhanced_player_data",
  current_cache = cache_file,
  extract_from_current = "enhanced_player_data",
  fallback_caches = list(
    merged_data = "analysis/cache/01_data_collection.rds",
    clean_results = "analysis/cache/01_data_collection.rds"
  ),
  extract_from_fallback = list(
    merged_data = "merged_data",
    clean_results = "clean_results"
  )
)

load_cache("enhanced_match_data", cache_file, extract_name = "enhanced_match_data")

# Ensure we have merged_data and clean_results
if (!exists("merged_data")) {
  load_cache("merged_data", "analysis/cache/01_data_collection.rds",
             extract_name = "merged_data")
}

if (!exists("clean_results")) {
  load_cache("clean_results", "analysis/cache/01_data_collection.rds",
             extract_name = "clean_results")
}

if (!exists("merged_data") || !exists("clean_results")) {
  stop("Please run 01_data_collection.R first to create the merged_data and clean_results objects")
}

# ROLLING STATISTICS ----

# Create rolling statistics for key player metrics
rolling_stats_data <- calculate_rolling_stats(
  player_data = merged_data,
  stats_cols = c("disposals", "goals", "marks", "tackles", "disposal_efficiency"),
  window = 5,
  min_games = 3
)

# Calculate advanced player impact metrics
impact_data <- calculate_player_impact(rolling_stats_data)

# TEAM METRICS ----

# Create team strength metrics (use clean_results directly - no duplicates)
team_strength_data <- calculate_team_strength(
  results_data = clean_results,
  window = 5
)

# Create match context features (use clean_results directly)
context_data <- create_match_context(
  match_data = clean_results
)

# Calculate head-to-head statistics (use clean_results directly)
h2h_data <- calculate_h2h_stats(
  results_data = clean_results,
  lookback_games = 10
)

# Create lag features for temporal modeling
lagged_team_data <- create_lag_features(
  data = team_strength_data,
  group_col = "team",
  feature_cols = c("win_percentage", "avg_margin", "recent_form", "avg_score_for"),
  lags = c(1, 3, 5)
)

# COMBINE FEATURES ----

# Combine features
enhanced_player_data <- impact_data %>%
  left_join(
    team_strength_data %>% select(team, date, win_percentage, avg_margin, recent_form),
    by = c("team", "date")
  )

enhanced_match_data <- h2h_data %>%
  left_join(
    context_data %>% select(match_id, round_type, season_pct, weekend_game, venue_advantage),
    by = "match_id"
  )

# Check feature completeness
feature_completeness <- check_data_completeness(
  enhanced_player_data,
  required_cols = c("player_id", "team", "disposals", "goals")
)

print(paste("Feature completeness:", feature_completeness$completeness_pct, "%"))

# VISUALIZATIONS ----

# Visualize key features
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Impact score distribution
  if ("impact_score" %in% names(enhanced_player_data)) {
    impact_plot <- ggplot(enhanced_player_data, aes(x = impact_score)) +
      geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
      labs(title = "Distribution of Player Impact Scores",
           x = "Impact Score", y = "Frequency") +
      theme_minimal()
    print(impact_plot)
  }

  # Team form over time
  if ("recent_form" %in% names(lagged_team_data)) {
    form_plot <- ggplot(lagged_team_data %>% filter(!is.na(recent_form)),
                        aes(x = date, y = recent_form, color = team)) +
      geom_line(alpha = 0.7) +
      labs(title = "Team Form Over Time", x = "Date", y = "Recent Form") +
      theme_minimal() +
      theme(legend.position = "none")
    print(form_plot)
  }
}

# SAVE TO CACHE ----
save_cache(
  list(
    enhanced_player_data = enhanced_player_data,
    enhanced_match_data = enhanced_match_data
  ),
  cache_file
)

# Enhanced datasets are now ready for player and team modeling
