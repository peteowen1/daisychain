# AFLW Data Collection Analysis Script
library(tidyverse)
devtools::load_all()

# CACHE SETUP ----
cache_file <- "analysis/cache/01_data_collection.rds"

# Load from cache if available
load_cache("merged_data", cache_file, extract_name = "merged_data")
load_cache("clean_results", cache_file, extract_name = "clean_results")
load_cache("clean_player_stats", cache_file, extract_name = "clean_player_stats")

# DATA COLLECTION ----

# Define seasons to analyze
seasons_to_collect <- 2017:2025

# Collect comprehensive AFLW data
aflw_comprehensive <- scrape_aflw_comprehensive(
  seasons = seasons_to_collect,
  include_lineups = FALSE  # Set to TRUE if you want lineup data
)

# Process and clean the data
clean_results <- process_aflw_results(aflw_comprehensive$results)
clean_player_stats <- process_aflw_player_stats(aflw_comprehensive$player_stats)
merged_data <- merge_results_stats(clean_results, clean_player_stats)

# DATA VALIDATION AND SUMMARY ----

# Validate data quality
validation_results <- validate_aflw_data(merged_data, type = "merged")
print(validation_results$summary)

# Basic dataset overview
season_summary <- merged_data %>%
  group_by(season) %>%
  summarise(
    matches = n_distinct(match_id),
    teams = n_distinct(team),
    players = n_distinct(player_id),
    .groups = "drop"
  )

print(season_summary)

# VISUALIZATIONS ----

# Quick visualizations (optional)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Goals distribution
  goals_plot <- ggplot(merged_data, aes(x = goals)) +
    geom_histogram(bins = 10, fill = "steelblue", alpha = 0.7) +
    labs(title = "Distribution of Goals per Game", x = "Goals", y = "Frequency") +
    theme_minimal()

  # Disposals by team result
  disposals_plot <- ggplot(merged_data, aes(x = team_result, y = disposals, fill = team_result)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "Disposals Distribution by Match Result", x = "Match Result", y = "Disposals") +
    theme_minimal() +
    theme(legend.position = "none")

  print(goals_plot)
  print(disposals_plot)
}

# SAVE TO CACHE ----
save_cache(
  list(
    merged_data = merged_data,
    clean_results = clean_results,
    clean_player_stats = clean_player_stats
  ),
  cache_file
)

# Data is now ready for feature engineering and modeling
# merged_data object contains the cleaned, validated dataset
