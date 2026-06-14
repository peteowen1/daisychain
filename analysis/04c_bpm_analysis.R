# 04c - BPM (Box Plus/Minus) Analysis
#
# This script creates a Box Plus/Minus metric that translates RAPM ratings
# into a simple box score formula. BPM provides a quick player evaluation
# using only basic statistics, without requiring complex lineup data.

library(tidyverse)
library(glmnet)
devtools::load_all()

cat("=== BPM ANALYSIS ===\n\n")

# CACHE SETUP ----
cache_file <- "analysis/cache/04c_bpm_analysis.rds"

# Load from cache with fallback to previous scripts
load_with_fallback(
  object_name = "bpm_results",
  current_cache = cache_file,
  fallback_caches = list(
    enhanced_player_data = "analysis/cache/02_feature_engineering.rds",
    rapm_results = "analysis/cache/04b_rapm_analysis.rds"
  ),
  extract_from_fallback = list(
    enhanced_player_data = "enhanced_player_data",
    rapm_results = NULL
  )
)

# Ensure we have required data
if (!exists("enhanced_player_data")) {
  load_cache("enhanced_player_data", "analysis/cache/02_feature_engineering.rds",
             extract_name = "enhanced_player_data")
}

if (!exists("rapm_results")) {
  load_cache("rapm_results", "analysis/cache/04b_rapm_analysis.rds")
}

if (!exists("enhanced_player_data") || !exists("rapm_results")) {
  stop("Please run 01_data_collection.R, 02_feature_engineering.R, and 04b_rapm_analysis.R first")
}

# Extract RAPM ratings
rapm_ratings <- rapm_results$rapm_ratings

# PHASE 1: CALCULATE CAREER STATISTICS ----

cat("Phase 1: Calculate Career Statistics\n")
cat("======================================\n")
cat("\nCalculating TOG-weighted career averages for all players...\n\n")

# Define statistics to use in BPM
bpm_stats <- c(
  "disposals", "goals", "marks", "tackles", "hitouts",
  "contested_possessions", "uncontested_possessions",
  "effective_disposals", "metres_gained", "intercepts",
  "inside_fifties", "clangers", "frees_for", "frees_against"
)

# Calculate career statistics
career_stats <- calculate_player_career_stats(
  enhanced_player_data,
  stats_to_aggregate = bpm_stats
)

# Display sample
cat("\nSample career statistics:\n")
print(head(career_stats, 10))

# PHASE 2: PREPARE BPM REGRESSION DATA ----

cat("\n\nPhase 2: Prepare BPM Regression Data\n")
cat("======================================\n")

# Join career stats with RAPM ratings
bpm_regression_data <- prepare_bpm_regression_data(career_stats, rapm_ratings)

# Identify predictor columns
predictor_cols <- names(bpm_regression_data)[grepl("_avg$", names(bpm_regression_data))]
cat(sprintf("\nPredictor columns: %d\n", length(predictor_cols)))
print(gsub("_avg$", "", predictor_cols))

# PHASE 3: FIT BPM MODEL ----

cat("\n\nPhase 3: Fit BPM Model\n")
cat("=======================\n")

# Fit elastic net model (alpha=0.5)
bpm_model <- fit_bpm_model(
  bpm_regression_data,
  predictor_cols,
  alpha = 0.5,
  nfolds = 10
)

# Plot cross-validation curve
plot(bpm_model$model, main = "BPM Model: Cross-Validation Curve")

# PHASE 4: EXTRACT BPM FORMULA ----

cat("\n\nPhase 4: Extract BPM Formula\n")
cat("=============================\n")

# Extract BPM coefficients (the formula!)
bpm_coefficients <- extract_bpm_coefficients(bpm_model, predictor_cols, lambda = "min")

# PHASE 5: CALCULATE BPM RATINGS ----

cat("\n\nPhase 5: Calculate BPM Ratings\n")
cat("===============================\n")

# Apply BPM formula to all players
bpm_ratings <- calculate_bpm_ratings(career_stats, bpm_coefficients, predictor_cols)

# Display top performers
cat("\n--- Top 20 Players by BPM ---\n")
print(bpm_ratings %>%
        slice_head(n = 20) %>%
        select(bpm_rank, player_name, team, n_games, latest_date, bpm_rating) %>%
        mutate(bpm_rating = round(bpm_rating, 2)))

# Display bottom performers
cat("\n--- Bottom 20 Players by BPM ---\n")
print(bpm_ratings %>%
        slice_tail(n = 20) %>%
        arrange(bpm_rating) %>%
        select(bpm_rank, player_name, team, n_games, latest_date, bpm_rating) %>%
        mutate(bpm_rating = round(bpm_rating, 2)))

# 2025 TEAM BPM SUMMARY ----
cat("\n--- 2025 Team BPM Summary ---\n")
cat("(Sum of BPM for players active in 2025)\n\n")

team_bpm_2025 <- bpm_ratings %>%
  filter(year(latest_date) == 2025) %>%
  group_by(team) %>%
  summarise(
    n_players = n(),
    total_bpm = sum(bpm_rating, na.rm = TRUE),
    mean_bpm = mean(bpm_rating, na.rm = TRUE),
    median_bpm = median(bpm_rating, na.rm = TRUE),
    top_player = player_name[which.max(bpm_rating)],
    top_player_bpm = max(bpm_rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_bpm))

print(team_bpm_2025 %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))))

cat(sprintf("\nTotal teams with 2025 players: %d\n", nrow(team_bpm_2025)))

# PHASE 5b: GAME-LEVEL BPM PREDICTIONS ----

cat("\n\nPhase 5b: Game-Level BPM Predictions\n")
cat("=====================================\n")
cat("Applying BPM formula to individual player-games...\n\n")

# Calculate game-level BPM
game_level_bpm <- calculate_game_level_bpm(enhanced_player_data, bpm_coefficients)

# Opponent column comes from enhanced_player_data (created in data_processing)

# Create season interaction variables
game_level_bpm <- game_level_bpm %>%
  mutate(
    team_season = paste(team, year_season, sep = "_"),
    opponent_season = paste(opponent, year_season, sep = "_")
  )

# Display top individual game performances
cat("\n--- Top 20 Individual Game Performances ---\n")
top_games <- game_level_bpm %>%
  arrange(desc(bpm_game)) %>%
  slice_head(n = 20) %>%
  select(
    # Context
    player_name, team, opponent, date, year_season,
    team_season, opponent_season,
    # BPM rating
    bpm_game,
    # All predictor stats (the stats that created the BPM)
    all_of(gsub("_avg$", "", predictor_cols))
  )

print(top_games %>%
        mutate(bpm_game = round(bpm_game, 2)))

# Display worst individual games
cat("\n--- Bottom 20 Individual Game Performances ---\n")
bottom_games <- game_level_bpm %>%
  arrange(bpm_game) %>%
  slice_head(n = 20) %>%
  select(
    # Context
    player_name, team, opponent, date, year_season,
    team_season, opponent_season,
    # BPM rating
    bpm_game,
    # All predictor stats (the stats that created the BPM)
    all_of(gsub("_avg$", "", predictor_cols))
  )

print(bottom_games %>%
        mutate(bpm_game = round(bpm_game, 2)))

# Calculate player consistency
player_consistency <- calculate_player_consistency(game_level_bpm)

# Most consistent players (low standard deviation, min 10 games)
cat("\n--- Most Consistent Players (Low BPM Variance) ---\n")
cat("(Minimum 10 games)\n\n")
most_consistent <- player_consistency %>%
  filter(n_games >= 10) %>%
  arrange(sd_bpm) %>%
  slice_head(n = 15)

print(most_consistent %>%
        select(player_name, team, n_games, mean_bpm, sd_bpm) %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))))

# Most volatile players (high standard deviation, min 10 games)
cat("\n--- Most Volatile Players (High BPM Variance) ---\n")
cat("(Minimum 10 games)\n\n")
most_volatile <- player_consistency %>%
  filter(n_games >= 10) %>%
  arrange(desc(sd_bpm)) %>%
  slice_head(n = 15)

print(most_volatile %>%
        select(player_name, team, n_games, mean_bpm, sd_bpm, range_bpm) %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))))

# Validation: Compare career BPM vs game-level mean
cat("\n--- Validation: Career BPM vs Game-Level Mean BPM ---\n")
validation <- bpm_ratings %>%
  select(player_id, player_name, bpm_rating) %>%
  inner_join(
    player_consistency %>% select(player_id, mean_bpm, n_games),
    by = "player_id"
  ) %>%
  filter(n_games >= 5) %>%
  mutate(difference = abs(bpm_rating - mean_bpm))

cat(sprintf("Correlation: %.4f\n", cor(validation$bpm_rating, validation$mean_bpm)))
cat(sprintf("Mean absolute difference: %.4f\n", mean(validation$difference)))
cat("(Should be very close if implementation is correct)\n")

# PHASE 6: COMPARE BPM WITH RAPM ----

cat("\n\nPhase 6: Compare BPM with RAPM\n")
cat("================================\n")

# Compare BPM and RAPM
comparison <- compare_bpm_rapm(bpm_ratings, rapm_ratings)

# Display correlations by games played
cat("\nCorrelation by experience level:\n")

experience_groups <- comparison %>%
  mutate(
    experience = case_when(
      n_games < 10 ~ "Low (< 10 games)",
      n_games < 30 ~ "Medium (10-29 games)",
      TRUE ~ "High (30+ games)"
    )
  )

for (exp_level in c("Low (< 10 games)", "Medium (10-29 games)", "High (30+ games)")) {
  subset_data <- experience_groups %>% filter(experience == exp_level)
  if (nrow(subset_data) > 0) {
    cor_val <- cor(subset_data$bpm_rating, subset_data$rapm_rating, use = "complete.obs")
    cat(sprintf("  %s: r = %.3f (n = %d)\n", exp_level, cor_val, nrow(subset_data)))
  }
}

# COMPILE RESULTS ----

cat("\n\nPhase 7: Compile Results\n")
cat("=========================\n")

bpm_results <- list(
  # Career stats
  career_stats = career_stats,

  # BPM model
  bpm_regression_data = bpm_regression_data,
  bpm_model = bpm_model,
  bpm_coefficients = bpm_coefficients,
  bpm_ratings = bpm_ratings,

  # Team summaries
  team_bpm_2025 = team_bpm_2025,

  # Game-level analysis
  game_level_bpm = game_level_bpm,
  player_consistency = player_consistency,
  top_games = top_games,
  bottom_games = bottom_games,

  # Comparison
  comparison = comparison,

  # Metadata
  predictor_cols = predictor_cols,
  n_players = nrow(bpm_ratings),
  model_performance = list(
    r2 = bpm_model$r2,
    rmse = bpm_model$rmse,
    correlation = cor(bpm_model$y_actual, bpm_model$predictions),
    lambda_min = bpm_model$lambda_min
  )
)

# Save to environment
assign("bpm_results", bpm_results, envir = .GlobalEnv)

# SAVE TO CACHE ----
save_cache(bpm_results, cache_file)

# VISUALIZATIONS ----

library(ggplot2)

cat("\nGenerating visualizations...\n")

# 1. BPM Formula Coefficients
plot_bpm_formula <- ggplot(
  bpm_coefficients$coefficients %>% filter(coefficient != 0),
  aes(x = reorder(statistic, coefficient), y = coefficient)
) +
  geom_col(aes(fill = coefficient > 0), alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("coral", "steelblue"), guide = "none") +
  labs(title = "BPM Formula: Statistic Weights",
       subtitle = "Contribution of each box score stat to BPM rating",
       x = NULL, y = "Coefficient") +
  theme_minimal()

print(plot_bpm_formula)

# 2. Top 25 Players by BPM
plot_top_bpm <- ggplot(bpm_ratings %>% slice_head(n = 25),
                       aes(x = reorder(player_name, bpm_rating), y = bpm_rating)) +
  geom_col(fill = "darkgreen", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 25 Players by BPM Rating",
       x = NULL, y = "BPM Rating") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

print(plot_top_bpm)

# 3. 2025 Team BPM Strength
plot_team_bpm_2025 <- ggplot(team_bpm_2025,
                             aes(x = reorder(team, total_bpm), y = total_bpm)) +
  geom_col(fill = "purple", alpha = 0.8) +
  coord_flip() +
  labs(title = "2025 Team Strength by Total BPM",
       subtitle = "Sum of BPM ratings for all players active in 2025",
       x = NULL, y = "Total BPM") +
  theme_minimal()

print(plot_team_bpm_2025)

# 4. BPM vs RAPM Scatter Plot
plot_bpm_rapm <- ggplot(comparison, aes(x = rapm_rating, y = bpm_rating)) +
  geom_point(aes(size = n_games, alpha = 0.6), color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "BPM vs RAPM Ratings",
       subtitle = sprintf("Correlation: %.3f | R²: %.3f",
                         cor(comparison$bpm_rating, comparison$rapm_rating),
                         bpm_model$r2),
       x = "RAPM Rating (True Impact)",
       y = "BPM Rating (Box Score Estimate)",
       size = "Games Played") +
  theme_minimal()

print(plot_bpm_rapm)

# 5. BPM vs RAPM: Top Players
top_comparison <- comparison %>%
  filter(bpm_rank <= 30 | rapm_rank <= 30)

plot_top_comparison <- ggplot(top_comparison,
                              aes(x = rapm_rating, y = bpm_rating, label = player_name)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.6) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 25) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Top 30 Players: BPM vs RAPM",
       subtitle = "Players above line: BPM overestimates | Below line: BPM underestimates",
       x = "RAPM Rating", y = "BPM Rating") +
  theme_minimal()

print(plot_top_comparison)

# 6. Rating Differences (Residuals)
plot_differences <- ggplot(comparison, aes(x = n_games, y = rating_diff)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = TRUE, color = "darkgreen") +
  labs(title = "BPM - RAPM Difference by Experience",
       subtitle = "Positive = BPM overestimates, Negative = BPM underestimates",
       x = "Games Played", y = "BPM - RAPM Difference") +
  theme_minimal()

print(plot_differences)

# 7. Actual vs Predicted in Training Set
plot_fit <- ggplot(data.frame(
  actual = bpm_model$y_actual,
  predicted = bpm_model$predictions
), aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "BPM Model: Actual vs Predicted RAPM",
       subtitle = sprintf("Training Set | R²: %.3f | RMSE: %.2f",
                         bpm_model$r2, bpm_model$rmse),
       x = "Actual RAPM Rating", y = "BPM Predicted Rating") +
  theme_minimal()

print(plot_fit)

# GAME-LEVEL VISUALIZATIONS ----

cat("\nGenerating game-level visualizations...\n")

# 8. Game-Level BPM Distribution
plot_game_bpm_dist <- ggplot(game_level_bpm, aes(x = bpm_game)) +
  geom_histogram(bins = 60, fill = "darkgreen", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Distribution of Game-Level BPM Ratings",
       subtitle = sprintf("All %d player-games", nrow(game_level_bpm)),
       x = "BPM Rating (Individual Game)", y = "Count") +
  theme_minimal()

print(plot_game_bpm_dist)

# 9. Player Consistency: SD vs Mean BPM
plot_consistency <- player_consistency %>%
  filter(n_games >= 10) %>%
  ggplot(aes(x = mean_bpm, y = sd_bpm)) +
  geom_point(aes(size = n_games), alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Player Consistency: Standard Deviation vs Mean BPM",
       subtitle = "Players with 10+ games | Larger points = more games",
       x = "Mean BPM", y = "Standard Deviation (Consistency)",
       size = "Games") +
  theme_minimal()

print(plot_consistency)

# 10. BPM Over Time for Top 5 Players
top_5_players <- bpm_ratings %>%
  slice_head(n = 5) %>%
  pull(player_id)

plot_bpm_trends <- game_level_bpm %>%
  filter(player_id %in% top_5_players) %>%
  ggplot(aes(x = date, y = bpm_game, color = player_name)) +
  geom_line(alpha = 0.7) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  labs(title = "BPM Trends Over Time: Top 5 Players",
       subtitle = "Game-by-game performance with trend line",
       x = "Date", y = "Game BPM", color = "Player") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot_bpm_trends)

# 11. Player Consistency Box Plot (Top 20 by Career BPM)
top_20_players <- bpm_ratings %>%
  slice_head(n = 20) %>%
  pull(player_id)

plot_bpm_boxes <- game_level_bpm %>%
  filter(player_id %in% top_20_players) %>%
  inner_join(bpm_ratings %>% select(player_id, bpm_rank), by = "player_id") %>%
  ggplot(aes(x = reorder(player_name, bpm_rank), y = bpm_game)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  coord_flip() +
  labs(title = "Game-Level BPM Variance: Top 20 Players",
       subtitle = "Box plot shows consistency of performance",
       x = NULL, y = "Game BPM") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

print(plot_bpm_boxes)

# SUMMARY ----

cat("\n=== BPM ANALYSIS COMPLETE ===\n\n")

cat("BPM Model Summary:\n")
cat(sprintf("  Players analyzed:      %d\n", bpm_results$n_players))
cat(sprintf("  Predictors used:       %d\n", length(predictor_cols)))
cat(sprintf("  Non-zero coefficients: %d\n",
            sum(bpm_coefficients$coefficients$coefficient != 0)))
cat(sprintf("  Model R²:              %.4f\n", bpm_results$model_performance$r2))
cat(sprintf("  Model RMSE:            %.2f\n", bpm_results$model_performance$rmse))
cat(sprintf("  BPM-RAPM Correlation:  %.4f\n", bpm_results$model_performance$correlation))

cat("\nTop 5 Players by BPM:\n")
print(bpm_ratings %>%
        slice_head(n = 5) %>%
        select(player_name, team, n_games, bpm_rating) %>%
        mutate(bpm_rating = round(bpm_rating, 2)))

cat("\nBPM Formula (Top 5 Most Important Stats):\n")
print(bpm_coefficients$coefficients %>%
        slice_head(n = 5) %>%
        select(statistic, coefficient) %>%
        mutate(coefficient = round(coefficient, 4)))

cat("\nTop 3 Teams by 2025 BPM:\n")
print(team_bpm_2025 %>%
        slice_head(n = 3) %>%
        select(team, n_players, total_bpm, top_player) %>%
        mutate(total_bpm = round(total_bpm, 2)))

cat("\nGame-Level BPM Summary:\n")
cat(sprintf("  Total player-games:    %d\n", nrow(game_level_bpm)))
cat(sprintf("  Best game BPM:         %.2f (%s on %s)\n",
            top_games$bpm_game[1], top_games$player_name[1], top_games$date[1]))
cat(sprintf("  Worst game BPM:        %.2f (%s on %s)\n",
            bottom_games$bpm_game[1], bottom_games$player_name[1], bottom_games$date[1]))
cat(sprintf("  Most consistent:       %s (SD: %.2f)\n",
            most_consistent$player_name[1], most_consistent$sd_bpm[1]))
cat(sprintf("  Most volatile:         %s (SD: %.2f)\n",
            most_volatile$player_name[1], most_volatile$sd_bpm[1]))

cat("\nResults saved to 'bpm_results'\n")
cat("  - bpm_ratings: Career-level BPM for all players\n")
cat("  - game_level_bpm: Game-by-game BPM ratings\n")
cat("  - player_consistency: Variance metrics\n")
cat("Cache saved to:", cache_file, "\n")
