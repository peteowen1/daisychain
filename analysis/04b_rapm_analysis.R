# 04b - RAPM (Regularized Adjusted Plus/Minus) Analysis
#
# This script implements RAPM similar to NBA analytics, where each player's
# coefficient represents their independent contribution to team margin,
# controlling for teammates and opponents.

library(tidyverse)
library(glmnet)
devtools::load_all()

cat("=== RAPM ANALYSIS ===\n\n")

# CACHE SETUP ----
cache_file <- "analysis/cache/04b_rapm_analysis.rds"

# Load from cache with fallback to previous scripts
load_with_fallback(
  object_name = "rapm_results",
  current_cache = cache_file,
  fallback_caches = list(
    enhanced_player_data = "analysis/cache/02_feature_engineering.rds",
    player_prediction_results = "analysis/cache/03_player_prediction_model.rds",
    player_analysis_results = "analysis/cache/04_player_analysis.rds"
  ),
  extract_from_fallback = list(
    enhanced_player_data = "enhanced_player_data",
    player_prediction_results = NULL,
    player_analysis_results = NULL
  )
)

# Ensure we have required data
if (!exists("enhanced_player_data")) {
  load_cache("enhanced_player_data", "analysis/cache/02_feature_engineering.rds",
             extract_name = "enhanced_player_data")
}

if (!exists("enhanced_player_data")) {
  stop("Please run 01_data_collection.R and 02_feature_engineering.R first")
}

# PHASE 1: PREPARE RAPM MATRIX ----

cat("Phase 1: Prepare RAPM Matrix\n")
cat("=============================\n")
cat("\nRAPM uses player time-on-ground to estimate individual contributions\n")
cat("to team margin, controlling for teammates and opponents.\n\n")

# Prepare RAPM matrix
rapm_data <- prepare_rapm_matrix(enhanced_player_data)

# PHASE 2: FIT RAPM MODEL ----

cat("\n\nPhase 2: Fit RAPM Model\n")
cat("=======================\n")

# Fit ridge regression model with cross-validation
rapm_model <- fit_rapm_model(rapm_data, nfolds = 10)

# PHASE 3: EXTRACT RAPM RATINGS ----

cat("\n\nPhase 3: Extract RAPM Ratings\n")
cat("==============================\n")

# Extract player ratings
rapm_ratings <- extract_rapm_ratings(rapm_model, rapm_data, lambda = "min")

# Display top performers
cat("\n--- Top 20 Players (Highest RAPM) ---\n")
print(rapm_ratings %>%
        slice_head(n = 20) %>%
        select(rank, player_name, team, n_games, latest_date, rapm_rating) %>%
        mutate(
          rapm_rating = round(rapm_rating, 2)
        ))

# Display bottom performers
cat("\n--- Bottom 20 Players (Lowest RAPM) ---\n")
print(rapm_ratings %>%
        slice_tail(n = 20) %>%
        arrange(rapm_rating) %>%
        select(rank, player_name, team, n_games, latest_date, rapm_rating) %>%
        mutate(
          rapm_rating = round(rapm_rating, 2)
        ))

# PHASE 4: TEAM-LEVEL AGGREGATION ----

cat("\n\nPhase 4: Team-Level Aggregation\n")
cat("================================\n")

# Aggregate to team level
team_rapm <- aggregate_rapm_by_team(rapm_ratings)

cat("\nTeam RAPM Summary:\n")
print(team_rapm %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))))

# PHASE 5: COMPARISON WITH PLAYER ANALYSIS ----

cat("\n\nPhase 5: Compare with Player Analysis (Script 04)\n")
cat("===================================================\n")

# Load player analysis results if available for comparison
if (exists("player_analysis_results")) {
  player_predictions <- player_analysis_results$regression_player_date$player_predictions

  # Merge RAPM with player predictions
  comparison <- rapm_ratings %>%
    select(player_id, player_name, team, rapm_rating) %>%
    inner_join(
      player_predictions %>%
        select(player_id, predicted_margin_contribution),
      by = "player_id"
    )

  # Calculate correlation
  cor_rapm_pred <- cor(comparison$rapm_rating,
                       comparison$predicted_margin_contribution,
                       use = "complete.obs")

  cat(sprintf("\nCorrelation between RAPM and Predicted Margin Contribution: %.3f\n",
              cor_rapm_pred))

  # Top 10 by each metric
  cat("\n--- Top 10 by RAPM vs Predicted Margin ---\n")

  top_rapm <- comparison %>%
    arrange(desc(rapm_rating)) %>%
    slice_head(n = 10) %>%
    pull(player_name)

  top_pred <- comparison %>%
    arrange(desc(predicted_margin_contribution)) %>%
    slice_head(n = 10) %>%
    pull(player_name)

  cat("\nTop 10 by RAPM:\n")
  cat(paste("  ", seq_along(top_rapm), top_rapm, collapse = "\n"), "\n")

  cat("\nTop 10 by Predicted Margin:\n")
  cat(paste("  ", seq_along(top_pred), top_pred, collapse = "\n"), "\n")

  # Players in both top 10
  overlap <- intersect(top_rapm, top_pred)
  cat(sprintf("\nPlayers in both top 10: %d\n", length(overlap)))
  if (length(overlap) > 0) {
    cat("  ", paste(overlap, collapse = ", "), "\n")
  }

} else {
  cat("\nPlayer analysis results not available for comparison.\n")
  cat("Run script 04 first to enable comparison.\n")
  comparison <- NULL
}

# COMPILE RESULTS ----

cat("\n\nPhase 6: Compile Results\n")
cat("========================\n")

rapm_results <- list(
  # RAPM data
  rapm_data = rapm_data,
  rapm_model = rapm_model,
  rapm_ratings = rapm_ratings,
  team_rapm = team_rapm,

  # Comparison
  comparison = comparison,

  # Metadata
  n_matches = length(unique(rapm_data$match_ids)),
  n_players = nrow(rapm_ratings),
  model_performance = list(
    r2 = rapm_model$r2,
    rmse = rapm_model$rmse,
    lambda_min = rapm_model$lambda_min
  )
)

# Save to environment
assign("rapm_results", rapm_results, envir = .GlobalEnv)

# SAVE TO CACHE ----
save_cache(rapm_results, cache_file)

# VISUALIZATIONS ----

library(ggplot2)

cat("\nGenerating visualizations...\n")

# 1. Top 25 Players by RAPM
plot_top_rapm <- ggplot(rapm_ratings %>% slice_head(n = 25),
                        aes(x = reorder(player_name, rapm_rating),
                            y = rapm_rating)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 25 Players by RAPM Rating",
       subtitle = "Points contribution per 100% time on ground",
       x = NULL, y = "RAPM Rating") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

print(plot_top_rapm)

# 2. Bottom 25 Players by RAPM
plot_bottom_rapm <- ggplot(rapm_ratings %>% slice_tail(n = 25) %>% arrange(rapm_rating),
                           aes(x = reorder(player_name, rapm_rating),
                               y = rapm_rating)) +
  geom_col(fill = "coral", alpha = 0.8) +
  coord_flip() +
  labs(title = "Bottom 25 Players by RAPM Rating",
       subtitle = "Points contribution per 100% time on ground",
       x = NULL, y = "RAPM Rating") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 9))

print(plot_bottom_rapm)

# 3. RAPM Distribution
plot_rapm_dist <- ggplot(rapm_ratings, aes(x = rapm_rating)) +
  geom_histogram(bins = 50, fill = "darkgreen", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Distribution of RAPM Ratings",
       subtitle = sprintf("Mean: %.2f, Median: %.2f",
                         mean(rapm_ratings$rapm_rating),
                         median(rapm_ratings$rapm_rating)),
       x = "RAPM Rating", y = "Count") +
  theme_minimal()

print(plot_rapm_dist)

# 4. Team RAPM
plot_team_rapm <- ggplot(team_rapm,
                         aes(x = reorder(team, total_rapm), y = total_rapm)) +
  geom_col(fill = "purple", alpha = 0.7) +
  coord_flip() +
  labs(title = "Team Strength by Total RAPM",
       subtitle = "Sum of all player RAPM ratings",
       x = NULL, y = "Total RAPM") +
  theme_minimal()

print(plot_team_rapm)

# 5. Comparison with Predicted Margin (if available)
if (!is.null(comparison)) {
  plot_comparison <- ggplot(comparison,
                           aes(x = predicted_margin_contribution, y = rapm_rating)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = "RAPM vs Predicted Margin Contribution",
         subtitle = sprintf("Correlation: %.3f", cor_rapm_pred),
         x = "Predicted Margin Contribution (Script 04)",
         y = "RAPM Rating") +
    theme_minimal()

  print(plot_comparison)

  # 6. Top players comparison scatter
  top_comparison <- comparison %>%
    filter(rank(desc(rapm_rating)) <= 30 |
           rank(desc(predicted_margin_contribution)) <= 30)

  plot_top_comparison <- ggplot(top_comparison,
                                aes(x = predicted_margin_contribution,
                                    y = rapm_rating,
                                    label = player_name)) +
    geom_point(color = "steelblue", size = 3, alpha = 0.6) +
    ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
    labs(title = "Top 30 Players: RAPM vs Predicted Margin",
         x = "Predicted Margin Contribution",
         y = "RAPM Rating") +
    theme_minimal()

  print(plot_top_comparison)
}

# SUMMARY ----

cat("\n=== RAPM ANALYSIS COMPLETE ===\n\n")

cat("RAPM Model Summary:\n")
cat(sprintf("  Matches analyzed:      %d\n", rapm_results$n_matches))
cat(sprintf("  Players rated:         %d\n", rapm_results$n_players))
cat(sprintf("  Model R²:              %.4f\n", rapm_results$model_performance$r2))
cat(sprintf("  Model RMSE:            %.2f points\n", rapm_results$model_performance$rmse))
cat(sprintf("  Optimal lambda:        %.6f\n", rapm_results$model_performance$lambda_min))

cat("\nTop 5 Players:\n")
print(rapm_ratings %>%
        slice_head(n = 5) %>%
        select(player_name, team, n_games, latest_date, rapm_rating) %>%
        mutate(rapm_rating = round(rapm_rating, 2)))

cat("\nResults saved to 'rapm_results'\n")
cat("Cache saved to:", cache_file, "\n")

plot(rapm_model$model)
rapm_model$model$lambda.min
