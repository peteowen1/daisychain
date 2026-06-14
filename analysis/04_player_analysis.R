# 04 - AFLW Player Analysis Script
library(tidyverse)
devtools::load_all()

# CACHE SETUP ----
cache_file <- "analysis/cache/04_player_analysis.rds"

# Load from cache with fallback to previous scripts
load_with_fallback(
  object_name = "player_analysis_results",
  current_cache = cache_file,
  fallback_caches = list(
    enhanced_player_data = "analysis/cache/02_feature_engineering.rds",
    player_prediction_results = "analysis/cache/03_player_prediction_model.rds"
  ),
  extract_from_fallback = list(
    enhanced_player_data = "enhanced_player_data",
    player_prediction_results = NULL  # Load entire object
  )
)

# Ensure we have required data
if (!exists("enhanced_player_data")) {
  load_cache("enhanced_player_data", "analysis/cache/02_feature_engineering.rds",
             extract_name = "enhanced_player_data")
}

if (!exists("player_prediction_results")) {
  load_cache("player_prediction_results", "analysis/cache/03_player_prediction_model.rds")
}

if (!exists("enhanced_player_data") || !exists("player_prediction_results")) {
  stop("Please run previous scripts: 01_data_collection.R, 02_feature_engineering.R, and 03_player_prediction_model.R")
}

cat("=== PLAYER ANALYSIS ===\n\n")

# REGRESSION ANALYSIS ----

cat("Phase 1: Regression Analysis\n")
cat("============================\n")

# Extract ALL predicted statistics with dates (not just latest)
predicted_stats <- player_prediction_results$all_predictions_with_rookies

# Available predicted statistics (exclude time_on_ground from regression)
pred_stat_cols <- names(predicted_stats)[grepl("_predicted$", names(predicted_stats))]
pred_stat_cols <- pred_stat_cols[pred_stat_cols != "time_on_ground_percentage_predicted"]
cat("Available predicted statistics:", length(pred_stat_cols), "\n")
print(gsub("_predicted", "", pred_stat_cols))

# Prepare game margins
game_margins <- prepare_game_margins(enhanced_player_data)
cat(sprintf("\nGame margins available: %d team-date combinations\n", nrow(game_margins)))

# REGRESSION 1: TEAM-DATE LEVEL ----
cat("\n--- Regression 1: Team-Date Level ---\n")

# Prepare regression data
team_date_regression_data <- prepare_team_date_regression(
  predicted_stats, game_margins, pred_stat_cols
)
cat(sprintf("Team-date regression data: %d team-games\n", nrow(team_date_regression_data)))

# Fit ridge regression model
team_model <- fit_margin_ridge_regression(team_date_regression_data, pred_stat_cols, nfolds = 10)

cat("\nTeam-Date Level Weights:\n")
print(team_model$weights_df)

cat(sprintf("\nTeam-Date Model Performance: R² = %.3f, RMSE = %.2f points\n",
            team_model$r2, team_model$rmse))

# Extract for later use
cv_model_team <- team_model$model
stat_weights_team <- team_model$weights
stat_weights_team_df <- team_model$weights_df
r2_team <- team_model$r2
rmse_team <- team_model$rmse

# REGRESSION 2: PLAYER-DATE LEVEL ----
cat("\n--- Regression 2: Player-Date Level ---\n")

# Prepare regression data
player_date_regression_data <- prepare_player_date_regression(
  predicted_stats, game_margins, pred_stat_cols
)
cat(sprintf("Player-date regression data: %d player-games\n", nrow(player_date_regression_data)))

# Fit ridge regression model
player_model <- fit_margin_ridge_regression(player_date_regression_data, pred_stat_cols, nfolds = 10)

cat("\nPlayer-Date Level Weights:\n")
print(player_model$weights_df)

cat(sprintf("\nPlayer-Date Model Performance: R² = %.3f, RMSE = %.2f points\n",
            player_model$r2, player_model$rmse))

# Extract for later use
cv_model_player <- player_model$model
stat_weights_player <- player_model$weights
stat_weights_player_df <- player_model$weights_df
r2_player <- player_model$r2
rmse_player <- player_model$rmse

# PREDICTIONS ----

cat("\n\nPhase 2: Generate Predictions\n")
cat("=============================\n")

# TEAM-LEVEL PREDICTIONS ----
cat("--- Team-Date Predictions ---\n")

# Prepare team-date aggregated data
team_date_predictions <- predicted_stats %>%
  group_by(team, date) %>%
  summarise(
    across(all_of(pred_stat_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# Generate predictions
team_date_predictions$predicted_margin <- predict_from_ridge(
  team_model, team_date_predictions, pred_stat_cols
)

# Latest predictions per team
team_latest <- team_date_predictions %>%
  group_by(team) %>%
  slice_max(date) %>%
  ungroup() %>%
  arrange(desc(predicted_margin))

cat("\nLatest predicted team strength:\n")
print(team_latest %>%
        select(team, date, predicted_margin) %>%
        mutate(predicted_margin = round(predicted_margin, 2)))

# PLAYER-LEVEL PREDICTIONS ----
cat("\n--- Player-Date Predictions ---\n")

# Generate predictions
predicted_stats$predicted_margin_contribution <- predict_from_ridge(
  player_model, predicted_stats, pred_stat_cols
)

# Latest player predictions
player_latest <- predicted_stats %>%
  group_by(player_id) %>%
  slice_max(date) %>%
  ungroup() %>%
  arrange(desc(predicted_margin_contribution))

cat("\nTop 15 predicted player contributions (latest):\n")
print(player_latest %>%
        slice_head(n = 15) %>%
        select(player_name, team, date, predicted_margin_contribution) %>%
        mutate(predicted_margin_contribution = round(predicted_margin_contribution, 2)))

# Aggregate to team level (using latest predictions)
team_predictions <- player_latest %>%
  filter(year(date)==year(Sys.Date())) %>%
  group_by(team) %>%
  summarise(
    total_predicted_contribution = sum(predicted_margin_contribution, na.rm = TRUE),
    avg_player_contribution = mean(predicted_margin_contribution, na.rm = TRUE),
    n_players = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_predicted_contribution))

cat("\nLatest team strength (sum of player contributions):\n")
print(team_predictions %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))))

# COMPILE RESULTS ----

cat("\n\nPhase 3: Compile Results\n")
cat("========================\n")

# Compile all player analysis results
player_analysis_results <- list(
  # Regression results
  regression_team_date = list(
    data = team_date_regression_data,
    weights = stat_weights_team,
    weights_df = stat_weights_team_df,
    model = cv_model_team,
    r2 = r2_team,
    rmse = rmse_team,
    predictions = team_date_predictions,
    latest = team_latest
  ),
  regression_player_date = list(
    data = player_date_regression_data,
    weights = stat_weights_player,
    weights_df = stat_weights_player_df,
    model = cv_model_player,
    r2 = r2_player,
    rmse = rmse_player,
    player_predictions = player_latest,
    team_summary = team_predictions
  ),

  # Integration with prediction results from script 03
  prediction_results = player_prediction_results
)

# Save to environment
assign("player_analysis_results", player_analysis_results, envir = .GlobalEnv)

# SAVE TO CACHE ----
save_cache(player_analysis_results, cache_file)

# VISUALIZATIONS ----

library(ggplot2)

cat("\nGenerating visualizations...\n")

# Compare regression weights (team vs player level)
weights_comparison <- stat_weights_team_df %>%
  rename(team_weight = weight, team_abs = abs_weight) %>%
  select(statistic, team_weight) %>%
  inner_join(
    stat_weights_player_df %>%
      rename(player_weight = weight) %>%
      select(statistic, player_weight),
    by = "statistic"
  ) %>%
  pivot_longer(cols = c(team_weight, player_weight),
               names_to = "level", values_to = "weight") %>%
  mutate(level = ifelse(level == "team_weight", "Team-Date", "Player-Date"))

weights_comp_plot <- ggplot(weights_comparison,
                            aes(x = reorder(statistic, weight), y = weight, fill = level)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Statistical Weights: Team-Date vs Player-Date Regression",
       x = "Statistic", y = "Weight Coefficient", fill = "Level") +
  theme_minimal()
print(weights_comp_plot)

# Team strength from team-date model
team_plot <- ggplot(team_latest, aes(x = reorder(team, predicted_margin), y = predicted_margin)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Predicted Team Strength (Team-Date Model)",
       x = "Team", y = "Predicted Margin") +
  theme_minimal()
print(team_plot)

# Top players from player-date model
player_plot <- ggplot(player_latest %>% slice_head(n = 20),
                      aes(x = reorder(player_name, predicted_margin_contribution),
                          y = predicted_margin_contribution)) +
  geom_col(fill = "darkgreen", alpha = 0.7) +
  coord_flip() +
  labs(title = "Top 20 Predicted Player Contributions",
       x = "Player", y = "Predicted Margin Contribution") +
  theme_minimal()
print(player_plot)

# Team strength from player-date model
team_player_plot <- ggplot(team_predictions,
                            aes(x = reorder(team, total_predicted_contribution),
                                y = total_predicted_contribution)) +
  geom_col(fill = "coral", alpha = 0.7) +
  coord_flip() +
  labs(title = "Team Strength (Sum of Player Contributions)",
       x = "Team", y = "Total Predicted Contribution") +
  theme_minimal()
print(team_player_plot)

# SUMMARY ----

cat("\n=== ANALYSIS COMPLETE ===\n\n")

cat("Regressions:\n")
cat(sprintf("  Team-Date: R² = %.3f, RMSE = %.2f (%d games)\n",
            r2_team, rmse_team, nrow(team_date_regression_data)))
cat(sprintf("  Player-Date: R² = %.3f, RMSE = %.2f (%d player-games)\n",
            r2_player, rmse_player, nrow(player_date_regression_data)))

cat("\nPredictions:\n")
cat(sprintf("  Team strength: %d teams\n", nrow(team_latest)))
cat(sprintf("  Player contributions: %d players\n", nrow(player_latest)))

cat("\nResults saved to 'player_analysis_results'\n")
