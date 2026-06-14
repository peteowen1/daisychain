# 03 - Advanced Player Performance Prediction Model
library(tidyverse)
library(xgboost)
devtools::load_all()

# CONFIGURATION ----
# Set to TRUE to re-run decay parameter optimization (takes ~5-10 minutes)
# Set to FALSE to load previously saved parameters
RUN_DECAY_OPTIMIZATION <- FALSE
DECAY_PARAMS_FILE <- "analysis/decay_params.rds"

# CACHE SETUP ----
cache_file <- "analysis/cache/03_player_prediction_model.rds"

# Load from cache with fallback to previous scripts
load_with_fallback(
  object_name = "player_prediction_results",
  current_cache = cache_file,
  fallback_caches = list(
    enhanced_player_data = "analysis/cache/02_feature_engineering.rds"
  ),
  extract_from_fallback = list(
    enhanced_player_data = "enhanced_player_data"
  )
)

# Ensure we have enhanced_player_data
if (!exists("enhanced_player_data")) {
  load_cache("enhanced_player_data", "analysis/cache/02_feature_engineering.rds",
             extract_name = "enhanced_player_data")
}

if (!exists("enhanced_player_data")) {
  stop("Please run 01_data_collection.R and 02_feature_engineering.R first")
}

cat("=== PLAYER PREDICTION MODEL ANALYSIS ===\n")
cat("Creating advanced player performance prediction system...\n\n")

# PHASE 1: EXPONENTIAL DECAY FEATURE ENGINEERING ----

cat("Phase 1: Exponential Decay Optimization\n")
cat("========================================\n")

# Define statistics to model
stats_to_model <- c("disposals", "goals", "marks", "tackles", "hitouts",
                   "contested_possessions", "uncontested_possessions",
                   "effective_disposals", "metres_gained", "intercepts",
                   "inside_fifties", "clangers", "frees_for", "frees_against",
                   "time_on_ground_percentage")

# Filter stats that exist in our data
available_stats <- stats_to_model[stats_to_model %in% names(enhanced_player_data)]
cat("Available statistics for modeling:", length(available_stats), "\n")
print(available_stats)

# Optimize or load decay parameters
if (RUN_DECAY_OPTIMIZATION) {
  cat("\nRunning decay parameter optimization (this may take several minutes)...\n")

  decay_params <- optimize_all_decay_parameters(
    player_data = enhanced_player_data,
    stats_to_optimize = available_stats
  )

  # Save for future use
  saveRDS(decay_params, DECAY_PARAMS_FILE)
  cat(sprintf("\nDecay parameters saved to %s\n", DECAY_PARAMS_FILE))

} else if (file.exists(DECAY_PARAMS_FILE)) {
  cat(sprintf("\nLoading previously saved decay parameters from %s\n", DECAY_PARAMS_FILE))
  decay_params <- readRDS(DECAY_PARAMS_FILE)

  # Verify all needed stats have parameters
  missing_stats <- setdiff(available_stats, names(decay_params))
  if (length(missing_stats) > 0) {
    cat(sprintf("\nWarning: Missing parameters for %d stats: %s\n",
                length(missing_stats), paste(missing_stats, collapse = ", ")))
    cat("Re-optimizing missing statistics...\n")

    new_params <- optimize_all_decay_parameters(
      player_data = enhanced_player_data,
      stats_to_optimize = missing_stats
    )
    decay_params <- c(decay_params, new_params)
    saveRDS(decay_params, DECAY_PARAMS_FILE)
  }

} else {
  cat(sprintf("\nNo saved decay parameters found at %s\n", DECAY_PARAMS_FILE))
  cat("Running decay parameter optimization (this may take several minutes)...\n")

  decay_params <- optimize_all_decay_parameters(
    player_data = enhanced_player_data,
    stats_to_optimize = available_stats
  )

  # Save for future use
  saveRDS(decay_params, DECAY_PARAMS_FILE)
  cat(sprintf("\nDecay parameters saved to %s\n", DECAY_PARAMS_FILE))
}

# Display summary
cat("\n=== OPTIMAL DECAY PARAMETERS ===\n")
decay_params_df <- data.frame(
  statistic = names(decay_params),
  beta = unlist(decay_params),
  half_life_days = -log(2) / log(unlist(decay_params))
) %>%
  arrange(desc(beta))

print(decay_params_df)

# Verify completeness
missing_params <- setdiff(available_stats, names(decay_params))
if (length(missing_params) > 0) {
  cat("\nWARNING: Missing decay parameters for:", paste(missing_params, collapse = ", "), "\n")
} else {
  cat(sprintf("\n✓ All %d statistics have optimized decay parameters\n", length(available_stats)))
}

# PHASE 2: CONTEXTUAL FEATURE ENGINEERING ----

cat("\nPhase 2: Contextual Feature Engineering\n")
cat("=======================================\n")

# Create comprehensive features for each player-match
modeling_dataset <- enhanced_player_data %>%
  arrange(player_id, date) %>%
  group_by(player_id) %>%
  mutate(
    # Days since last game
    days_rest = ifelse(row_number() == 1, 7,
                      as.numeric(date - lag(date, 1))),
    days_rest = pmin(days_rest, 30), # Cap at 30 days

    # Season game number (fatigue factor)
    season_game_number = row_number(),

    # Recent form trend (last 3 games vs previous 3) - using impact_score
    recent_avg_3 = (lag(impact_score, 1) + lag(impact_score, 2) + lag(impact_score, 3)) / 3,
    previous_avg_3 = (lag(impact_score, 4) + lag(impact_score, 5) + lag(impact_score, 6)) / 3,
    recent_form_trend = ifelse(row_number() <= 6, 0, recent_avg_3 - previous_avg_3)
  ) %>%
  ungroup() %>%
  group_by(season, round_number, team) %>%
  mutate(
    # Team context
    team_total_rating = sum(impact_score, na.rm = TRUE),
    team_avg_rating = mean(impact_score, na.rm = TRUE)
  ) %>%
  ungroup()

# Add venue familiarity
modeling_dataset <- modeling_dataset %>%
  arrange(player_id, date) %>%
  group_by(player_id, venue) %>%
  mutate(
    games_at_venue = row_number() - 1 # Previous games at this venue
  ) %>%
  ungroup()

# Calculate exponential decay features for each stat using DARKO approach
modeling_dataset <- calculate_decay_features(
  player_data = modeling_dataset,
  stats_list = available_stats,
  decay_params = decay_params
)

cat(sprintf("Dataset with decay features: %d player-match records\n", nrow(modeling_dataset)))

# PHASE 3: XGBOOST PREDICTION MODELS ----

cat("\nPhase 3: XGBoost Prediction Models\n")
cat("==================================\n")

# Prepare feature matrix
feature_cols <- c(
  paste0(available_stats, "_exp_avg"),
  "days_rest", "season_game_number", "recent_form_trend",
  "games_at_venue", "team_avg_rating"
)

# Add categorical features
modeling_dataset <- modeling_dataset %>%
  mutate(
    home_away_num = ifelse(home_away == "Home", 1, 0)
  )

# One-hot encode player position
if ("player_player_position" %in% names(modeling_dataset)) {
  cat("One-hot encoding player positions...\n")

  # Get unique positions
  positions <- unique(modeling_dataset$player_player_position)
  positions <- positions[!is.na(positions)]

  cat(sprintf("Found %d unique positions: %s\n",
              length(positions),
              paste(head(positions, 5), collapse = ", ")))

  # Create binary column for each position
  for (pos in positions) {
    # Clean position name for column name
    pos_clean <- gsub("[^a-zA-Z0-9]", "_", pos)
    pos_col_name <- paste0("pos_", pos_clean)

    modeling_dataset[[pos_col_name]] <- ifelse(
      modeling_dataset$player_player_position == pos,
      1,
      0
    )
    modeling_dataset[[pos_col_name]][is.na(modeling_dataset$player_player_position)] <- 0
  }

  # Add position columns to features
  position_cols <- paste0("pos_", gsub("[^a-zA-Z0-9]", "_", positions))
  feature_cols <- c(feature_cols, "home_away_num", position_cols)
} else {
  feature_cols <- c(feature_cols, "home_away_num")
  cat("Warning: player_player_position not found in data\n")
}

# Keep unfiltered version for demonstrating hybrid predictions (with all features)
modeling_dataset_with_first_games <- modeling_dataset

# Remove rows without exponential averages (first game for each player)
modeling_dataset <- modeling_dataset %>%
  filter(!is.na(disposals_exp_avg))

cat(sprintf("\nModeling dataset: %d player-match records (experienced players)\n", nrow(modeling_dataset)))
cat(sprintf("Including first games: %d player-match records\n", nrow(modeling_dataset_with_first_games)))

# Define XGBoost parameters (can be tuned here)
xgb_params <- list(
  max_depth = 6,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  objective = "reg:squarederror",
  eval_metric = "rmse"
)

# Train models for each statistic
stat_models <- list()
model_performance <- data.frame(
  statistic = character(),
  train_mae = numeric(),
  test_mae = numeric(),
  train_rmse = numeric(),
  test_rmse = numeric(),
  train_r2 = numeric(),
  test_r2 = numeric(),
  cv_rounds = numeric(),
  cv_rmse = numeric(),
  top_feature_1 = character(),
  top_feature_2 = character(),
  top_feature_3 = character(),
  stringsAsFactors = FALSE
)

for (stat in available_stats) {
  model_result <- train_xgb_stat_model(
    data = modeling_dataset,
    stat_name = stat,
    feature_columns = feature_cols,
    xgb_params = xgb_params
  )

  if (!is.null(model_result)) {
    stat_models[[stat]] <- model_result

    # Ensure we have 3 features (pad with NA if needed)
    top_features <- c(model_result$top_3_features, rep(NA, 3))[1:3]

    model_performance <- rbind(model_performance, data.frame(
      statistic = stat,
      train_mae = model_result$train_mae,
      test_mae = model_result$test_mae,
      train_rmse = model_result$train_rmse,
      test_rmse = model_result$test_rmse,
      train_r2 = model_result$train_r2,
      test_r2 = model_result$test_r2,
      cv_rounds = model_result$cv_rounds,
      cv_rmse = model_result$cv_rmse,
      top_feature_1 = top_features[1],
      top_feature_2 = top_features[2],
      top_feature_3 = top_features[3]
    ))
  }
}

cat("\n=== MODEL PERFORMANCE SUMMARY ===\n")
cat("\nOverall Performance (sorted by test RMSE):\n")
print(model_performance %>%
      select(statistic, test_rmse, test_r2, cv_rounds) %>%
      arrange(test_rmse))

cat("\n\nTop Features by Model:\n")
print(model_performance %>%
      select(statistic, top_feature_1, top_feature_2, top_feature_3) %>%
      arrange(statistic))

# PHASE 4: GENERATE PREDICTIONS ----

cat("\nPhase 4: Generating Player Predictions\n")
cat("======================================\n")

# Calculate position-based first game averages for fallback
position_averages <- calculate_position_first_game_averages(
  player_data = enhanced_player_data,
  stats_list = available_stats
)

# Generate predictions for the latest available data (hybrid approach)
cat("\nLatest predictions:\n")
latest_predictions <- generate_player_predictions(
  current_data = modeling_dataset %>%
    group_by(player_id) %>%
    slice_max(date) %>%
    ungroup(),
  models = stat_models,
  position_averages = position_averages
)

# Generate predictions for all data (experienced players only)
cat("\nAll predictions (modeling dataset - experienced players):\n")
total_predictions <- generate_player_predictions(
  current_data = modeling_dataset,
  models = stat_models,
  position_averages = position_averages
)

# Generate predictions including first-game players (hybrid demonstration)
cat("\nAll predictions (including first-game players):\n")
all_predictions_with_rookies <- generate_player_predictions(
  current_data = modeling_dataset_with_first_games,
  models = stat_models,
  position_averages = position_averages
)

cat(sprintf("\nGenerated predictions for %d players (latest)\n", nrow(latest_predictions)))
cat(sprintf("  - XGBoost: %d, Position avg: %d\n",
            sum(latest_predictions$prediction_method == "xgboost"),
            sum(latest_predictions$prediction_method == "position_avg")))

cat(sprintf("Generated predictions for %d player-matches (modeling dataset)\n", nrow(total_predictions)))
cat(sprintf("  - XGBoost: %d, Position avg: %d\n",
            sum(total_predictions$prediction_method == "xgboost"),
            sum(total_predictions$prediction_method == "position_avg")))

cat(sprintf("Generated predictions for %d player-matches (including rookies)\n", nrow(all_predictions_with_rookies)))
cat(sprintf("  - XGBoost: %d, Position avg: %d\n",
            sum(all_predictions_with_rookies$prediction_method == "xgboost"),
            sum(all_predictions_with_rookies$prediction_method == "position_avg")))

# SAVE RESULTS ----

cat("\nSaving prediction models and results...\n")

# Save model components for use in other scripts
player_prediction_results <- list(
  models = stat_models,
  decay_parameters = decay_params,
  performance = model_performance,
  latest_predictions = latest_predictions,
  total_predictions = total_predictions,
  all_predictions_with_rookies = all_predictions_with_rookies,
  modeling_dataset = modeling_dataset,
  position_averages = position_averages,
  xgb_params = xgb_params,
  feature_columns = feature_cols
)

# Save to environment for next script
assign("player_prediction_results", player_prediction_results, envir = .GlobalEnv)

# SAVE TO CACHE ----
save_cache(player_prediction_results, cache_file)

cat("\n=== PLAYER PREDICTION MODEL COMPLETE ===\n")
cat("Results saved to 'player_prediction_results' object\n")
cat("Ready for integration with player value analysis\n")
