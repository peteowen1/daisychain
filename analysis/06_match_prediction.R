# 06 - Match Prediction Model
library(tidyverse)
library(xgboost)
library(mgcv)
devtools::load_all()

# CACHE SETUP ----
cache_file <- "analysis/cache/06_match_prediction.rds"

load_with_fallback(
  object_name = "match_prediction_results",
  current_cache = cache_file,
  fallback_caches = list(
    enhanced_player_data = "analysis/cache/02_feature_engineering.rds",
    player_analysis_results = "analysis/cache/04_player_analysis.rds",
    player_prediction_results = "analysis/cache/03_player_prediction_model.rds",
    elo_results = "analysis/cache/05_elo_model.rds"
  ),
  extract_from_fallback = list(
    enhanced_player_data = "enhanced_player_data",
    player_analysis_results = NULL,
    player_prediction_results = NULL,
    elo_results = NULL
  )
)

# Load required data
if (!exists("enhanced_player_data")) {
  load_cache("enhanced_player_data", "analysis/cache/02_feature_engineering.rds",
             extract_name = "enhanced_player_data")
}
if (!exists("player_analysis_results")) {
  load_cache("player_analysis_results", "analysis/cache/04_player_analysis.rds")
}
if (!exists("player_prediction_results")) {
  load_cache("player_prediction_results", "analysis/cache/03_player_prediction_model.rds")
}
if (!exists("elo_results")) {
  load_cache("elo_results", "analysis/cache/05_elo_model.rds")
}

cat("=== MATCH PREDICTION MODEL ===\n\n")

# PREPARE DATA ----
cat("Preparing match data...\n")

team_strength <- player_analysis_results$regression_team_date$latest %>%
  select(team, predicted_margin) %>%
  rename(team_strength = predicted_margin)

match_data <- prepare_match_data_from_player(enhanced_player_data, team_strength)

# Add predicted stat differences
predicted_stats <- player_prediction_results$all_predictions_with_rookies
pred_stat_cols <- names(predicted_stats)[grepl("_predicted$", names(predicted_stats))]
pred_stat_cols <- pred_stat_cols[pred_stat_cols != "time_on_ground_percentage_predicted"]

match_data <- add_team_stat_differences(match_data, predicted_stats, pred_stat_cols)
stat_diff_cols <- attr(match_data, "stat_diff_cols")
stat_sum_cols <- attr(match_data, "stat_sum_cols")

# Add ELO features (both win/loss and margin-based)
match_data <- add_elo_momentum_features(
  match_data,
  team_elo_ratings = elo_results$team_elo_winloss,
  elo_momentum = elo_results$elo_momentum_winloss,
  current_elo = elo_results$current_elo_winloss %>% rename(current_rating = winloss_rating)
) %>%
  rename(
    winloss_elo_diff = elo_diff,
    winloss_home_elo = home_elo,
    winloss_away_elo = away_elo,
    winloss_home_momentum = home_elo_momentum,
    winloss_away_momentum = away_elo_momentum,
    winloss_momentum_diff = elo_momentum_diff
  )

# Add Margin-based ELO
match_data_margin_elo <- add_elo_momentum_features(
  match_data,
  team_elo_ratings = elo_results$team_elo_margin,
  elo_momentum = elo_results$elo_momentum_margin,
  current_elo = elo_results$current_elo_margin %>% rename(current_rating = margin_rating)
)

match_data <- match_data %>%
  mutate(
    margin_elo_diff = match_data_margin_elo$elo_diff,
    margin_home_elo = match_data_margin_elo$home_elo,
    margin_away_elo = match_data_margin_elo$away_elo,
    margin_home_momentum = match_data_margin_elo$home_elo_momentum,
    margin_away_momentum = match_data_margin_elo$away_elo_momentum,
    margin_momentum_diff = match_data_margin_elo$elo_momentum_diff
  )

# Calculate features and train/test split
match_data <- match_data %>%
  mutate(
    strength_diff = home_strength - away_strength,
    home_win = ifelse(home_margin > 0, 1, 0),
    total_score = home_score + away_score,
    year = year(date),
    season = year(date),
    home_team_season = paste0(home_team, "_", season),
    away_team_season = paste0(away_team, "_", season)
  ) %>%
  drop_na(home_strength, away_strength, home_margin)

train_data <- match_data %>% filter(year <= 2024)
test_data <- match_data %>% filter(year >= 2025)

cat(sprintf("Train: %d matches, Test: %d matches\n", nrow(train_data), nrow(test_data)))

# XGBOOST MARGIN MODEL ----
cat("\n=== XGBoost Margin Model ===\n")

feature_cols <- c("home_strength", "away_strength", "strength_diff",
                  "winloss_elo_diff", "winloss_home_momentum", "winloss_away_momentum",
                  "margin_elo_diff", "margin_home_momentum", "margin_away_momentum",
                  stat_diff_cols)

X_train_margin <- as.matrix(train_data[, feature_cols])
y_train_margin <- train_data$home_margin
X_test_margin <- as.matrix(test_data[, feature_cols])
y_test_margin <- test_data$home_margin

xgb_params_margin <- list(
  max_depth = 5, eta = 0.05, subsample = 0.6, colsample_bytree = 0.6,
  gamma = 5, min_child_weight = 10, lambda = 1, alpha = 1,
  objective = "reg:squarederror", eval_metric = "rmse"
)

cv_margin <- xgb.cv(
  params = xgb_params_margin, data = X_train_margin, label = y_train_margin,
  nrounds = 500, nfold = 5, early_stopping_rounds = 20, verbose = 0
)

optimal_rounds_margin <- cv_margin$best_iteration
dtrain_margin <- xgb.DMatrix(data = X_train_margin, label = y_train_margin)
margin_model <- xgb.train(params = xgb_params_margin, data = dtrain_margin,
                          nrounds = optimal_rounds_margin, verbose = 0)

predictions_margin_test <- predict(margin_model, X_test_margin)
rmse_margin <- sqrt(mean((y_test_margin - predictions_margin_test)^2))
mae_margin <- mean(abs(y_test_margin - predictions_margin_test))
r2_margin <- 1 - sum((y_test_margin - predictions_margin_test)^2) /
                  sum((y_test_margin - mean(y_test_margin))^2)

# Baseline
baseline_margin <- mean(y_train_margin)
rmse_baseline_margin <- sqrt(mean((y_test_margin - baseline_margin)^2))

cat(sprintf("RMSE: %.2f (baseline: %.2f, %.1f%% better)\n",
            rmse_margin, rmse_baseline_margin,
            100 * (rmse_baseline_margin - rmse_margin) / rmse_baseline_margin))
cat(sprintf("R²: %.3f\n", r2_margin))

# XGBOOST TOTAL MODEL ----
cat("\n=== XGBoost Total Model ===\n")

# Include both sum and diff features, let the model decide
feature_cols_total <- c("home_strength", "away_strength", "strength_diff",
                        "winloss_elo_diff", "winloss_home_momentum", "winloss_away_momentum",
                        "margin_elo_diff", "margin_home_momentum", "margin_away_momentum",
                        stat_sum_cols, stat_diff_cols)

X_train_total <- as.matrix(train_data[, feature_cols_total])
y_train_total <- train_data$total_score
X_test_total <- as.matrix(test_data[, feature_cols_total])
y_test_total <- test_data$total_score

xgb_params_total <- list(
  max_depth = 5, eta = 0.05, subsample = 0.6, colsample_bytree = 0.6,
  gamma = 5, min_child_weight = 10, lambda = 1, alpha = 1,
  objective = "reg:squarederror", eval_metric = "rmse"
)

cv_total <- xgb.cv(
  params = xgb_params_total, data = X_train_total, label = y_train_total,
  nrounds = 500, nfold = 5, early_stopping_rounds = 20, verbose = 0
)

optimal_rounds_total <- cv_total$best_iteration
dtrain_total <- xgb.DMatrix(data = X_train_total, label = y_train_total)
total_model <- xgb.train(params = xgb_params_total, data = dtrain_total,
                         nrounds = optimal_rounds_total, verbose = 0)

predictions_total_test <- predict(total_model, X_test_total)
rmse_total <- sqrt(mean((y_test_total - predictions_total_test)^2))
mae_total <- mean(abs(y_test_total - predictions_total_test))
r2_total <- 1 - sum((y_test_total - predictions_total_test)^2) /
                 sum((y_test_total - mean(y_test_total))^2)

# Baseline
baseline_total <- mean(y_train_total)
rmse_baseline_total <- sqrt(mean((y_test_total - baseline_total)^2))

cat(sprintf("RMSE: %.2f (baseline: %.2f, %.1f%% better)\n",
            rmse_total, rmse_baseline_total,
            100 * (rmse_baseline_total - rmse_total) / rmse_baseline_total))
cat(sprintf("R²: %.3f\n", r2_total))

# XGBOOST WIN MODEL ----
cat("\n=== XGBoost Win Model ===\n")

train_data$predicted_margin <- predict(margin_model, X_train_margin)
test_data$predicted_margin <- predict(margin_model, X_test_margin)
train_data$predicted_total <- predict(total_model, X_train_total)
test_data$predicted_total <- predict(total_model, X_test_total)

feature_cols_win <- c(feature_cols, "predicted_margin", "predicted_total")
X_train_win <- as.matrix(train_data[, feature_cols_win])
y_train_win <- train_data$home_win
X_test_win <- as.matrix(test_data[, feature_cols_win])
y_test_win <- test_data$home_win

xgb_params_win <- list(
  max_depth = 4, eta = 0.05, subsample = 0.6, colsample_bytree = 0.6,
  gamma = 5, min_child_weight = 10, lambda = 1, alpha = 1, max_delta_step = 1,
  objective = "binary:logistic", eval_metric = "logloss"
)

cv_win <- xgb.cv(
  params = xgb_params_win, data = X_train_win, label = y_train_win,
  nrounds = 500, nfold = 5, stratified = TRUE, early_stopping_rounds = 20, verbose = 0
)

optimal_rounds_win <- cv_win$best_iteration
dtrain_win <- xgb.DMatrix(data = X_train_win, label = y_train_win)
win_model <- xgb.train(params = xgb_params_win, data = dtrain_win,
                       nrounds = optimal_rounds_win, verbose = 0)

predictions_win_test <- predict(win_model, X_test_win)
accuracy_win <- mean((predictions_win_test > 0.5) == y_test_win)
logloss_win <- -mean(y_test_win * log(predictions_win_test + 1e-15) +
                     (1 - y_test_win) * log(1 - predictions_win_test + 1e-15))

# Baseline
baseline_win_prob <- mean(y_train_win)
accuracy_baseline_win <- mean((y_test_win == 1 & baseline_win_prob >= 0.5) |
                               (y_test_win == 0 & baseline_win_prob < 0.5))
logloss_baseline_win <- -mean(y_test_win * log(baseline_win_prob + 1e-15) +
                               (1 - y_test_win) * log(1 - baseline_win_prob + 1e-15))

cat(sprintf("Accuracy: %.1f%% (baseline: %.1f%%, %.1f%% better)\n",
            accuracy_win * 100, accuracy_baseline_win * 100,
            100 * (accuracy_win - accuracy_baseline_win) / accuracy_baseline_win))
cat(sprintf("LogLoss: %.4f (baseline: %.4f, %.1f%% better)\n",
            logloss_win, logloss_baseline_win,
            100 * (logloss_baseline_win - logloss_win) / logloss_baseline_win))

# GAM MARGIN MODEL ----
cat("\n=== GAM Margin Model ===\n")

gam_margin_train <- prepare_gam_data(train_data, stat_diff_cols, "home_margin")
gam_factor_levels <- list(
  home_team_season = levels(gam_margin_train$home_team_season),
  away_team_season = levels(gam_margin_train$away_team_season),
  venue = levels(gam_margin_train$venue)
)
gam_margin_test <- prepare_gam_data(test_data, stat_diff_cols, "home_margin", gam_factor_levels)

stat_diff_formula <- paste0("s(", stat_diff_cols, ", bs = 'ts')", collapse = " + ")
elo_formula_margin <- paste0(
  "s(winloss_elo_diff, bs = 'ts') + s(winloss_home_momentum, bs = 'ts') + s(winloss_away_momentum, bs = 'ts') + ",
  "s(margin_elo_diff, bs = 'ts') + s(margin_home_momentum, bs = 'ts') + s(margin_away_momentum, bs = 'ts')"
)

gam_formula_margin <- as.formula(paste0(
  "home_margin ~ s(strength_diff, bs = 'ts') + s(round_number, bs = 'ts') + ti(home_strength, away_strength) + ",
  elo_formula_margin, " + ", stat_diff_formula
))

gam_margin <- bam(gam_formula_margin, data = gam_margin_train,
                  family = gaussian(), discrete = TRUE, select = TRUE)

predictions_gam_margin_test <- predict(gam_margin, newdata = gam_margin_test)
rmse_gam_margin <- sqrt(mean((gam_margin_test$home_margin - predictions_gam_margin_test)^2))
r2_gam_margin <- 1 - sum((gam_margin_test$home_margin - predictions_gam_margin_test)^2) /
                    sum((gam_margin_test$home_margin - mean(gam_margin_test$home_margin))^2)

baseline_gam_margin <- mean(gam_margin_train$home_margin)
rmse_baseline_gam_margin <- sqrt(mean((gam_margin_test$home_margin - baseline_gam_margin)^2))

cat(sprintf("RMSE: %.2f (baseline: %.2f, %.1f%% better)\n",
            rmse_gam_margin, rmse_baseline_gam_margin,
            100 * (rmse_baseline_gam_margin - rmse_gam_margin) / rmse_baseline_gam_margin))
cat(sprintf("R²: %.3f\n", r2_gam_margin))

# GAM TOTAL MODEL ----
cat("\n=== GAM Total Model ===\n")

# Include both sum and diff features, let the model decide
gam_total_train <- prepare_gam_data(train_data, c(stat_sum_cols, stat_diff_cols), "total_score", gam_factor_levels)
gam_total_test <- prepare_gam_data(test_data, c(stat_sum_cols, stat_diff_cols), "total_score", gam_factor_levels)

stat_sum_formula <- paste0("s(", stat_sum_cols, ", bs = 'ts')", collapse = " + ")

gam_formula_total <- as.formula(paste0(
  "total_score ~ s(home_strength, bs = 'ts') + s(away_strength, bs = 'ts') + s(round_number, bs = 'ts') + ",
  "ti(home_strength, away_strength) + ", elo_formula_margin, " + ",
  stat_sum_formula, " + ", stat_diff_formula
))

gam_total <- bam(gam_formula_total, data = gam_total_train,
                 family = gaussian(), discrete = TRUE, select = TRUE)

predictions_gam_total_test <- predict(gam_total, newdata = gam_total_test)
rmse_gam_total <- sqrt(mean((gam_total_test$total_score - predictions_gam_total_test)^2))
r2_gam_total <- 1 - sum((gam_total_test$total_score - predictions_gam_total_test)^2) /
                   sum((gam_total_test$total_score - mean(gam_total_test$total_score))^2)

baseline_gam_total <- mean(gam_total_train$total_score)
rmse_baseline_gam_total <- sqrt(mean((gam_total_test$total_score - baseline_gam_total)^2))

cat(sprintf("RMSE: %.2f (baseline: %.2f, %.1f%% better)\n",
            rmse_gam_total, rmse_baseline_gam_total,
            100 * (rmse_baseline_gam_total - rmse_gam_total) / rmse_baseline_gam_total))
cat(sprintf("R²: %.3f\n", r2_gam_total))

# GAM WIN MODEL ----
cat("\n=== GAM Win Model ===\n")

train_data_gam <- train_data %>%
  mutate(
    predicted_margin_gam = predict(gam_margin, newdata = gam_margin_train),
    predicted_total_gam = predict(gam_total, newdata = gam_total_train)
  )

test_data_gam <- test_data %>%
  mutate(
    predicted_margin_gam = predictions_gam_margin_test,
    predicted_total_gam = predictions_gam_total_test
  )

gam_win_train <- prepare_gam_data(train_data_gam,
                                   c(stat_diff_cols, "predicted_margin_gam", "predicted_total_gam"),
                                   "home_win")
gam_win_test <- prepare_gam_data(test_data_gam,
                                  c(stat_diff_cols, "predicted_margin_gam", "predicted_total_gam"),
                                  "home_win", gam_factor_levels)

elo_formula_win <- "s(winloss_elo_diff, bs = 'ts') + s(margin_elo_diff, bs = 'ts')"

gam_formula_win <- as.formula(paste0(
  "home_win ~ s(strength_diff, bs = 'ts') + s(predicted_margin_gam, bs = 'ts') + s(predicted_total_gam, bs = 'ts') + ",
  "s(round_number, bs = 'ts') + ti(home_strength, away_strength) + ",
  "ti(predicted_margin_gam, strength_diff) + ti(predicted_margin_gam, predicted_total_gam) + ",
  elo_formula_win, " + ", stat_diff_formula
))

gam_win <- bam(gam_formula_win, data = gam_win_train,
               family = binomial(), discrete = TRUE, select = TRUE)

predictions_gam_win_test <- predict(gam_win, newdata = gam_win_test, type = "response")
accuracy_gam_win <- mean((predictions_gam_win_test > 0.5) == gam_win_test$home_win)
logloss_gam_win <- -mean(gam_win_test$home_win * log(predictions_gam_win_test + 1e-15) +
                         (1 - gam_win_test$home_win) * log(1 - predictions_gam_win_test + 1e-15))

baseline_gam_win_prob <- mean(gam_win_train$home_win)
accuracy_baseline_gam_win <- mean((gam_win_test$home_win == 1 & baseline_gam_win_prob >= 0.5) |
                                   (gam_win_test$home_win == 0 & baseline_gam_win_prob < 0.5))
logloss_baseline_gam_win <- -mean(gam_win_test$home_win * log(baseline_gam_win_prob + 1e-15) +
                                   (1 - gam_win_test$home_win) * log(1 - baseline_gam_win_prob + 1e-15))

cat(sprintf("Accuracy: %.1f%% (baseline: %.1f%%, %.1f%% better)\n",
            accuracy_gam_win * 100, accuracy_baseline_gam_win * 100,
            100 * (accuracy_gam_win - accuracy_baseline_gam_win) / accuracy_baseline_gam_win))
cat(sprintf("LogLoss: %.4f (baseline: %.4f, %.1f%% better)\n",
            logloss_gam_win, logloss_baseline_gam_win,
            100 * (logloss_baseline_gam_win - logloss_gam_win) / logloss_baseline_gam_win))

# SUMMARY ----
cat("\n=== MODEL COMPARISON ===\n")
cat("\nMargin Prediction:\n")
cat(sprintf("  XGBoost: RMSE = %.2f, R² = %.3f\n", rmse_margin, r2_margin))
cat(sprintf("  GAM:     RMSE = %.2f, R² = %.3f\n", rmse_gam_margin, r2_gam_margin))
cat(sprintf("  Winner:  %s\n", ifelse(rmse_margin < rmse_gam_margin, "XGBoost", "GAM")))

cat("\nTotal Score Prediction:\n")
cat(sprintf("  XGBoost: RMSE = %.2f, R² = %.3f\n", rmse_total, r2_total))
cat(sprintf("  GAM:     RMSE = %.2f, R² = %.3f\n", rmse_gam_total, r2_gam_total))
cat(sprintf("  Winner:  %s\n", ifelse(rmse_total < rmse_gam_total, "XGBoost", "GAM")))

cat("\nWin Probability:\n")
cat(sprintf("  XGBoost: Accuracy = %.1f%%, LogLoss = %.4f\n", accuracy_win * 100, logloss_win))
cat(sprintf("  GAM:     Accuracy = %.1f%%, LogLoss = %.4f\n", accuracy_gam_win * 100, logloss_gam_win))
cat(sprintf("  Winner:  %s\n", ifelse(logloss_win < logloss_gam_win, "XGBoost", "GAM")))

# SAVE RESULTS ----
match_prediction_results <- list(
  xgb_margin_model = margin_model,
  xgb_total_model = total_model,
  xgb_win_model = win_model,
  gam_margin_model = gam_margin,
  gam_total_model = gam_total,
  gam_win_model = gam_win,
  test_performance = list(
    xgb_margin = list(rmse = rmse_margin, r2 = r2_margin),
    xgb_total = list(rmse = rmse_total, r2 = r2_total),
    xgb_win = list(accuracy = accuracy_win, logloss = logloss_win),
    gam_margin = list(rmse = rmse_gam_margin, r2 = r2_gam_margin),
    gam_total = list(rmse = rmse_gam_total, r2 = r2_gam_total),
    gam_win = list(accuracy = accuracy_gam_win, logloss = logloss_gam_win)
  ),
  gam_factor_levels = gam_factor_levels,
  feature_cols = list(margin = feature_cols, total = feature_cols_total, win = feature_cols_win),
  stat_diff_cols = stat_diff_cols,
  stat_sum_cols = stat_sum_cols
)

assign("match_prediction_results", match_prediction_results, envir = .GlobalEnv)
save_cache(match_prediction_results, cache_file)

cat("\n=== COMPLETE ===\n")
cat("Results saved to 'match_prediction_results'\n")
