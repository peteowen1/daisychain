# 03b - Bayesian Player Performance Prediction Model (Testing)
library(tidyverse)
library(brms)
devtools::load_all()

# CACHE SETUP ----
cache_file <- "analysis/cache/03b_bayesian_player_prediction.rds"

# Load from cache with fallback to previous scripts
load_with_fallback(
  object_name = "bayesian_test_results",
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

cat("=== BAYESIAN PLAYER PREDICTION MODEL (TESTING) ===\n")
cat("Testing brms approach for player performance prediction...\n\n")

# PHASE 1: DATA PREPARATION FOR BAYESIAN MODELING ----

cat("Phase 1: Data Preparation\n")
cat("========================\n")

# Define statistics to model (start with a few key ones for testing)
test_stats <- c("disposals", "goals", "marks", "tackles")

# Filter available stats and prepare modeling data
available_test_stats <- test_stats[test_stats %in% names(enhanced_player_data)]
cat("Testing with statistics:", paste(available_test_stats, collapse = ", "), "\n")

# Create modeling dataset with required features
modeling_data <- enhanced_player_data %>%
  # Filter to players with reasonable game counts
  group_by(player_id) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  # Add contextual features
  arrange(player_id, date) %>%
  group_by(player_id) %>%
  mutate(
    # Game sequence for this player
    player_game_number = row_number(),

    # Days since last game
    days_rest = ifelse(row_number() == 1, 7,
                      as.numeric(date - lag(date, 1))),
    days_rest = pmin(days_rest, 30), # Cap at 30 days

    # Days since season start (for decay modeling)
    days_since_season_start = as.numeric(date - min(date))
  ) %>%
  ungroup() %>%
  # Add home/away numeric
  mutate(
    home_away_num = ifelse(home_away == "Home", 1, 0),
    # Scale days for numerical stability
    days_rest_scaled = scale(days_rest)[,1],
    days_since_start_scaled = scale(days_since_season_start)[,1]
  ) %>%
  # Remove rows with missing key variables
  filter(!is.na(home_away), !is.na(team)) %>%
  # Select relevant columns
  select(player_id, player_name, team, date, season, round_number,
         player_game_number, days_rest_scaled, days_since_start_scaled,
         home_away_num, all_of(available_test_stats)) %>%
  drop_na()

cat(sprintf("Prepared modeling data: %d player-game records\n", nrow(modeling_data)))
cat(sprintf("Players: %d, Teams: %d\n",
           n_distinct(modeling_data$player_id),
           n_distinct(modeling_data$team)))

# PHASE 2: BAYESIAN MODEL SPECIFICATION AND FITTING ----

cat("\nPhase 2: Bayesian Model Fitting\n")
cat("===============================\n")

# Function to fit Bayesian model for a single statistic
fit_bayesian_stat_model <- function(data, stat_name) {
  cat(sprintf("Fitting Bayesian model for %s...\n", stat_name))

  # Prepare formula with hierarchical structure
  formula_str <- paste0(
    stat_name, " ~ ",
    # Fixed effects
    "days_rest_scaled + home_away_num + days_since_start_scaled + ",
    # Random effects - player-specific intercepts and slopes
    "(1 + days_since_start_scaled | player_id) + ",
    # Team effects
    "(1 | team)"
  )

  model_formula <- as.formula(formula_str)
  cat("Formula:", formula_str, "\n")

  # Fit model with brms
  tryCatch({
    brm_model <- brm(
      formula = model_formula,
      data = data,
      family = gaussian(),
      # Priors for regularization
      prior = c(
        # Weakly informative priors for fixed effects
        prior(normal(0, 2), class = Intercept),
        prior(normal(0, 1), class = b),
        # Priors for random effect SDs
        prior(exponential(1), class = sd),
        # Prior for residual SD
        prior(exponential(1), class = sigma)
      ),
      # Sampling parameters
      chains = 2,
      iter = 1000,
      cores = 2,
      refresh = 0, # Reduce output
      control = list(adapt_delta = 0.95)
    )

    # Model diagnostics
    cat("Model summary for", stat_name, ":\n")
    print(summary(brm_model))

    # Check convergence
    rhat_max <- max(rhat(brm_model), na.rm = TRUE)
    cat(sprintf("Max R-hat: %.3f %s\n", rhat_max,
               ifelse(rhat_max < 1.1, "(Good)", "(Needs attention)")))

    return(list(
      model = brm_model,
      stat = stat_name,
      formula = model_formula,
      converged = rhat_max < 1.1
    ))

  }, error = function(e) {
    cat("Error fitting model for", stat_name, ":", e$message, "\n")
    return(NULL)
  })
}

# Fit models for each test statistic
bayesian_models <- list()
model_summaries <- list()

for (stat in available_test_stats) {
  # Only use data where this stat is not missing
  stat_data <- modeling_data %>%
    filter(!is.na(.data[[stat]]))

  if (nrow(stat_data) >= 100) { # Minimum data requirement
    model_result <- fit_bayesian_stat_model(stat_data, stat)

    if (!is.null(model_result)) {
      bayesian_models[[stat]] <- model_result

      # Extract model performance metrics
      model_summaries[[stat]] <- list(
        stat = stat,
        n_obs = nrow(stat_data),
        converged = model_result$converged,
        loo_ic = NA # Will calculate if model fits well
      )
    }
  } else {
    cat(sprintf("Insufficient data for %s (%d observations)\n", stat, nrow(stat_data)))
  }
}

# PHASE 3: MODEL EVALUATION AND COMPARISON ----

cat("\nPhase 3: Model Evaluation\n")
cat("========================\n")

# Function to evaluate model predictions
evaluate_bayesian_model <- function(model_result, data, stat_name) {
  cat(sprintf("Evaluating %s model...\n", stat_name))

  tryCatch({
    # Generate posterior predictions
    pp_check_result <- pp_check(model_result$model, ndraws = 50)
    print(pp_check_result)

    # Cross-validation (if computational resources allow)
    if (nrow(data) < 500) { # Only for smaller datasets due to computational cost
      loo_result <- loo(model_result$model)
      cat("LOO Information Criterion:", loo_result$estimates["looic", "Estimate"], "\n")
      return(loo_result$estimates["looic", "Estimate"])
    }

    return(NA)

  }, error = function(e) {
    cat("Error in model evaluation:", e$message, "\n")
    return(NA)
  })
}

# Evaluate fitted models
for (stat in names(bayesian_models)) {
  stat_data <- modeling_data %>% filter(!is.na(.data[[stat]]))
  loo_ic <- evaluate_bayesian_model(bayesian_models[[stat]], stat_data, stat)
  model_summaries[[stat]]$loo_ic <- loo_ic
}

# PHASE 4: PREDICTION GENERATION ----

cat("\nPhase 4: Generating Predictions\n")
cat("===============================\n")

# Function to generate predictions for new data
generate_bayesian_predictions <- function(model_list, new_data) {
  predictions <- new_data %>%
    select(player_id, player_name, team, date)

  for (stat in names(model_list)) {
    if (!is.null(model_list[[stat]]) && model_list[[stat]]$converged) {
      cat(sprintf("Generating predictions for %s...\n", stat))

      tryCatch({
        # Generate posterior predictive samples
        pred_samples <- posterior_predict(
          model_list[[stat]]$model,
          newdata = new_data,
          ndraws = 100
        )

        # Calculate prediction intervals
        predictions[[paste0(stat, "_pred_mean")]] <- apply(pred_samples, 2, mean)
        predictions[[paste0(stat, "_pred_lower")]] <- apply(pred_samples, 2, quantile, 0.1)
        predictions[[paste0(stat, "_pred_upper")]] <- apply(pred_samples, 2, quantile, 0.9)
        predictions[[paste0(stat, "_pred_sd")]] <- apply(pred_samples, 2, sd)

      }, error = function(e) {
        cat("Error generating predictions for", stat, ":", e$message, "\n")
      })
    }
  }

  return(predictions)
}

# Generate predictions for latest player data
latest_player_data <- modeling_data %>%
  group_by(player_id) %>%
  slice_max(date) %>%
  ungroup()

cat(sprintf("Generating predictions for %d players...\n", nrow(latest_player_data)))

bayesian_predictions <- generate_bayesian_predictions(bayesian_models, latest_player_data)

# PHASE 5: COMPARISON WITH SIMPLE APPROACH ----

cat("\nPhase 5: Comparison Analysis\n")
cat("===========================\n")

# Compare Bayesian predictions with simple exponential smoothing
comparison_results <- list()

for (stat in available_test_stats) {
  if (paste0(stat, "_pred_mean") %in% names(bayesian_predictions)) {

    # Calculate simple exponential smoothing baseline
    simple_predictions <- modeling_data %>%
      group_by(player_id) %>%
      arrange(date) %>%
      summarise(
        latest_date = max(date),
        simple_exp_smooth = {
          values <- .data[[stat]][!is.na(.data[[stat]])]
          if (length(values) >= 2) {
            # Simple exponential smoothing with alpha = 0.3
            weights <- 0.3 * (0.7 ^ (length(values):1 - 1))
            sum(values * weights) / sum(weights)
          } else if (length(values) == 1) {
            values[1]
          } else {
            NA
          }
        },
        .groups = "drop"
      )

    # Merge for comparison
    comparison <- bayesian_predictions %>%
      select(player_id, player_name, team,
             !!paste0(stat, "_pred_mean"), !!paste0(stat, "_pred_sd")) %>%
      left_join(simple_predictions, by = "player_id") %>%
      filter(!is.na(simple_exp_smooth), !is.na(.data[[paste0(stat, "_pred_mean")]]))

    if (nrow(comparison) > 0) {
      # Calculate correlation between approaches
      correlation <- cor(comparison[[paste0(stat, "_pred_mean")]],
                        comparison$simple_exp_smooth, use = "complete.obs")

      comparison_results[[stat]] <- list(
        correlation = correlation,
        n_players = nrow(comparison),
        bayesian_mean = mean(comparison[[paste0(stat, "_pred_mean")]], na.rm = TRUE),
        simple_mean = mean(comparison$simple_exp_smooth, na.rm = TRUE),
        bayesian_sd = mean(comparison[[paste0(stat, "_pred_sd")]], na.rm = TRUE)
      )

      cat(sprintf("%s: Correlation = %.3f, n = %d, Uncertainty = %.3f\n",
                 stat, correlation, nrow(comparison),
                 mean(comparison[[paste0(stat, "_pred_sd")]], na.rm = TRUE)))
    }
  }
}

# RESULTS SUMMARY ----

cat("\n=== BAYESIAN MODEL TESTING RESULTS ===\n")

# Model fitting success
successful_models <- sum(sapply(bayesian_models, function(x) !is.null(x) && x$converged))
cat(sprintf("Successfully fitted models: %d/%d\n", successful_models, length(available_test_stats)))

# Convergence summary
if (length(bayesian_models) > 0) {
  convergence_summary <- sapply(model_summaries, function(x) x$converged)
  cat("Model convergence:", paste(names(convergence_summary)[convergence_summary], collapse = ", "), "\n")
}

# Prediction comparison
if (length(comparison_results) > 0) {
  cat("\nPrediction correlations with simple exponential smoothing:\n")
  for (stat in names(comparison_results)) {
    cat(sprintf("  %s: r = %.3f\n", stat, comparison_results[[stat]]$correlation))
  }
}

# Save results for comparison
bayesian_test_results <- list(
  models = bayesian_models,
  predictions = bayesian_predictions,
  model_summaries = model_summaries,
  comparison_results = comparison_results,
  modeling_data = modeling_data
)

# Store in global environment for analysis
assign("bayesian_test_results", bayesian_test_results, envir = .GlobalEnv)

# SAVE TO CACHE ----
save_cache(bayesian_test_results, cache_file)

cat("\n=== BAYESIAN TESTING COMPLETE ===\n")
cat("Results saved to 'bayesian_test_results' object\n")
cat("Evaluate results before deciding whether to adopt this approach\n")

# Optional: Quick visualization if ggplot2 is available
if (requireNamespace("ggplot2", quietly = TRUE) && length(comparison_results) > 0) {
  library(ggplot2)

  # Plot comparison for first available statistic
  first_stat <- names(comparison_results)[1]

  if (!is.null(first_stat)) {
    comparison_plot_data <- bayesian_predictions %>%
      select(player_id, player_name, team,
             !!paste0(first_stat, "_pred_mean"), !!paste0(first_stat, "_pred_sd")) %>%
      left_join(
        modeling_data %>%
          group_by(player_id) %>%
          summarise(simple_exp_smooth = mean(.data[[first_stat]], na.rm = TRUE), .groups = "drop"),
        by = "player_id"
      ) %>%
      filter(!is.na(simple_exp_smooth), !is.na(.data[[paste0(first_stat, "_pred_mean")]]))

    if (nrow(comparison_plot_data) > 0) {
      p <- ggplot(comparison_plot_data,
                  aes(x = simple_exp_smooth, y = .data[[paste0(first_stat, "_pred_mean")]])) +
        geom_point(alpha = 0.6) +
        geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
        labs(title = paste("Bayesian vs Simple Predictions:", first_stat),
             x = "Simple Exponential Smoothing",
             y = "Bayesian Prediction (Mean)") +
        theme_minimal()

      print(p)
    }
  }
}