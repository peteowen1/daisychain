# 05 - AFLW ELO Model Script
library(tidyverse)
devtools::load_all()

# CACHE SETUP ----
cache_file <- "analysis/cache/05_elo_model.rds"

# Load from cache with fallback to previous scripts
load_with_fallback(
  object_name = "elo_results",
  current_cache = cache_file,
  fallback_caches = list(
    enhanced_match_data = "analysis/cache/02_feature_engineering.rds",
    clean_results = "analysis/cache/01_data_collection.rds"
  ),
  extract_from_fallback = list(
    enhanced_match_data = "enhanced_match_data",
    clean_results = "clean_results"
  )
)

cat("=== ELO MODEL ===\n\n")

# DATA PREPARATION ----

# Load match data - prefer enhanced_match_data, fall back to clean_results
if (!exists("enhanced_match_data") && !exists("clean_results")) {
  load_cache("enhanced_match_data", "analysis/cache/02_feature_engineering.rds",
             extract_name = "enhanced_match_data")
}

if (!exists("enhanced_match_data") && !exists("clean_results")) {
  load_cache("clean_results", "analysis/cache/01_data_collection.rds",
             extract_name = "clean_results")
}

if (!exists("enhanced_match_data") && !exists("clean_results")) {
  stop("Please run 01_data_collection.R or 02_feature_engineering.R first")
}

# Use enhanced_match_data if available, otherwise clean_results
if (exists("enhanced_match_data")) {
  match_data <- enhanced_match_data
  cat("Using enhanced_match_data from script 02\n")
} else {
  match_data <- clean_results
  cat("Using clean_results from script 01\n")
}

# Ensure required columns exist and have no NAs
cat("Validating match data for ELO calculation...\n")
required_cols <- c("date", "home_team", "away_team", "margin")
missing_cols <- setdiff(required_cols, names(match_data))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Remove any matches with NA in critical columns
match_data <- match_data %>%
  filter(!is.na(home_team), !is.na(away_team), !is.na(margin), !is.na(date))

cat(sprintf("Match data validated: %d matches\n", nrow(match_data)))

# ELO RATINGS ----

cat("\nPhase 1: Calculate ELO Ratings\n")
cat("===============================\n")

# Calculate Win/Loss ELO ratings
cat("\n1a. Win/Loss ELO (binary outcome)\n")
team_elo_winloss <- calculate_team_elo_ratings(
  results_data = match_data,
  initial_rating = 1500,
  k_factor = 32,
  home_advantage = 50
)

cat(sprintf("Win/Loss ELO calculated: %d team-game records\n", nrow(team_elo_winloss)))

# Current Win/Loss ELO standings
current_elo_winloss <- team_elo_winloss %>%
  group_by(team) %>%
  slice_max(date, n = 1) %>%
  ungroup() %>%
  arrange(desc(rating_after)) %>%
  select(team, winloss_rating = rating_after, winloss_change = rating_change)

cat("\nCurrent Win/Loss ELO Standings:\n")
print(current_elo_winloss %>% mutate(across(where(is.numeric), ~ round(.x, 1))))

# Calculate Margin-based ELO ratings
cat("\n1b. Margin-based ELO (sqrt margin)\n")
team_elo_margin <- calculate_margin_elo_ratings(
  results_data = match_data,
  initial_rating = 1500,
  k_factor = 32,
  home_advantage = 50,
  margin_scale = 10
)

cat(sprintf("Margin-based ELO calculated: %d team-game records\n", nrow(team_elo_margin)))

# Current Margin-based ELO standings
current_elo_margin <- team_elo_margin %>%
  group_by(team) %>%
  slice_max(date, n = 1) %>%
  ungroup() %>%
  arrange(desc(rating_after)) %>%
  select(team, margin_rating = rating_after, margin_change = rating_change)

cat("\nCurrent Margin-based ELO Standings:\n")
print(current_elo_margin %>% mutate(across(where(is.numeric), ~ round(.x, 1))))

# Combined current ELO standings
current_elo_combined <- current_elo_winloss %>%
  inner_join(current_elo_margin, by = "team") %>%
  mutate(rating_diff = margin_rating - winloss_rating) %>%
  arrange(desc(margin_rating))

cat("\nCombined ELO Comparison:\n")
print(current_elo_combined %>% mutate(across(where(is.numeric), ~ round(.x, 1))))

# TEAM MOMENTUM ----

cat("\nPhase 2: Calculate Team Momentum\n")
cat("=================================\n")

# Calculate Win/Loss ELO momentum
cat("\n2a. Win/Loss ELO momentum\n")
elo_momentum_winloss <- calculate_elo_momentum(
  team_elo_ratings = team_elo_winloss,
  window = 5
)

cat(sprintf("Win/Loss ELO momentum calculated: %d records\n", nrow(elo_momentum_winloss)))

# Calculate Margin-based ELO momentum
cat("\n2b. Margin-based ELO momentum\n")
elo_momentum_margin <- calculate_elo_momentum(
  team_elo_ratings = team_elo_margin,
  window = 5
)

cat(sprintf("Margin-based ELO momentum calculated: %d records\n", nrow(elo_momentum_margin)))

# Current momentum leaders (Win/Loss)
current_momentum_winloss <- team_elo_winloss %>%
  left_join(elo_momentum_winloss, by = c("team", "date")) %>%
  group_by(team) %>%
  slice_max(date, n = 1) %>%
  ungroup() %>%
  filter(!is.na(elo_momentum)) %>%
  arrange(desc(elo_momentum)) %>%
  select(team, winloss_rating = rating_after, winloss_momentum = elo_momentum)

# Current momentum leaders (Margin)
current_momentum_margin <- team_elo_margin %>%
  left_join(elo_momentum_margin, by = c("team", "date")) %>%
  group_by(team) %>%
  slice_max(date, n = 1) %>%
  ungroup() %>%
  filter(!is.na(elo_momentum)) %>%
  arrange(desc(elo_momentum)) %>%
  select(team, margin_rating = rating_after, margin_momentum = elo_momentum)

# Combined momentum
current_momentum_combined <- current_momentum_winloss %>%
  inner_join(current_momentum_margin, by = "team") %>%
  arrange(desc(margin_momentum))

cat("\nTop Teams by Momentum (Margin-based):\n")
print(current_momentum_combined %>%
        slice_head(n = 8) %>%
        mutate(across(where(is.numeric), ~ round(.x, 1))))

# ELO STATISTICS ----

cat("\nPhase 3: ELO Statistics\n")
cat("=======================\n")

# Calculate ELO volatility for both systems
elo_volatility_winloss <- team_elo_winloss %>%
  group_by(team) %>%
  summarise(
    winloss_volatility = sd(rating_change, na.rm = TRUE),
    winloss_avg = mean(rating_after, na.rm = TRUE),
    .groups = "drop"
  )

elo_volatility_margin <- team_elo_margin %>%
  group_by(team) %>%
  summarise(
    margin_volatility = sd(rating_change, na.rm = TRUE),
    margin_avg = mean(rating_after, na.rm = TRUE),
    .groups = "drop"
  )

elo_volatility_combined <- elo_volatility_winloss %>%
  inner_join(elo_volatility_margin, by = "team")

cat("\nELO Volatility Comparison (top 5 by margin volatility):\n")
print(elo_volatility_combined %>%
        arrange(desc(margin_volatility)) %>%
        slice_head(n = 5) %>%
        mutate(across(where(is.numeric), ~ round(.x, 1))))

# COMPILE RESULTS ----

cat("\nPhase 4: Compile Results\n")
cat("========================\n")

# Compile all ELO results for downstream use
elo_results <- list(
  # Win/Loss ELO
  team_elo_winloss = team_elo_winloss,
  current_elo_winloss = current_elo_winloss,
  elo_momentum_winloss = elo_momentum_winloss,
  current_momentum_winloss = current_momentum_winloss,

  # Margin-based ELO
  team_elo_margin = team_elo_margin,
  current_elo_margin = current_elo_margin,
  elo_momentum_margin = elo_momentum_margin,
  current_momentum_margin = current_momentum_margin,

  # Combined/Comparison
  current_elo_combined = current_elo_combined,
  current_momentum_combined = current_momentum_combined,
  elo_volatility_combined = elo_volatility_combined,

  # Legacy compatibility (use margin-based as default)
  team_elo_ratings = team_elo_margin,
  current_elo_standings = current_elo_margin,
  elo_momentum = elo_momentum_margin,
  current_momentum = current_momentum_margin
)

# Save to environment
assign("elo_results", elo_results, envir = .GlobalEnv)

# SAVE TO CACHE ----
save_cache(elo_results, cache_file)

# VISUALIZATIONS ----

library(ggplot2)

cat("\nGenerating visualizations...\n")

# 1. Win/Loss vs Margin ELO comparison
elo_comparison_plot <- ggplot(current_elo_combined,
                               aes(x = winloss_rating, y = margin_rating)) +
  geom_point(size = 3, alpha = 0.7, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = team), vjust = -0.5, size = 2.5) +
  labs(title = "Win/Loss ELO vs Margin-based ELO",
       subtitle = "Teams above line: margin-based > win/loss",
       x = "Win/Loss ELO", y = "Margin-based ELO") +
  theme_minimal()
print(elo_comparison_plot)

# 2. Margin-based ELO progression over time
elo_progression_margin <- ggplot(team_elo_margin,
                                  aes(x = date, y = rating_after, color = team)) +
  geom_line(alpha = 0.7, linewidth = 0.8) +
  labs(title = "Margin-based ELO Rating Progression",
       x = "Date", y = "ELO Rating") +
  theme_minimal() +
  theme(legend.position = "none")
print(elo_progression_margin)

# 3. Current standings comparison
standings_data <- current_elo_combined %>%
  select(team, winloss_rating, margin_rating) %>%
  pivot_longer(cols = c(winloss_rating, margin_rating),
               names_to = "elo_type", values_to = "rating") %>%
  mutate(elo_type = recode(elo_type,
                            "winloss_rating" = "Win/Loss",
                            "margin_rating" = "Margin-based"))

standings_comparison_plot <- ggplot(standings_data,
                                     aes(x = reorder(team, rating),
                                         y = rating, fill = elo_type)) +
  geom_col(position = "dodge", alpha = 0.7) +
  coord_flip() +
  labs(title = "Current ELO Standings: Win/Loss vs Margin-based",
       x = "Team", y = "ELO Rating", fill = "ELO Type") +
  theme_minimal()
print(standings_comparison_plot)

# 4. Momentum comparison
momentum_comparison_plot <- ggplot(current_momentum_combined %>% slice_head(n = 10),
                                    aes(x = winloss_momentum, y = margin_momentum)) +
  geom_point(size = 3, alpha = 0.7, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = team), vjust = -0.5, size = 2.5) +
  labs(title = "Momentum Comparison: Win/Loss vs Margin-based",
       subtitle = "Top 10 teams by margin-based momentum",
       x = "Win/Loss Momentum", y = "Margin-based Momentum") +
  theme_minimal()
print(momentum_comparison_plot)

# SUMMARY ----

cat("\n=== ELO MODEL COMPLETE ===\n\n")

cat("ELO Systems Calculated:\n")
cat("  1. Win/Loss ELO (binary outcome)\n")
cat("  2. Margin-based ELO (sqrt margin)\n\n")

cat("Data Summary:\n")
cat(sprintf("  Teams: %d\n", n_distinct(team_elo_margin$team)))
cat(sprintf("  Matches: %d\n", nrow(match_data)))

cat("\nWin/Loss ELO Range:\n")
cat(sprintf("  Min: %.1f, Max: %.1f, Spread: %.1f\n",
            min(current_elo_winloss$winloss_rating),
            max(current_elo_winloss$winloss_rating),
            max(current_elo_winloss$winloss_rating) - min(current_elo_winloss$winloss_rating)))

cat("\nMargin-based ELO Range:\n")
cat(sprintf("  Min: %.1f, Max: %.1f, Spread: %.1f\n",
            min(current_elo_margin$margin_rating),
            max(current_elo_margin$margin_rating),
            max(current_elo_margin$margin_rating) - min(current_elo_margin$margin_rating)))

cat("\nResults saved to 'elo_results'\n")
cat("Both ELO systems available for use in script 06 match predictions\n")
