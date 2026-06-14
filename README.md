# daisychain: AFLW Data Analysis and Predictive Modeling

## Overview

The `daisychain` package provides a comprehensive suite of tools for Australian Football League Women's (AFLW) data collection, analysis, and predictive modeling. The package enables users to:

- Scrape AFLW data using fitzRoy
- Analyze player contributions to game margins
- Build player rating systems
- Create team performance models
- Predict match outcomes (margins, totals, win probabilities)

## Installation

### From Source

```r
# Install development tools if needed
install.packages(c("devtools", "roxygen2", "testthat"))

# Clone or download the repository, then:
devtools::install("path/to/daisychain")
```

### Dependencies

The package requires:
- fitzRoy (>= 1.6.0) for AFLW data access
- dplyr, ggplot2, lubridate for data manipulation and visualization
- broom, modelr for modeling utilities
- Optional: glmnet, randomForest, caret for advanced modeling

## Quick Start

```r
library(daisychain)

# 1. Collect AFLW data
aflw_data <- scrape_aflw_comprehensive(seasons = 2022:2023)

# 2. Process and clean the data
clean_results <- process_aflw_results(aflw_data$results)
clean_player_stats <- process_aflw_player_stats(aflw_data$player_stats)
merged_data <- merge_results_stats(clean_results, clean_player_stats)

# 3. Calculate player ratings and margin contributions
player_ratings <- calculate_player_ratings(merged_data)
margin_contributions <- calculate_player_margin_contribution(player_ratings)

# 4. Build team models
team_ratings <- calculate_team_elo_ratings(clean_results)
team_model <- build_team_performance_model(team_ratings)

# 5. Predict match outcomes
match_features <- create_match_context(clean_results)
prediction_models <- build_match_prediction_model(match_features)
```

## Core Functionality

### Data Collection

The package provides wrapper functions around fitzRoy for AFLW-specific data collection:

```r
# Collect match results
results <- scrape_aflw_results(seasons = 2022:2023)

# Collect player statistics
player_stats <- scrape_aflw_player_stats(seasons = 2022:2023)

# Comprehensive data collection
all_data <- scrape_aflw_comprehensive(seasons = 2022:2023)
```

### Player Analysis

Analyze individual player performance and contribution to team success:

```r
# Calculate player impact metrics
impact_data <- calculate_player_impact(player_data)

# Build player rating system
player_ratings <- calculate_player_ratings(impact_data)

# Analyze margin contributions
margin_data <- calculate_player_margin_contribution(player_ratings)

# Identify top performers
top_players <- identify_top_performers(player_ratings, metric = "overall_rating")

# Calculate player value above replacement
pvar_data <- calculate_pvar(player_ratings)
```

### Team Analysis

Model team performance and strength:

```r
# Calculate Elo ratings
elo_ratings <- calculate_team_elo_ratings(results_data)

# Analyze roster strength
roster_strength <- analyze_team_roster_strength(player_ratings)

# Calculate team momentum
momentum <- calculate_team_momentum(team_results)

# Predict team matchups
prediction <- predict_team_matchup("Adelaide", "Carlton", team_ratings)
```

### Match Prediction

Build comprehensive models for match outcome prediction:

```r
# Create advanced features
rolling_stats <- calculate_rolling_stats(player_data)
team_strength <- calculate_team_strength(results_data)
h2h_stats <- calculate_h2h_stats(results_data)

# Build prediction models
match_models <- build_match_prediction_model(
  match_data = enhanced_features,
  target_vars = c("margin", "total_score"),
  model_type = "ensemble"
)

# Make predictions
prediction <- predict_match_outcome(
  match_models,
  "Adelaide",
  "Carlton",
  match_features
)
```

## Analysis Workflow

The package includes complete analysis scripts in the `analysis/` directory:

1. **01_data_collection.R** - Demonstrates data scraping and cleaning
2. **02_feature_engineering.R** - Creates advanced features for modeling
3. **03_player_prediction_model.R** - Advanced player performance prediction using XGBoost
4. **04_player_analysis.R** - Player performance analysis and data-driven valuations
5. **05_team_modeling.R** - Team analysis and match prediction

Run these scripts in sequence to see the full capabilities of the package.

## Key Features

### Player Metrics
- **Overall Rating**: Composite rating based on multiple performance dimensions
- **Margin Contribution**: Quantifies player impact on game margins
- **PVAR**: Player Value Above Replacement analysis
- **Form Analysis**: Recent performance trends and consistency metrics

### Team Analysis
- **Elo Rating System**: Dynamic team strength ratings
- **Roster Strength**: Analysis of team depth and star power
- **Momentum Tracking**: Recent form and trend analysis
- **Head-to-Head Statistics**: Historical matchup analysis

### Predictive Modeling
- **Match Outcome Prediction**: Win probability, margin, and total score
- **Player Performance Models**: Predict individual player ratings
- **Team Performance Models**: Model team success factors
- **Ensemble Methods**: Combine multiple modeling approaches

### Data Quality
- **Validation Functions**: Check data completeness and quality
- **Cleaning Utilities**: Standardize team names and formats
- **Feature Engineering**: Create rolling averages, lag features, and contextual variables

## Model Performance

The package includes comprehensive model evaluation:

```r
# Evaluate player models
evaluation <- evaluate_model_performance(player_model)

# Evaluate match predictions
match_eval <- evaluate_match_predictions(match_models, test_data)

# Generate prediction reports
report <- generate_prediction_report(match_prediction)
```

## Example: Complete Analysis

```r
library(daisychain)

# Step 1: Data Collection
cat("Collecting AFLW data...\n")
aflw_data <- scrape_aflw_comprehensive(seasons = 2022:2023, include_lineups = FALSE)

# Step 2: Data Processing
clean_results <- process_aflw_results(aflw_data$results)
clean_player_stats <- process_aflw_player_stats(aflw_data$player_stats)
merged_data <- merge_results_stats(clean_results, clean_player_stats)

# Step 3: Feature Engineering
rolling_data <- calculate_rolling_stats(merged_data)
impact_data <- calculate_player_impact(rolling_data)
team_strength <- calculate_team_strength(clean_results)

# Step 4: Player Analysis
player_ratings <- calculate_player_ratings(impact_data, include_margin_impact = TRUE)
margin_contributions <- calculate_player_margin_contribution(player_ratings)
top_players <- identify_top_performers(player_ratings, min_games = 5)

# Step 5: Team Analysis
elo_ratings <- calculate_team_elo_ratings(clean_results)
roster_strength <- analyze_team_roster_strength(player_ratings)

# Step 6: Match Prediction
match_features <- create_match_context(clean_results)
h2h_data <- calculate_h2h_stats(clean_results)
prediction_models <- build_match_prediction_model(h2h_data,
                                                  target_vars = c("margin", "total_score"))

# Step 7: Make Predictions
sample_prediction <- predict_match_outcome(prediction_models, "Adelaide", "Carlton",
                                          match_features[1,])
report <- generate_prediction_report(sample_prediction)
cat(report, sep = "\n")
```

## Data Sources

The package relies on fitzRoy for data access, which provides:
- Match results and fixtures
- Player statistics
- Team lineups (optional)
- Ladder standings

Data is available from 2017 onwards (first AFLW season).

## Model Types Supported

- **Linear Regression**: Baseline models with interpretable coefficients
- **Elastic Net**: Regularized regression for high-dimensional data
- **Random Forest**: Non-linear ensemble models
- **Ensemble Methods**: Combine multiple model types

## Contributing

This package follows R package development best practices:
- Comprehensive documentation with roxygen2
- Unit tests with testthat
- Consistent code style
- Modular function design

## Citation

If you use this package in research or analysis, please cite:

```
O'Reilly, P. (2024). daisychain: AFLW Data Analysis and Predictive Modeling.
R package version 0.1.0.
```

## License

MIT + file LICENSE

## Support

For questions, issues, or feature requests, please contact the package maintainer or create an issue in the repository.

---

*Built with ❤️ for AFLW analytics*