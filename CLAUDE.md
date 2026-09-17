# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

This is `daisychain`, an R package for AFLW (Australian Football League Women's) data analysis and predictive modeling. The core innovation is **player margin contribution analysis** - quantifying how individual player performances impact team success through game margins.

## Development Commands

### Package Development
```r
# Generate documentation and check package
devtools::document()
devtools::check()

# Install package locally
devtools::install()

# Run tests
devtools::test()
# Run specific test file
testthat::test_file("tests/testthat/test-player-metrics.R")

# Load package for development
devtools::load_all()
```

## Architecture & Data Flow

### Core Data Pipeline
1. **Data Collection** (`scrape_aflw_data.R`) → Raw AFLW data via fitzRoy
2. **Data Processing** (`data_processing.R`) → Clean, validate, merge data
3. **Feature Engineering** (`feature_engineering.R`) → Rolling stats, team metrics, context
4. **Analysis Layer** → Player metrics, team models, match prediction

### Key Innovation: Player Margin Contribution
The package's unique contribution is linking individual player performance to team success:
- `calculate_player_margin_contribution()` weights player stats by correlation with game margins
- `calculate_player_ratings()` builds multi-dimensional ratings including margin impact
- `identify_top_performers()` finds players who contribute most to winning

### Modeling Architecture
- **Player Models** (`player_models.R`): Individual performance prediction and margin impact
- **Team Models** (`team_models.R`): Elo ratings, roster strength, momentum tracking
- **Match Prediction** (`match_prediction.R`): Comprehensive outcome modeling (margin, totals, win probability)

### Analysis Scripts Structure
Located in `analysis/` directory, designed for sequential execution:
1. `01_data_collection.R` - Data scraping and basic processing
2. `02_feature_engineering.R` - Advanced feature creation
3. `03_player_prediction_model.R` - Player performance modeling (XGBoost)
4. `03b_bayesian_player_prediction.R` - Bayesian player modeling
5. `04_player_analysis.R` - Player margin contribution and ratings analysis
6. `05_elo_model.R` - ELO ratings and team strength metrics
7. `06_match_prediction.R` - Match outcome prediction (margin, win probability)
8. `04b_rapm_analysis.R` - RAPM (Regularized Adjusted Plus/Minus) player impact analysis
9. `04c_bpm_analysis.R` - BPM (Box Plus/Minus) career-stat-based player analysis

## Key Constraints & Patterns

### Conditional Dependencies
Advanced modeling features use conditional imports:
```r
if (requireNamespace("slider", quietly = TRUE)) {
  # Use slider for rolling calculations
} else {
  # Fallback to base R methods
}
```

### Data Validation
All functions include input validation and graceful error handling:
- Season validation (AFLW started 2017)
- Missing data handling
- Data quality checks via `validate_aflw_data()`

## Core Functions by Module

### Data Collection (`scrape_aflw_data.R`)
- `scrape_aflw_comprehensive()` - Primary data collection function
- `scrape_aflw_results()`, `scrape_aflw_player_stats()` - Specific data types

### Player Analysis (`player_metrics.R`)
- `calculate_player_margin_contribution()` - Core innovation
- `calculate_player_ratings()` - Multi-dimensional rating system
- `calculate_pvar()` - Player Value Above Replacement

### Team Analysis (`team_models.R`)
- `calculate_team_elo_ratings()` - Dynamic team strength
- `analyze_team_roster_strength()` - Squad analysis
- `predict_team_matchup()` - Head-to-head predictions

### ELO Features (`elo_utils.R`)
- `add_elo_features_to_matches()` - Join ELO ratings to match data
- `calculate_elo_momentum()` - Rolling average of rating changes
- `get_team_elo_at_date()` - Lookup team ELO at specific date
- `add_elo_momentum_features()` - Add ELO and momentum features together

### Match Prediction (`match_prediction.R`)
- `build_match_prediction_model()` - Comprehensive modeling
- `predict_match_outcome()` - Win probability, margin, totals
- `generate_prediction_report()` - Formatted insights

### Feature Engineering (`feature_engineering.R`)
- `calculate_rolling_stats()` - Time-based metrics
- `create_match_context()` - Situational features
- `calculate_h2h_stats()` - Historical matchup data

### RAPM Analysis (`rapm_analysis.R`)
- `prepare_rapm_matrix()` - Build sparse player-contribution matrix (TOG-weighted) for Regularized Adjusted Plus/Minus

### BPM Analysis (`bpm_analysis.R`)
- `calculate_player_career_stats()` - Aggregate TOG-weighted career box-score stats for Box Plus/Minus

### Margin Regression (`margin_regression.R`)
- `prepare_game_margins()` / ridge regression helpers - Team-margin regression on player contributions

### Caching (`cache_utils.R`)
- `load_cache()` / `save_cache()` / `load_with_fallback()` - Generic cache-or-fetch helpers used across the pipeline

### Match Data Prep (`match_data_prep.R`)
- `prepare_match_data_from_player()`, `add_team_stat_differences()`, `prepare_gam_data()` - Aggregates player-level data to match/team level for the GAM stat models

### Player Prediction Helpers (`player_prediction_helpers.R`)
- `optimize_all_decay_parameters()`, `calculate_decay_features()`, `train_xgb_stat_model()` - Decay-weighted feature engineering and XGBoost stat-projection training

### Player Value Helpers (`player_value_helpers.R`)
- `calculate_player_match_value()`, `aggregate_team_values()`, `learn_stat_weights_from_data()` - Per-stat value weighting, learned from data rather than hand-set

(this module list is not guaranteed exhaustive — run `ls R/` to confirm)

## Testing Strategy

Tests focus on core functionality and edge cases:
- Input validation (invalid seasons, missing data)
- Function output structure and data types
- Mathematical calculations (ratings, margins)
- Located in `tests/testthat/`

## Related Projects

AFL-adjacent. See `C:\dev\torpverse\` for the main AFL analytics ecosystem (torp package, TORP ratings). Both use fitzRoy for data and share player-impact methodology (daisychain's margin contribution overlaps with torp's TORP/PSR approach). For full ecosystem: `C:\dev\ECOSYSTEM.md`
