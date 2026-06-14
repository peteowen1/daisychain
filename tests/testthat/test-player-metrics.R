test_that("calculate_player_impact creates expected metrics", {
  # Create sample player data
  sample_data <- data.frame(
    player_id = c("P1", "P2", "P3"),
    disposals = c(20, 15, 10),
    effective_disposals = c(18, 12, 8),
    goals = c(2, 1, 0),
    behinds = c(1, 2, 1),
    marks = c(5, 3, 2),
    tackles = c(4, 6, 3),
    contested_possessions = c(8, 7, 5),
    metres_gained = c(200, 150, 100),
    intercepts = c(2, 1, 3),
    inside_fifties = c(3, 2, 1),
    kicks = c(12, 8, 6),
    clangers = c(2, 3, 1),
    total_score = c(13, 8, 1),
    scoring_shots = c(3, 3, 1)
  )

  result <- calculate_player_impact(sample_data)

  # Check that new metrics are created
  expect_true("disposal_efficiency" %in% names(result))
  expect_true("score_per_disposal" %in% names(result))
  expect_true("metres_per_disposal" %in% names(result))
  expect_true("impact_score" %in% names(result))

  # Check calculations are reasonable
  expect_true(all(result$disposal_efficiency >= 0 & result$disposal_efficiency <= 100, na.rm = TRUE))
  expect_true(all(result$impact_score >= 0, na.rm = TRUE))
})

test_that("calculate_player_ratings creates rating components", {
  # Create sample data with required columns
  sample_data <- data.frame(
    player_id = c("P1", "P2", "P3"),
    disposals = c(20, 15, 10),
    disposal_efficiency = c(80, 70, 60),
    goals = c(2, 1, 0),
    behinds = c(1, 2, 1),
    inside_fifties = c(3, 2, 1),
    tackles = c(4, 6, 3),
    intercepts = c(2, 1, 3),
    contested_possessions = c(8, 7, 5),
    marks = c(5, 3, 2),
    hitouts = c(0, 0, 15),
    goal_accuracy = c(66.7, 33.3, 0)
  )

  result <- calculate_player_ratings(sample_data, include_margin_impact = FALSE)

  # Check that rating components are created
  expect_true("possession_rating" %in% names(result))
  expect_true("scoring_rating" %in% names(result))
  expect_true("defensive_rating" %in% names(result))
  expect_true("overall_rating" %in% names(result))

  # Check ratings are in reasonable range (0-10)
  expect_true(all(result$overall_rating >= 0 & result$overall_rating <= 10, na.rm = TRUE))
  expect_true(all(result$possession_rating >= 0 & result$possession_rating <= 10, na.rm = TRUE))
})

test_that("identify_top_performers filters and ranks correctly", {
  # Create sample player data
  sample_data <- data.frame(
    player_id = paste0("P", 1:10),
    player_name = paste("Player", 1:10),
    team = rep(c("Team A", "Team B"), 5),
    overall_rating = c(8.5, 7.2, 6.8, 9.1, 5.5, 7.8, 6.2, 8.0, 7.5, 6.0),
    match_id = rep(paste0("M", 1:5), 2)
  )

  # Add multiple games per player
  sample_data <- rbind(sample_data, sample_data)
  sample_data$match_id[11:20] <- paste0("M", 6:10)

  result <- identify_top_performers(sample_data, metric = "overall_rating",
                                   min_games = 2, top_n = 5)

  # Check structure
  expect_true("rank" %in% names(result))
  expect_true("avg_metric" %in% names(result))
  expect_true("games_played" %in% names(result))

  # Check filtering and ranking
  expect_true(all(result$games_played >= 2))
  expect_equal(nrow(result), 5)
  expect_false(is.unsorted(rev(result$avg_metric)))  # Should be descending order
})

test_that("calculate_pvar handles replacement level calculation", {
  # Create sample data with sufficient players
  set.seed(42)
  sample_data <- data.frame(
    player_id = paste0("P", 1:20),
    player_name = paste("Player", 1:20),
    team = rep(c("Team A", "Team B", "Team C", "Team D"), 5),
    overall_rating = rnorm(20, mean = 6, sd = 1.5),
    match_id = rep(paste0("M", 1:10), 2)
  )

  # Add multiple games per player
  sample_data <- rbind(sample_data, sample_data, sample_data)
  sample_data$match_id <- rep(paste0("M", 1:30), 2)

  result <- calculate_pvar(sample_data)

  # Check PVAR metrics are created
  expect_true("pvar_game" %in% names(result))
  expect_true("replacement_rating" %in% names(result))
  expect_true("avg_pvar_per_game" %in% names(result))
  expect_true("pvar_category" %in% names(result))

  # Check that replacement rating is reasonable
  expect_true(all(result$replacement_rating > 0, na.rm = TRUE))
  expect_true(all(result$replacement_rating < 10, na.rm = TRUE))
})