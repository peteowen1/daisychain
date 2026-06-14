test_that("validate_seasons works correctly", {
  # Test valid seasons
  expect_true(validate_seasons(2020:2023))
  expect_true(validate_seasons(2022))

  # Test invalid seasons
  expect_error(validate_seasons(2010))  # Before AFLW
  expect_error(validate_seasons(2050))  # Future
  expect_error(validate_seasons("2020"))  # Non-numeric
})

test_that("clean_team_names standardizes team names", {
  # Test known team names
  expect_equal(clean_team_names("Adelaide"), "Adelaide")
  expect_equal(clean_team_names("ADEL"), "Adelaide")
  expect_equal(clean_team_names("Brisbane Lions"), "Brisbane Lions")
  expect_equal(clean_team_names("BRIS"), "Brisbane Lions")

  # Test multiple names
  result <- clean_team_names(c("Adelaide", "BRIS", "Carlton"))
  expect_equal(result, c("Adelaide", "Brisbane Lions", "Carlton"))

  # Test NA handling
  expect_true(is.na(clean_team_names(NA)))
})

test_that("safe_divide handles division correctly", {
  # Normal division
  expect_equal(safe_divide(10, 2), 5)

  # Division by zero
  expect_equal(safe_divide(10, 0, na_value = 0), 0)
  expect_true(is.na(safe_divide(10, 0)))

  # Vector operations
  result <- safe_divide(c(10, 20, 30), c(2, 0, 5), na_value = 0)
  expect_equal(result, c(5, 0, 6))
})

test_that("check_data_completeness identifies issues", {
  # Create test data with missing values
  test_data <- data.frame(
    a = c(1, 2, NA, 4),
    b = c("x", "y", "z", NA),
    c = c(1:4)
  )

  result <- check_data_completeness(test_data, required_cols = c("a", "b", "c"))

  expect_equal(result$n_rows, 4)
  expect_equal(result$n_cols, 3)
  expect_equal(result$complete_cases, 2)
  expect_gt(result$missing_summary$missing_count[result$missing_summary$column == "a"], 0)
})

test_that("standardize_column_names works correctly", {
  test_data <- data.frame(
    `Player.Name` = c("A", "B"),
    `Team Name` = c("X", "Y"),
    GoalsScored = c(1, 2),
    check.names = FALSE
  )

  result <- standardize_column_names(test_data)

  # Check that names are standardized
  expect_true("player_name" %in% names(result) ||
              "team_name" %in% names(result) ||
              "goalsscored" %in% names(result))
  expect_false(any(grepl("\\.", names(result))))
  expect_false(any(grepl("\\s", names(result))))
})