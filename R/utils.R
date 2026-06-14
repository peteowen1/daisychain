#' Check Package Dependencies
#'
#' Check if required packages are installed and load them.
#'
#' @param packages Character vector of package names to check
#' @param install_missing Logical, whether to install missing packages
#' @return Logical vector indicating which packages are available
#' @importFrom utils install.packages
#' @export
#' @examples
#' \dontrun{
#' check_dependencies(c("dplyr", "ggplot2"))
#' }
check_dependencies <- function(packages, install_missing = FALSE) {

  available <- sapply(packages, function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      return(TRUE)
    } else {
      if (install_missing) {
        cat("Installing missing package:", pkg, "\n")
        utils::install.packages(pkg)
        return(requireNamespace(pkg, quietly = TRUE))
      } else {
        warning(paste("Package", pkg, "is not available"))
        return(FALSE)
      }
    }
  })

  return(available)
}

#' Validate Season Parameter
#'
#' Check if season parameters are valid for AFLW data.
#'
#' @param seasons Numeric vector of seasons
#' @return Logical indicating if seasons are valid
#' @export
#' @examples
#' \dontrun{
#' validate_seasons(2020:2023)
#' }
validate_seasons <- function(seasons) {

  if (!is.numeric(seasons)) {
    stop("Seasons must be numeric")
  }

  min_season <- 2017  # First AFLW season
  max_season <- as.numeric(format(Sys.Date(), "%Y"))

  invalid_seasons <- seasons[seasons < min_season | seasons > max_season]

  if (length(invalid_seasons) > 0) {
    stop(paste("Invalid seasons:", paste(invalid_seasons, collapse = ", "),
               ". AFLW seasons range from", min_season, "to", max_season))
  }

  return(TRUE)
}

#' Standardize Column Names
#'
#' Standardize column names to snake_case and handle common variations.
#'
#' @param data Data frame to standardize
#' @return Data frame with standardized column names
#' @export
#' @examples
#' \dontrun{
#' clean_data <- standardize_column_names(raw_data)
#' }
standardize_column_names <- function(data) {

  # Define common column name mappings
  name_mappings <- list(
    "Player" = "player_name",
    "Team" = "team",
    "Round" = "round_number",
    "Season" = "season",
    "Date" = "date",
    "Kicks" = "kicks",
    "Handballs" = "handballs",
    "Disposals" = "disposals",
    "Marks" = "marks",
    "Goals" = "goals",
    "Behinds" = "behinds",
    "Tackles" = "tackles",
    "Hitouts" = "hitouts",
    "Home.Team" = "home_team",
    "Away.Team" = "away_team",
    "Home.Score" = "home_score",
    "Away.Score" = "away_score",
    "Venue" = "venue"
  )

  old_names <- names(data)
  new_names <- old_names

  # Apply mappings
  for (old_pattern in names(name_mappings)) {
    matches <- grep(old_pattern, old_names, ignore.case = TRUE)
    if (length(matches) > 0) {
      new_names[matches] <- name_mappings[[old_pattern]]
    }
  }

  # Convert to snake_case for any remaining names
  new_names <- gsub("([a-z])([A-Z])", "\\1_\\2", new_names)  # camelCase to snake_case
  new_names <- gsub("\\.", "_", new_names)  # dots to underscores
  new_names <- gsub("\\s+", "_", new_names)  # spaces to underscores
  new_names <- tolower(new_names)  # to lowercase
  new_names <- gsub("_{2,}", "_", new_names)  # multiple underscores to single
  new_names <- gsub("^_|_$", "", new_names)  # remove leading/trailing underscores

  names(data) <- new_names
  return(data)
}

#' Check Data Completeness
#'
#' Check for missing values and data completeness issues.
#'
#' @param data Data frame to check
#' @param required_cols Character vector of required column names
#' @param min_rows Minimum number of rows expected
#' @return List with completeness statistics and issues
#' @export
#' @examples
#' \dontrun{
#' completeness <- check_data_completeness(player_data, c("player_name", "team"))
#' }
check_data_completeness <- function(data, required_cols = NULL, min_rows = 1) {

  # Filter out list columns for complete.cases check
  non_list_cols <- sapply(data, function(x) !is.list(x))
  data_for_completeness <- data[, non_list_cols, drop = FALSE]

  results <- list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    missing_summary = list(),
    issues = character(),
    complete_cases = ifelse(ncol(data_for_completeness) > 0,
                           sum(complete.cases(data_for_completeness)),
                           nrow(data))
  )

  # Check minimum rows
  if (nrow(data) < min_rows) {
    results$issues <- c(results$issues,
                       paste("Data has", nrow(data), "rows but minimum", min_rows, "required"))
  }

  # Check required columns
  if (!is.null(required_cols)) {
    missing_cols <- required_cols[!required_cols %in% names(data)]
    if (length(missing_cols) > 0) {
      results$issues <- c(results$issues,
                         paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }
  }

  # Check missing values
  missing_counts <- sapply(data, function(x) sum(is.na(x)))
  missing_pct <- round(missing_counts / nrow(data) * 100, 2)

  results$missing_summary <- data.frame(
    column = names(missing_counts),
    missing_count = missing_counts,
    missing_pct = missing_pct,
    stringsAsFactors = FALSE
  )

  # Flag columns with high missing percentages
  high_missing <- names(missing_pct)[missing_pct > 50]
  if (length(high_missing) > 0) {
    results$issues <- c(results$issues,
                       paste("Columns with >50% missing values:",
                             paste(high_missing, collapse = ", ")))
  }

  # Overall completeness
  results$completeness_pct <- round(results$complete_cases / nrow(data) * 100, 2)

  return(results)
}

#' Detect Data Anomalies
#'
#' Detect potential anomalies and outliers in numerical data.
#'
#' @param data Data frame to check
#' @param numeric_cols Character vector of numeric columns to check (if NULL, auto-detect)
#' @param outlier_method Method for outlier detection ("iqr" or "zscore")
#' @param threshold Threshold for outlier detection
#' @return List with anomaly detection results
#' @export
#' @examples
#' \dontrun{
#' anomalies <- detect_anomalies(player_stats, c("disposals", "goals"))
#' }
detect_anomalies <- function(data,
                             numeric_cols = NULL,
                             outlier_method = "iqr",
                             threshold = 3) {

  if (is.null(numeric_cols)) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
  }

  results <- list(
    method = outlier_method,
    threshold = threshold,
    anomalies = list(),
    summary = data.frame()
  )

  for (col in numeric_cols) {
    if (col %in% names(data) && is.numeric(data[[col]])) {
      col_data <- data[[col]][!is.na(data[[col]])]

      if (length(col_data) > 0) {
        if (outlier_method == "iqr") {
          Q1 <- quantile(col_data, 0.25)
          Q3 <- quantile(col_data, 0.75)
          IQR <- Q3 - Q1
          lower_bound <- Q1 - 1.5 * IQR
          upper_bound <- Q3 + 1.5 * IQR
          outliers <- which(data[[col]] < lower_bound | data[[col]] > upper_bound)
        } else if (outlier_method == "zscore") {
          z_scores <- abs((col_data - mean(col_data)) / sd(col_data))
          outliers <- which(z_scores > threshold)
        }

        results$anomalies[[col]] <- list(
          outlier_indices = outliers,
          n_outliers = length(outliers),
          outlier_values = if(length(outliers) > 0) data[[col]][outliers] else numeric(0)
        )
      }
    }
  }

  # Create summary
  summary_data <- data.frame(
    column = names(results$anomalies),
    n_outliers = sapply(results$anomalies, function(x) x$n_outliers),
    outlier_pct = sapply(results$anomalies, function(x) {
      round(x$n_outliers / nrow(data) * 100, 2)
    }),
    stringsAsFactors = FALSE
  )

  results$summary <- summary_data
  return(results)
}

#' Create Data Summary Report
#'
#' Generate a comprehensive summary report of the data.
#'
#' @param data Data frame to summarize
#' @param title Optional title for the report
#' @return Character vector with formatted report
#' @export
#' @examples
#' \dontrun{
#' report <- create_data_summary(player_data, "Player Statistics Summary")
#' cat(report, sep = "\n")
#' }
create_data_summary <- function(data, title = "Data Summary Report") {

  report <- character()
  report <- c(report, paste("=== ", title, " ==="))
  report <- c(report, paste("Generated on:", Sys.time()))
  report <- c(report, "")

  # Basic dimensions
  report <- c(report, "BASIC INFORMATION:")
  report <- c(report, paste("  Rows:", nrow(data)))
  report <- c(report, paste("  Columns:", ncol(data)))
  report <- c(report, "")

  # Column types
  report <- c(report, "COLUMN TYPES:")
  col_types <- sapply(data, class)
  type_summary <- table(col_types)
  for (type in names(type_summary)) {
    report <- c(report, paste(" ", type, ":", type_summary[[type]], "columns"))
  }
  report <- c(report, "")

  # Missing values
  completeness <- check_data_completeness(data)
  report <- c(report, "DATA COMPLETENESS:")
  report <- c(report, paste("  Complete cases:", completeness$complete_cases,
                           "(" , completeness$completeness_pct, "%)"))

  high_missing <- completeness$missing_summary[completeness$missing_summary$missing_pct > 10, ]
  if (nrow(high_missing) > 0) {
    report <- c(report, "  Columns with >10% missing:")
    for (i in 1:nrow(high_missing)) {
      report <- c(report, paste("   ", high_missing$column[i], ":",
                               high_missing$missing_pct[i], "%"))
    }
  }
  report <- c(report, "")

  # Numeric summaries
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  if (length(numeric_cols) > 0) {
    report <- c(report, "NUMERIC COLUMNS SUMMARY:")
    numeric_summary <- data[numeric_cols] %>%
      dplyr::summarise_all(list(
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      ), .names = "{.col}_{.fn}")

    for (col in numeric_cols) {
      if (length(col) > 20) next  # Skip if too many columns
      min_val <- round(numeric_summary[[paste0(col, "_min")]], 2)
      max_val <- round(numeric_summary[[paste0(col, "_max")]], 2)
      mean_val <- round(numeric_summary[[paste0(col, "_mean")]], 2)
      median_val <- round(numeric_summary[[paste0(col, "_median")]], 2)

      report <- c(report, paste("  ", col, ":"))
      report <- c(report, paste("    Range:", min_val, "-", max_val))
      report <- c(report, paste("    Mean:", mean_val, "| Median:", median_val))
    }
  }

  return(report)
}

#' Safe Division
#'
#' Perform division with protection against division by zero.
#'
#' @param numerator Numeric vector (numerator)
#' @param denominator Numeric vector (denominator)
#' @param na_value Value to return when denominator is 0 or NA
#' @return Numeric vector with safe division results
#' @export
#' @examples
#' \dontrun{
#' efficiency <- safe_divide(effective_disposals, total_disposals, 0)
#' }
safe_divide <- function(numerator, denominator, na_value = NA_real_) {
  result <- ifelse(denominator == 0 | is.na(denominator) | is.na(numerator),
                   na_value,
                   numerator / denominator)
  return(result)
}

#' Convert Character Columns to Factors
#'
#' Convert character columns to factors for better memory usage and modeling.
#'
#' @param data Data frame to convert
#' @param exclude_cols Character vector of columns to exclude from conversion
#' @param max_levels Maximum number of levels for conversion (default: 50)
#' @return Data frame with character columns converted to factors
#' @export
#' @examples
#' \dontrun{
#' factor_data <- char_to_factors(data, exclude_cols = c("player_name"))
#' }
char_to_factors <- function(data, exclude_cols = NULL, max_levels = 50) {

  char_cols <- names(data)[sapply(data, is.character)]

  if (!is.null(exclude_cols)) {
    char_cols <- char_cols[!char_cols %in% exclude_cols]
  }

  for (col in char_cols) {
    n_unique <- length(unique(data[[col]]))
    if (n_unique <= max_levels) {
      data[[col]] <- as.factor(data[[col]])
    } else {
      warning(paste("Column", col, "has", n_unique,
                   "unique values, not converting to factor"))
    }
  }

  return(data)
}