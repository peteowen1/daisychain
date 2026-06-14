#' Clean and Standardize AFLW Team Names
#'
#' Standardize team names across different data sources and formats.
#'
#' @param team_names Character vector of team names to clean
#' @return Character vector of standardized team names
#' @export
#' @examples
#' \dontrun{
#' clean_team_names(c("Adelaide Crows", "ADEL", "Adelaide"))
#' }
clean_team_names <- function(team_names) {

  # Define team name mappings
  team_mapping <- list(
    "Adelaide" = c("Adelaide", "Adelaide Crows", "ADEL", "ADE"),
    "Brisbane Lions" = c("Brisbane Lions", "Brisbane", "BRIS", "BL", "BRI"),
    "Carlton" = c("Carlton", "Carlton Blues", "CARL", "CAR"),
    "Collingwood" = c("Collingwood", "Collingwood Magpies", "COLL", "COL"),
    "Essendon" = c("Essendon", "Essendon Bombers", "ESS", "ESS"),
    "Fremantle" = c("Fremantle", "Fremantle Dockers", "FREM", "FRE"),
    "Geelong Cats" = c("Geelong Cats", "Geelong", "GEEL", "GEE"),
    "Gold Coast Suns" = c("Gold Coast Suns", "Gold Coast", "GCFC", "GC", "GCS"),
    "GWS Giants" = c("GWS Giants", "GWS", "Greater Western Sydney", "GWSG", "GWS"),
    "Hawthorn" = c("Hawthorn", "Hawthorn Hawks", "HAW", "HAW"),
    "Melbourne" = c("Melbourne", "Melbourne Demons", "MELB", "MEL"),
    "North Melbourne" = c("North Melbourne", "North Melbourne Kangaroos", "NMFC", "NM", "NTH"),
    "Port Adelaide" = c("Port Adelaide", "Port Adelaide Power", "PORT", "PA", "PAP"),
    "Richmond" = c("Richmond", "Richmond Tigers", "RICH", "RIC"),
    "St Kilda" = c("St Kilda", "St Kilda Saints", "STK", "SK", "STK"),
    "Sydney Swans" = c("Sydney Swans", "Sydney", "SYD", "SYD"),
    "West Coast Eagles" = c("West Coast Eagles", "West Coast", "WCE", "WC", "WCE"),
    "Western Bulldogs" = c("Western Bulldogs", "Bulldogs", "WB", "WBD", "BULL")
  )

  # Create reverse mapping
  reverse_mapping <- list()
  for (standard_name in names(team_mapping)) {
    for (variant in team_mapping[[standard_name]]) {
      reverse_mapping[[variant]] <- standard_name
    }
  }

  # Clean team names
  cleaned_names <- sapply(team_names, function(name) {
    if (is.na(name)) return(NA_character_)

    # Trim whitespace and handle case
    clean_name <- trimws(name)

    # Direct match
    if (clean_name %in% names(reverse_mapping)) {
      return(reverse_mapping[[clean_name]])
    }

    # Case-insensitive match
    matches <- names(reverse_mapping)[tolower(names(reverse_mapping)) == tolower(clean_name)]
    if (length(matches) > 0) {
      return(reverse_mapping[[matches[1]]])
    }

    # Return original if no match found
    warning(paste("No mapping found for team name:", name))
    return(clean_name)
  })

  return(unname(cleaned_names))
}

#' Process and Clean AFLW Match Results
#'
#' Clean and standardize match results data from fitzRoy.
#'
#' @param results Raw results data frame from fitzRoy
#' @return Cleaned and standardized results data frame
#' @export
#' @examples
#' \dontrun{
#' raw_results <- scrape_aflw_results(seasons = 2023)
#' clean_results <- process_aflw_results(raw_results)
#' }
process_aflw_results <- function(results) {

  if (nrow(results) == 0) {
    warning("Empty results data frame provided")
    return(results)
  }

  # Make a copy to avoid modifying original
  clean_results <- results

  # Map fitzRoy column names to standard names
  if ("match.homeTeam.name" %in% names(clean_results)) {
    clean_results$home_team <- clean_team_names(clean_results$`match.homeTeam.name`)
  }
  if ("match.awayTeam.name" %in% names(clean_results)) {
    clean_results$away_team <- clean_team_names(clean_results$`match.awayTeam.name`)
  }
  if ("match.date" %in% names(clean_results)) {
    clean_results$date <- lubridate::as_date(clean_results$`match.date`)
  }
  if ("venue.name" %in% names(clean_results)) {
    clean_results$venue <- trimws(clean_results$`venue.name`)
  }
  if ("round.year" %in% names(clean_results)) {
    clean_results$season <- as.numeric(clean_results$`round.year`)
  }
  if ("round.roundNumber" %in% names(clean_results)) {
    clean_results$round_number <- as.numeric(clean_results$`round.roundNumber`)
  }

  # Extract scores
  if ("homeTeamScore.matchScore.totalScore" %in% names(clean_results)) {
    clean_results$home_score <- as.numeric(clean_results$`homeTeamScore.matchScore.totalScore`)
  }
  if ("awayTeamScore.matchScore.totalScore" %in% names(clean_results)) {
    clean_results$away_score <- as.numeric(clean_results$`awayTeamScore.matchScore.totalScore`)
  }

  # Calculate margin and total score
  if ("home_score" %in% names(clean_results) && "away_score" %in% names(clean_results)) {
    clean_results$margin <- clean_results$home_score - clean_results$away_score
    clean_results$total_score <- clean_results$home_score + clean_results$away_score
  }

  # Determine winner
  if ("margin" %in% names(clean_results) && "home_team" %in% names(clean_results) && "away_team" %in% names(clean_results)) {
    clean_results$winner <- ifelse(clean_results$margin > 0,
                                   clean_results$home_team,
                                   ifelse(clean_results$margin < 0,
                                          clean_results$away_team,
                                          "Draw"))
  }

  # Add match_id if not present
  if (!"match_id" %in% names(clean_results) &&
      "season" %in% names(clean_results) &&
      "round_number" %in% names(clean_results) &&
      "home_team" %in% names(clean_results) &&
      "away_team" %in% names(clean_results)) {
    clean_results$match_id <- paste(
                                    clean_results$date,
                                    fitzRoy::parse_team_abbr(clean_results$home_team),
                                    fitzRoy::parse_team_abbr(clean_results$away_team),
                                    sep = "_")
  }

  return(clean_results)
}

#' Process and Clean AFLW Player Statistics
#'
#' Clean and standardize player statistics data from fitzRoy.
#'
#' @param player_stats Raw player statistics data frame from fitzRoy
#' @return Cleaned and standardized player statistics data frame
#' @export
#' @examples
#' \dontrun{
#' raw_stats <- scrape_aflw_player_stats(seasons = 2023)
#' clean_stats <- process_aflw_player_stats(raw_stats)
#' }
process_aflw_player_stats <- function(player_stats) {

  if (nrow(player_stats) == 0) {
    warning("Empty player stats data frame provided")
    return(player_stats)
  }

  # Make a copy
  clean_stats <- player_stats

  # Map fitzRoy column names to standard names
  if ("team.name" %in% names(clean_stats)) {
    clean_stats$team <- clean_team_names(clean_stats$`team.name`)
  } else if ("home.team.name" %in% names(clean_stats) && "away.team.name" %in% names(clean_stats)) {
    # For some data, we need to determine which team the player belongs to
    # This is more complex and may require additional logic
    clean_stats$team <- ifelse(!is.na(clean_stats$`home.team.name`),
                              clean_team_names(clean_stats$`home.team.name`),
                              clean_team_names(clean_stats$`away.team.name`))
  }

  # Create player name from given name and surname
  if ("player.givenName" %in% names(clean_stats) && "player.surname" %in% names(clean_stats)) {
    clean_stats$player_name <- paste(trimws(clean_stats$`player.givenName`),
                                    trimws(clean_stats$`player.surname`))
    # Remove any special characters that might cause issues
    clean_stats$player_name <- gsub("[^A-Za-z0-9 '-]", "", clean_stats$player_name)
  } else if ("player.player.player.givenName" %in% names(clean_stats) && "player.player.player.surname" %in% names(clean_stats)) {
    clean_stats$player_name <- paste(trimws(clean_stats$`player.player.player.givenName`),
                                    trimws(clean_stats$`player.player.player.surname`))
    # Remove any special characters that might cause issues
    clean_stats$player_name <- gsub("[^A-Za-z0-9 '-]", "", clean_stats$player_name)
  }

  # Map other important columns
  if ("round.roundNumber" %in% names(clean_stats)) {
    clean_stats$round_number <- as.numeric(clean_stats$`round.roundNumber`)
  }
  if ("compSeason.shortName" %in% names(clean_stats)) {
    clean_stats$season <- as.numeric(clean_stats$`compSeason.shortName`)
  }

  # Ensure all statistical columns are numeric
  stat_columns <- c(
    "kicks", "handballs", "disposals", "marks", "goals", "behinds",
    "tackles", "hitouts", "clangers", "frees_for", "frees_against",
    "contested_possessions", "uncontested_possessions", "effective_disposals",
    "disposal_efficiency", "metres_gained", "intercepts", "inside_fifties",
    "goal_accuracy", "rating_points", "supercoach_score", "afl_fantasy_score"
  )

  for (col in stat_columns) {
    if (col %in% names(clean_stats)) {
      clean_stats[[col]] <- as.numeric(clean_stats[[col]])
      # Replace any infinite or missing values with 0 for counting stats
      if (col %in% c("kicks", "handballs", "disposals", "marks", "goals", "behinds", "tackles")) {
        clean_stats[[col]][is.na(clean_stats[[col]]) | is.infinite(clean_stats[[col]])] <- 0
      }
    }
  }

  # Calculate derived statistics
  if ("kicks" %in% names(clean_stats) && "handballs" %in% names(clean_stats)) {
    if (!"disposals" %in% names(clean_stats) || any(is.na(clean_stats$disposals))) {
      clean_stats$disposals <- clean_stats$kicks + clean_stats$handballs
    }
  }

  if ("goals" %in% names(clean_stats) && "behinds" %in% names(clean_stats)) {
    clean_stats$total_score <- clean_stats$goals * 6 + clean_stats$behinds
    clean_stats$scoring_shots <- clean_stats$goals + clean_stats$behinds

    # Goal accuracy (avoid division by zero)
    clean_stats$goal_accuracy <- ifelse(clean_stats$scoring_shots > 0,
                                        clean_stats$goals / clean_stats$scoring_shots * 100,
                                        NA_real_)
  }

  # Add unique player identifier
  if (!"player_id" %in% names(clean_stats) &&
      "player_name" %in% names(clean_stats) &&
      "team" %in% names(clean_stats)) {
    clean_stats$player_id <- paste(clean_stats$player_name,
                                   clean_stats$team,
                                   sep = "_")
  }

  # Create match identifier - more complex for player stats as we need to determine home/away teams
  if (!"match_id" %in% names(clean_stats)) {
    if ("season" %in% names(clean_stats) && "round_number" %in% names(clean_stats)) {
      if ("home.team.name" %in% names(clean_stats) && "away.team.name" %in% names(clean_stats)) {
        # Use the actual home/away team names from the data
        clean_stats$match_id <- paste(
                                     as.Date(clean_stats$utcStartTime),
                                     fitzRoy::parse_team_abbr((clean_stats$`home.team.name`)),
                                     fitzRoy::parse_team_abbr((clean_stats$`away.team.name`)),
                                     sep = "_")
      }
    }
  }

  return(clean_stats)
}

#' Merge Match Results with Player Statistics
#'
#' Combine match results and player statistics data on common match identifiers.
#' Automatically adds derived columns: team_result, home_away, opponent, and year_season.
#'
#' @param results Cleaned match results data frame
#' @param player_stats Cleaned player statistics data frame
#' @return Merged data frame with both match and player information, including:
#'   - team_result: "Win", "Loss", or "Draw"
#'   - home_away: "Home" or "Away"
#'   - opponent: The opposing team
#'   - year_season: Year extracted from date (e.g., 2024)
#' @export
#' @examples
#' \dontrun{
#' results <- process_aflw_results(scrape_aflw_results(2023))
#' stats <- process_aflw_player_stats(scrape_aflw_player_stats(2023))
#' merged_data <- merge_results_stats(results, stats)
#' }
merge_results_stats <- function(results, player_stats) {

  # Check if data frames are empty
  if (nrow(results) == 0) {
    warning("Results data frame is empty")
    return(data.frame())
  }
  if (nrow(player_stats) == 0) {
    warning("Player stats data frame is empty")
    return(data.frame())
  }

  # Check if both data frames have the required columns
  if (!"match_id" %in% names(results)) {
    stop("Results data frame must have 'match_id' column")
  }
  if (!"match_id" %in% names(player_stats)) {
    stop("Player stats data frame must have 'match_id' column")
  }

  # Merge data frames
  merged_data <- dplyr::left_join(player_stats, results,
                                  by = "match_id",
                                  suffix = c("", "_match"))

  # Add team result indicator
  if ("team" %in% names(merged_data) && "winner" %in% names(merged_data)) {
    merged_data$team_result <- ifelse(merged_data$team == merged_data$winner, "Win",
                                      ifelse(merged_data$winner == "Draw", "Draw", "Loss"))
  }

  # Add home/away indicator
  if ("team" %in% names(merged_data) &&
      "home_team" %in% names(merged_data) &&
      "away_team" %in% names(merged_data)) {
    merged_data$home_away <- ifelse(merged_data$team == merged_data$home_team, "Home",
                                    ifelse(merged_data$team == merged_data$away_team, "Away", NA))
  }

  # Add opponent indicator
  if ("home_away" %in% names(merged_data) &&
      "home_team" %in% names(merged_data) &&
      "away_team" %in% names(merged_data)) {
    merged_data$opponent <- ifelse(merged_data$home_away == "Home",
                                  merged_data$away_team,
                                  merged_data$home_team)
  }

  # Add year_season from date
  if ("date" %in% names(merged_data)) {
    merged_data$year_season <- as.integer(format(merged_data$date, "%Y"))
  }

  merged_data <- merged_data %>%
    janitor::clean_names()

  return(merged_data)
}

#' Validate AFLW Data Quality
#'
#' Check data quality and consistency in processed AFLW data.
#'
#' @param data Processed AFLW data frame
#' @param type Type of data ("results", "player_stats", or "merged")
#' @return List containing validation results and any issues found
#' @export
#' @examples
#' \dontrun{
#' validation <- validate_aflw_data(clean_results, type = "results")
#' print(validation$summary)
#' }
validate_aflw_data <- function(data, type = "merged") {

  validation_results <- list(
    type = type,
    n_rows = nrow(data),
    n_cols = ncol(data),
    issues = list(),
    summary = character()
  )

  if (type %in% c("results", "merged")) {
    # Check for missing match results
    if ("home_score" %in% names(data)) {
      missing_scores <- sum(is.na(data$home_score) | is.na(data$away_score))
      if (missing_scores > 0) {
        validation_results$issues$missing_scores <- missing_scores
      }
    }

    # Check for invalid margins
    if ("margin" %in% names(data) && "home_score" %in% names(data)) {
      expected_margin <- data$home_score - data$away_score
      margin_mismatch <- sum(abs(data$margin - expected_margin) > 0.1, na.rm = TRUE)
      if (margin_mismatch > 0) {
        validation_results$issues$margin_mismatch <- margin_mismatch
      }
    }
  }

  if (type %in% c("player_stats", "merged")) {
    # Check for impossible statistics
    if ("disposal_efficiency" %in% names(data)) {
      invalid_efficiency <- sum(data$disposal_efficiency > 100 | data$disposal_efficiency < 0, na.rm = TRUE)
      if (invalid_efficiency > 0) {
        validation_results$issues$invalid_disposal_efficiency <- invalid_efficiency
      }
    }

    if ("goal_accuracy" %in% names(data)) {
      invalid_accuracy <- sum(data$goal_accuracy > 100 | data$goal_accuracy < 0, na.rm = TRUE)
      if (invalid_accuracy > 0) {
        validation_results$issues$invalid_goal_accuracy <- invalid_accuracy
      }
    }

    # Check for negative counting stats
    counting_stats <- c("kicks", "handballs", "disposals", "marks", "goals", "behinds", "tackles")
    for (stat in counting_stats) {
      if (stat %in% names(data)) {
        negative_values <- sum(data[[stat]] < 0, na.rm = TRUE)
        if (negative_values > 0) {
          validation_results$issues[[paste0("negative_", stat)]] <- negative_values
        }
      }
    }
  }

  # Generate summary
  if (length(validation_results$issues) == 0) {
    validation_results$summary <- "Data validation passed - no issues found."
  } else {
    issue_count <- length(validation_results$issues)
    validation_results$summary <- paste("Data validation found", issue_count, "potential issues.")
  }

  validation_results <- validation_results %>%
    janitor::clean_names()

  return(validation_results)
}
