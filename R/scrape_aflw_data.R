#' Scrape AFLW Match Results
#'
#' Fetch match results for specified AFLW seasons and rounds using fitzRoy.
#'
#' @param seasons Numeric vector of seasons to scrape (e.g., 2020:2024)
#' @param rounds Numeric vector of rounds to scrape (default: all rounds)
#' @param source Character string specifying data source ("AFL" or "afltables")
#' @return Data frame with match results including teams, scores, venue, date
#' @export
#' @examples
#' \dontrun{
#' # Get all matches from 2023 season
#' results_2023 <- scrape_aflw_results(seasons = 2023)
#'
#' # Get specific rounds from multiple seasons
#' results <- scrape_aflw_results(seasons = 2022:2023, rounds = 1:5)
#' }
scrape_aflw_results <- function(seasons, rounds = NULL, source = "AFL") {

  # Input validation
  if (!is.numeric(seasons) || any(seasons < 2017)) {
    stop("Seasons must be numeric and >= 2017 (first AFLW season)")
  }

  if (!source %in% c("AFL", "afltables")) {
    stop("Source must be either 'AFL' or 'afltables'")
  }

  # Initialize results list
  all_results <- list()

  # Loop through seasons
  for (season in seasons) {
    cat("Scraping season", season, "...\n")

    tryCatch({
      if (is.null(rounds)) {
        # Get all rounds for the season
        season_results <- fitzRoy::fetch_results(
          season = season,
          comp = "AFLW",
          source = source
        )
      } else {
        # Get specific rounds
        round_results <- list()
        for (round in rounds) {
          round_data <- fitzRoy::fetch_results(
            season = season,
            round_number = round,
            comp = "AFLW",
            source = source
          )
          round_results[[length(round_results) + 1]] <- round_data
        }
        season_results <- dplyr::bind_rows(round_results)
      }

      if (nrow(season_results) > 0) {
        all_results[[length(all_results) + 1]] <- season_results
      } else {
        cat("Warning: No results found for season", season, "\n")
      }
    }, error = function(e) {
      cat("Warning: Could not fetch results for season", season, "-", e$message, "\n")
    })
  }

  # Combine all results
  final_results <- dplyr::bind_rows(all_results)

  cat("Successfully scraped", nrow(final_results), "matches\n")
  return(final_results)
}

#' Scrape AFLW Player Statistics
#'
#' Fetch detailed player statistics for specified AFLW seasons and rounds.
#'
#' @param seasons Numeric vector of seasons to scrape
#' @param rounds Numeric vector of rounds to scrape (default: all rounds)
#' @param source Character string specifying data source
#' @return Data frame with detailed player statistics
#' @export
#' @examples
#' \dontrun{
#' # Get player stats for 2023 season
#' player_stats <- scrape_aflw_player_stats(seasons = 2023)
#'
#' # Get stats for specific rounds
#' stats <- scrape_aflw_player_stats(seasons = 2023, rounds = 1:10)
#' }
scrape_aflw_player_stats <- function(seasons, rounds = NULL, source = "AFL") {

  # Input validation
  if (!is.numeric(seasons) || any(seasons < 2017)) {
    stop("Seasons must be numeric and >= 2017 (first AFLW season)")
  }

  # Initialize results
  all_stats <- list()

  # Loop through seasons
  for (season in seasons) {
    cat("Scraping player stats for season", season, "...\n")

    tryCatch({
      if (is.null(rounds)) {
        # Get all available player stats for the season
        season_stats <- fitzRoy::fetch_player_stats(
          season = season,
          comp = "AFLW",
          source = source
        )
      } else {
        # Get specific rounds
        round_stats <- list()
        for (round in rounds) {
          round_data <- fitzRoy::fetch_player_stats(
            season = season,
            round_number = round,
            comp = "AFLW",
            source = source
          )
          round_stats[[length(round_stats) + 1]] <- round_data
        }
        season_stats <- dplyr::bind_rows(round_stats)
      }

      if (nrow(season_stats) > 0) {
        all_stats[[length(all_stats) + 1]] <- season_stats
      } else {
        cat("Warning: No player stats found for season", season, "\n")
      }
    }, error = function(e) {
      cat("Warning: Could not fetch player stats for season", season, "-", e$message, "\n")
    })
  }

  # Combine all results
  final_stats <- dplyr::bind_rows(all_stats)

  cat("Successfully scraped stats for", length(unique(final_stats$player_id)), "players\n")
  return(final_stats)
}

#' Scrape AFLW Team Lineups
#'
#' Fetch team lineups for specified AFLW matches.
#'
#' @param seasons Numeric vector of seasons to scrape
#' @param rounds Numeric vector of rounds to scrape (default: all rounds)
#' @param source Character string specifying data source
#' @return Data frame with team lineups
#' @export
#' @examples
#' \dontrun{
#' # Get lineups for 2023 season
#' lineups <- scrape_aflw_lineups(seasons = 2023)
#' }
scrape_aflw_lineups <- function(seasons, rounds = NULL, source = "AFL") {

  # Input validation
  if (!is.numeric(seasons) || any(seasons < 2017)) {
    stop("Seasons must be numeric and >= 2017 (first AFLW season)")
  }

  # Initialize results
  all_lineups <- list()

  # Loop through seasons
  for (season in seasons) {
    cat("Scraping lineups for season", season, "...\n")

    if (is.null(rounds)) {
      # Get all lineups for the season
      tryCatch({
        season_lineups <- fitzRoy::fetch_lineup(
          season = season,
          comp = "AFLW",
          source = source
        )
        all_lineups[[length(all_lineups) + 1]] <- season_lineups
      }, error = function(e) {
        cat("Warning: Could not fetch lineups for season", season, "\n")
      })
    } else {
      # Get lineups for specific rounds
      season_lineups <- list()
      for (round in rounds) {
        tryCatch({
          round_lineups <- fitzRoy::fetch_lineup(
            season = season,
            round_number = round,
            comp = "AFLW",
            source = source
          )
          season_lineups[[length(season_lineups) + 1]] <- round_lineups
        }, error = function(e) {
          cat("Warning: Could not fetch lineups for season", season, "round", round, "\n")
        })
      }

      if (length(season_lineups) > 0) {
        all_lineups[[length(all_lineups) + 1]] <- dplyr::bind_rows(season_lineups)
      }
    }
  }

  # Combine all results
  if (length(all_lineups) > 0) {
    final_lineups <- dplyr::bind_rows(all_lineups)
    cat("Successfully scraped lineups for", nrow(final_lineups), "player selections\n")
    return(final_lineups)
  } else {
    warning("No lineup data found")
    return(data.frame())
  }
}

#' Scrape AFLW Ladder/Standings
#'
#' Fetch ladder standings for specified AFLW seasons.
#'
#' @param seasons Numeric vector of seasons to scrape
#' @param rounds Numeric vector of rounds to scrape (default: final ladder)
#' @param source Character string specifying data source
#' @return Data frame with ladder standings
#' @export
#' @examples
#' \dontrun{
#' # Get final ladder for 2023
#' ladder <- scrape_aflw_ladder(seasons = 2023)
#'
#' # Get ladder progression through season
#' ladder_rounds <- scrape_aflw_ladder(seasons = 2023, rounds = 1:10)
#' }
scrape_aflw_ladder <- function(seasons, rounds = NULL, source = "AFL") {

  # Input validation
  if (!is.numeric(seasons) || any(seasons < 2017)) {
    stop("Seasons must be numeric and >= 2017 (first AFLW season)")
  }

  # Initialize results
  all_ladders <- list()

  # Loop through seasons
  for (season in seasons) {
    cat("Scraping ladder for season", season, "...\n")

    tryCatch({
      if (is.null(rounds)) {
        # Get final ladder
        season_ladder <- fitzRoy::fetch_ladder(
          season = season,
          comp = "AFLW",
          source = source
        )
      } else {
        # Get ladder for specific rounds
        round_ladders <- list()
        for (round in rounds) {
          round_ladder <- fitzRoy::fetch_ladder(
            season = season,
            round_number = round,
            comp = "AFLW",
            source = source
          )
          round_ladders[[length(round_ladders) + 1]] <- round_ladder
        }
        season_ladder <- dplyr::bind_rows(round_ladders)
      }

      if (nrow(season_ladder) > 0) {
        all_ladders[[length(all_ladders) + 1]] <- season_ladder
      } else {
        cat("Warning: No ladder data found for season", season, "\n")
      }
    }, error = function(e) {
      cat("Warning: Could not fetch ladder for season", season, "-", e$message, "\n")
    })
  }

  # Combine all results
  final_ladder <- dplyr::bind_rows(all_ladders)

  cat("Successfully scraped ladder data\n")
  return(final_ladder)
}

#' Comprehensive AFLW Data Collection
#'
#' Fetch all available AFLW data for specified seasons (results, player stats, lineups, ladder).
#'
#' @param seasons Numeric vector of seasons to scrape
#' @param rounds Numeric vector of rounds to scrape (default: all rounds)
#' @param include_lineups Logical, whether to include lineup data (can be slow)
#' @param source Character string specifying data source
#' @return Named list containing results, player_stats, lineups (if requested), and ladder
#' @export
#' @examples
#' \dontrun{
#' # Get all data for 2023 season
#' aflw_data <- scrape_aflw_comprehensive(seasons = 2023)
#'
#' # Access different components
#' results <- aflw_data$results
#' player_stats <- aflw_data$player_stats
#' ladder <- aflw_data$ladder
#' }
scrape_aflw_comprehensive <- function(seasons,
                                      rounds = NULL,
                                      include_lineups = TRUE,
                                      source = "AFL") {

  cat("Starting comprehensive AFLW data collection...\n")

  # Scrape match results
  cat("\n=== MATCH RESULTS ===\n")
  results <- scrape_aflw_results(seasons = seasons, rounds = rounds, source = source)

  # Scrape player statistics
  cat("\n=== PLAYER STATISTICS ===\n")
  player_stats <- scrape_aflw_player_stats(seasons = seasons, rounds = rounds, source = source)

  # Scrape ladder
  cat("\n=== LADDER ===\n")
  ladder <- scrape_aflw_ladder(seasons = seasons, rounds = rounds, source = source)

  # Prepare output
  output <- list(
    results = results,
    player_stats = player_stats,
    ladder = ladder
  )

  # Optionally include lineups
  if (include_lineups) {
    cat("\n=== LINEUPS ===\n")
    lineups <- scrape_aflw_lineups(seasons = seasons, rounds = rounds, source = source)
    output$lineups <- lineups
  }

  cat("\n=== DATA COLLECTION COMPLETE ===\n")
  cat("Collected data for", length(seasons), "seasons\n")
  cat("Match results:", nrow(results), "matches\n")
  cat("Player statistics:", nrow(player_stats), "records\n")
  cat("Ladder data:", nrow(ladder), "records\n")
  if (include_lineups) {
    cat("Lineup data:", nrow(output$lineups), "player selections\n")
  }

  return(output)
}