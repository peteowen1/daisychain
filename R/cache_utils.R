#' Load Data from Cache
#'
#' Loads cached data from RDS files if the object doesn't exist in the environment.
#' Can load from the current script's cache or from previous scripts' caches.
#'
#' @param object_name Character string of the object name to check/load
#' @param cache_file Path to the cache file to load from
#' @param extract_name If the cache contains a list, the name of the element to extract.
#'   If NULL (default), loads the entire object.
#' @param envir Environment to check for existing object and load into. Defaults to parent frame.
#' @param verbose Logical. If TRUE (default), prints status messages.
#'
#' @return Logical. TRUE if data was loaded from cache, FALSE if object already existed or cache not found.
#'
#' @examples
#' \dontrun{
#' # Load merged_data from cache if it doesn't exist
#' load_cache("merged_data", "analysis/cache/01_data_collection.rds", extract_name = "merged_data")
#'
#' # Load entire object
#' load_cache("player_prediction_results", "analysis/cache/03_player_prediction_model.rds")
#' }
#'
#' @export
load_cache <- function(object_name, cache_file, extract_name = NULL,
                       envir = parent.frame(), verbose = TRUE) {

  # Check if object already exists in environment
  if (exists(object_name, envir = envir)) {
    if (verbose) {
      cat(sprintf("Object '%s' already exists in environment\n", object_name))
    }
    return(FALSE)
  }

  # Check if cache file exists
  if (!file.exists(cache_file)) {
    if (verbose) {
      cat(sprintf("Cache file not found: %s\n", cache_file))
    }
    return(FALSE)
  }

  # Load from cache
  if (verbose) {
    cat(sprintf("Loading '%s' from cache: %s\n", object_name, cache_file))
  }

  cached_data <- readRDS(cache_file)

  # Extract specific element if requested
  if (!is.null(extract_name)) {
    if (is.list(cached_data) && extract_name %in% names(cached_data)) {
      assign(object_name, cached_data[[extract_name]], envir = envir)
    } else {
      stop(sprintf("Cache element '%s' not found in %s", extract_name, cache_file))
    }
  } else {
    assign(object_name, cached_data, envir = envir)
  }

  if (verbose) {
    cat(sprintf("Successfully loaded '%s' from cache\n", object_name))
  }

  return(TRUE)
}


#' Save Data to Cache
#'
#' Saves data to an RDS cache file.
#'
#' @param data Data to save. Can be a single object or a named list of objects.
#' @param cache_file Path to save the cache file
#' @param verbose Logical. If TRUE (default), prints status messages.
#'
#' @return Invisible NULL
#'
#' @examples
#' \dontrun{
#' # Save multiple objects as a list
#' save_cache(
#'   list(merged_data = merged_data, clean_results = clean_results),
#'   "analysis/cache/01_data_collection.rds"
#' )
#'
#' # Save single object
#' save_cache(player_prediction_results, "analysis/cache/03_player_prediction_model.rds")
#' }
#'
#' @export
save_cache <- function(data, cache_file, verbose = TRUE) {

  # Create cache directory if it doesn't exist
  cache_dir <- dirname(cache_file)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    if (verbose) {
      cat(sprintf("Created cache directory: %s\n", cache_dir))
    }
  }

  # Save to cache
  if (verbose) {
    cat(sprintf("Saving data to cache: %s\n", cache_file))
  }

  saveRDS(data, cache_file)

  if (verbose) {
    cat(sprintf("Successfully saved to %s\n", cache_file))
  }

  invisible(NULL)
}


#' Load Data from Current or Previous Script Cache
#'
#' Attempts to load data from cache, trying the current script first,
#' then falling back to previous scripts if specified.
#'
#' @param object_name Character string of the object name to check/load
#' @param current_cache Path to the current script's cache file
#' @param extract_from_current If loading from current cache (which may be a list),
#'   the name of the element to extract. If NULL, loads entire object.
#' @param fallback_caches Named list of fallback cache files to try.
#'   Names should be the object names to load, values should be cache file paths.
#' @param extract_from_fallback Named list matching fallback_caches structure,
#'   specifying which element to extract from each fallback cache.
#' @param required Logical. If TRUE, stops with error if data can't be loaded.
#'   If FALSE (default), returns FALSE if unable to load.
#' @param envir Environment to load into. Defaults to parent frame.
#' @param verbose Logical. If TRUE (default), prints status messages.
#'
#' @return Logical. TRUE if data was loaded, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' # Try to load enhanced_player_data from current or previous cache
#' load_with_fallback(
#'   object_name = "enhanced_player_data",
#'   current_cache = "analysis/cache/02_feature_engineering.rds",
#'   extract_from_current = "enhanced_player_data",
#'   fallback_caches = list(
#'     merged_data = "analysis/cache/01_data_collection.rds"
#'   ),
#'   extract_from_fallback = list(
#'     merged_data = "merged_data"
#'   ),
#'   required = FALSE
#' )
#' }
#'
#' @export
load_with_fallback <- function(object_name, current_cache,
                                extract_from_current = NULL,
                                fallback_caches = NULL,
                                extract_from_fallback = NULL,
                                required = FALSE,
                                envir = parent.frame(),
                                verbose = TRUE) {

  # Check if object already exists
  if (exists(object_name, envir = envir)) {
    if (verbose) {
      cat(sprintf("Object '%s' already exists in environment\n", object_name))
    }
    return(TRUE)
  }

  # Try loading from current cache
  loaded <- load_cache(object_name, current_cache, extract_from_current,
                       envir = envir, verbose = verbose)

  if (loaded) {
    return(TRUE)
  }

  # Try fallback caches
  if (!is.null(fallback_caches) && length(fallback_caches) > 0) {
    if (verbose) {
      cat(sprintf("Trying %d fallback cache(s) for '%s'\n",
                  length(fallback_caches), object_name))
    }

    for (fb_obj_name in names(fallback_caches)) {
      fb_cache_file <- fallback_caches[[fb_obj_name]]
      fb_extract <- if (!is.null(extract_from_fallback)) {
        extract_from_fallback[[fb_obj_name]]
      } else {
        NULL
      }

      # Load the fallback object with its original name
      fb_loaded <- load_cache(fb_obj_name, fb_cache_file, fb_extract,
                              envir = envir, verbose = verbose)

      if (fb_loaded) {
        return(TRUE)
      }
    }
  }

  # If we get here, couldn't load the data
  if (required) {
    stop(sprintf("Required object '%s' could not be loaded from cache", object_name))
  }

  if (verbose) {
    cat(sprintf("Could not load '%s' from any cache\n", object_name))
  }

  return(FALSE)
}
