#' Get example OSM data
#'
#' This function retrieves example OpenStreetMap (OSM) data from the
#' Zenodo data repository, and it can be used in examples and tests. The code
#' used to generate the example dataset is available at
#' https://github.com/CityRiverSpaces/CRiSpExampleData. Note that the example
#' dataset is cached locally, so that subsequent calls to the function can
#' load the example data from disk without having to re-download the data.
#'
#' @param force_download Download data even if cached data is available
#' @return A list of sf objects containing the OSM data as [`sf::sfc`]
#'   objects.
#' @importFrom utils download.file
#' @importFrom stats setNames
#' @export
#'
#' @examplesIf interactive()
#' get_osm_example_data(force_download = TRUE)
get_example_data <- function(force_download = FALSE) {
  file <- get_example_data_file("bucharest_osm.gpkg",
                                force_download = force_download)
  names <- sf::st_layers(file)$name
  lapply(names, \(layer) sf::st_read(file, layer = layer, quiet = TRUE)) |>
    setNames(names)
}

#' Retrieve an example data file from the data repository
#'
#' Store the file in the cache directory, for subsequent reuse.
#'
#' @param force_download Download data even if cached data is available
#' @return A character string representing the file path
#' @keywords internal
get_example_data_file <- function(filename, force_download = FALSE) {
  if (force_download) {
    download_url <- get_download_url(filename)
    # temporarily increase timeout, reset value on exit
    op <- options(timeout = 120)
    on.exit(options(op))
    data <- retry(download.file, url = download_url, mode = "wb", quiet = TRUE)
  }

  data
}

#' Form the URL to download a given file from the Zenodo data repository
#'
#' @noRd
get_download_url <- function(filename) {
  paste(zenodo_record_url, "files", filename, sep = "/")
}

#' Retry function call, for interaction with APIs and external services
#'
#' @noRd
retry <- function(func, ..., max_retries = 5, delay = 2) {
  attempt <- 1
  while (attempt <= max_retries) {
    result <- tryCatch({
      func(...)  # Call the function with arguments
    }, error = function(e) {
      warning(sprintf("Attempt %d failed: %s", attempt, e$message))
      NULL
    })

    if (!is.null(result)) {
      return(result)  # Successfully retrieved result
    }

    message(sprintf("Retrying in %d seconds...", delay))
    Sys.sleep(delay)
    attempt <- attempt + 1
  }

  stop("Function failed after multiple attempts.")
}

#' Example data files that can be used in examples and tests are stored in
#' a Zenodo data repository (DOI: 10.4121/f5d5e118-b5bd-4dfb-987f-fe10d1b9b386).
#' Files can be downloaded programmatically from the following URL.
#'
#' @noRd
zenodo_record_url <- "https://zenodo.org/records/16325879"
