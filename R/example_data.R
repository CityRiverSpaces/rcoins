#' Get example data
#'
#' This function retrieves example OpenStreetMap (OSM) data for the city of
#' Bucharest, Romania, from a persistent URL on the 4TU.ResearchData data
#' repository. The dataset includes the street network and the geometry of the
#' Dâmbovița river.
#'
#' @return A list of sf objects containing the OSM data.
#' @importFrom utils download.file
#' @importFrom stats setNames
#' @export
#'
#' @examplesIf interactive()
#' get_example_data()
get_example_data <- function() {
  # nolint start
  url_osm <- "https://data.4tu.nl/file/f5d5e118-b5bd-4dfb-987f-fe10d1b9b386/f519315e-b92d-4815-b924-3175bd2a7a61"
  # nolint end
  temp_file <- tempfile(fileext = ".gpkg")
  download.file(url_osm, destfile = temp_file, mode = "wb", quiet = TRUE)
  layers <- c("streets", "river_centerline")
  sapply(layers, \(x) sf::st_read(temp_file, layer = x, quiet = TRUE))
}
