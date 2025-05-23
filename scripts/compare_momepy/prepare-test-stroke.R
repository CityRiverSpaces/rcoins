#' Utility functions to compare rcoins stroke to momepy COINS.
#'
#' The bucharest example data is used for this comparison.
library(sf)
library(rcoins)

prepare_stroke_data <- function() {
  bucharest <- get_example_data()

  st_write(bucharest$streets, dsn = "bucharest.gpkg", layer = "streets")

  strokes <- stroke(bucharest$streets)

  st_write(strokes, dsn = "bucharest.gpkg", layer = "strokes")
}

prepare_stroke_data()
