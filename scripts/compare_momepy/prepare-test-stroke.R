#' Utility functions to compare rcoins stroke to momepy COINS.
#'
#' The bucharest data is used for this comparison.
library(sf)
library(rcoins)

prepare_stroke_data <- function() {
  st_write(
    bucharest$river,
    dsn = "data/bucharest.gpkg",
    layer = "river",
    delete_dsn = TRUE
  )
  st_write(
    bucharest$streets,
    dsn = "data/bucharest.gpkg",
    layer = "streets"
  )

  strokes <- stroke(bucharest$streets)
  st_write(
    strokes,
    dsn = "data/bucharest.gpkg",
    layer = "strokes"
  )
}

prepare_stroke_data()
