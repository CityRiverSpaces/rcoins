#' Utility functions to compare rcoins stroke to momepy COINS.
#'
#' The bucharest data is used for this comparison.
library(sf)
library(CRiSpData)
library(rcoins)

prepare_stroke_data <- function() {
  st_write(
    bucharest_osm$river_centerline,
    dsn = "bucharest.gpkg",
    layer = "river",
    delete_dsn = TRUE
  )
  st_write(
    bucharest_osm$streets,
    dsn = "bucharest.gpkg",
    layer = "streets"
  )

  strokes <- stroke(bucharest_osm$streets)
  st_write(
    strokes,
    dsn = "bucharest.gpkg",
    layer = "strokes"
  )
}

prepare_stroke_data()
