library(dplyr)
library(osmdata)
library(rlang)
library(sf)
library(usethis)


get_osm_bb <- function(city_name) {
  bb <- getbb(city_name)
  bb <- bb |> as.vector()
  names(bb) <- c("xmin", "ymin", "xmax", "ymax")
  bb <- st_bbox(bb, crs = 4326)
  return(bb)
}

osmdata_as_sf <- function(key, value, bb) {
  bb |>
    opq() |>
    add_osm_feature(key = key, value = value) |>
    osmdata_sf()
}

get_osm_streets <- function(bb, crs, highway_values = NULL) {
  if (is.null(highway_values)) {
    highway_values <- c("motorway", "trunk", "primary", "secondary", "tertiary")
  }

  link_values <- sapply(X = highway_values,
                        FUN = \(x) sprintf("%s_link", x),
                        USE.NAMES = FALSE)

  streets <- osmdata_as_sf("highway", c(highway_values, link_values), bb)

  # Cast polygons (closed streets) into lines
  poly_to_lines <- suppressWarnings(
    streets$osm_polygons |> st_cast("LINESTRING")
  )

  # Combine all features in one data frame
  streets_lines <- streets$osm_lines |>
    bind_rows(poly_to_lines) |>
    select("highway") |>
    rename(!!sym("type") := !!sym("highway")) |>
    st_transform(crs)

  return(streets_lines)
}

get_osm_river <- function(river_name, bb, crs) {
  # Get the river centreline
  river_centerline <- osmdata_as_sf("waterway", "river", bb)
  river_centerline <- river_centerline$osm_multilines |>
    filter(.data$name == river_name) |>
    st_transform(crs) |>
    st_geometry()

  return(river_centerline)
}

get_osmdata <- function(city_name, river_name, crs, buffer = NULL) {
  bb <- get_osm_bb(city_name)

  if (!is.null(buffer)) {
    bb <- bb |>
      st_as_sfc() |>
      st_transform(crs = crs) |>
      st_buffer(buffer) |>
      st_transform(crs = 4326) |>
      st_bbox()
  }

  streets <- get_osm_streets(bb, crs)
  river <- get_osm_river(river_name, bb, crs)

  osm_data <- list(
    bb = bb,
    river_centerline = river,
    streets = streets
  )

  return(osm_data)
}


# Set the parameters
city_name <- "Bucharest"
river_name <- "Dâmbovița"
epsg_code <- 32635
bbox_buffer <- 2000 # m

# Fetch the data
bucharest <- get_osmdata(
  city_name,
  river_name,
  crs = epsg_code,
  buffer = bbox_buffer
)

# Fix encoding issue in the WKT string of city boundary
fix_wkt_encoding <- function(x) {
  wkt <- st_crs(x)$wkt
  st_crs(x)$wkt <- gsub("°|º", "\\\u00b0", wkt)
  x
}
bucharest <- lapply(bucharest, fix_wkt_encoding)

# Save as package data
use_data(bucharest, overwrite = TRUE)
