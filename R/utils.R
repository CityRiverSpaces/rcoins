#' @noRd
to_sfc <- function(x) {
  sfc <- NULL
  if (inherits(x,  "sfc")) {
    sfc <- x
  } else if (inherits(x,  "sf")) {
    sfc <- sf::st_geometry(x)
  } else if (inherits(x,  "sfnetwork")) {
    sf <- sf::st_as_sf(x, "edges")
    sfc <- to_sfc(sf)
  } else {
    sfc <- sf::st_as_sfc(x)
  }
  return(sfc)
}

#' @noRd
check_geometry <- function(geometry) {
  # verify that we only have linestrings
  geometry_type <- sf::st_geometry_type(geometry)
  is_not_linestring <- geometry_type != "LINESTRING"
  if (any(is_not_linestring)) {
    template <- "Edges should be of type LINESTRING (a %s is provided)."
    stop(sprintf(template, geometry_type[is_not_linestring]))
  }
}
