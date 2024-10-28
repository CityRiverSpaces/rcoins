#' Identify continuous lines in a network
#'
#' Apply the Continuity in Street Network (COINS) method to identify
#' sequences of edges that form naturally continuous strokes in a network.
#'
#' @param edges An object of class \code{\link[sfc]{sfc}} (or compatible),
#' including the edge geometries (should be of type LineString or
#' MultiLineString).
#'
#' @param angle_threshold Consecutive line segments can be considered part of
#' the same stroke if the internal angle they form is larger than
#' \code{angle_threshold} (in degrees). It should fall in the range
#' \eqn{0 \leq angle_threshold < 180}.
#'
#' @param attributes If \code{TRUE}, return a label for each edge, representing
#' the groups each edge belongs to. Only possible for \code{flow_mode = TRUE}.
#'
#' @param flow_mode If \code{TRUE}, line segments that belong to the same edge
#' are not split across strokes (even if they form internal angles smaller than
#' \code{angle_threshold}).
#'
#' @param from_edge Only look for the continuous strokes that include the
#' provided edges or line segments.
#'
#' @return An object of class \code{\link[sfc]{sfc}} (if
#' \code{attributes = FALSE}), a vector with the same length as \code{edges}
#' otherwise.
#'
#' @export
stroke <- function(edges, angle_threshold = 0, attributes = FALSE,
                   flow_mode = FALSE, from_edge = NULL) {

  if (attributes && !flow_mode) {
    stop("Stroke attributes can be returned only if `flow_mode = TRUE`)")
  }

  if (attributes) stop("attribute mode not implemented.")
  if (flow_mode) stop("flow mode not implemented.")
  if (!is.null(from_edge)) stop("from_edge mode not implemented")

  edges_sfc <- to_sfc(edges)
  check_geometry(edges_sfc)

  # extract CRS from the edges
  crs <- sf::st_crs(edges_sfc)

  # split the edges into their constituent points
  edge_pts <- sfheaders::sfc_to_df(edges_sfc)

  # find unique points ("nodes") and assign them IDs
  nodes <- unique_nodes(edge_pts)

  # build array of line segments, referring to points using their IDs
  segments <- to_line_segments(edge_pts, nodes)

  # build connectivity table: for each node, find intersecting line segments
  links <- get_links(segments)

  # calculate interior angles between segment pairs, identify best links
  best_links <- best_link(nodes, segments, links, angle_threshold)

  # verify that the best links identified fulfill input requirements
  final_links <- cross_check_links(best_links, flow_mode)

  # merge line segments into strokes following the predetermined connectivity
  strokes  <- merge_lines(nodes, segments, final_links, from_edge)

  # add the CRS to the edges, done!
  sf::st_crs(strokes) <- crs
  return(strokes)
}

#' Find unique nodes and label them with IDs
#' @noRd
unique_nodes <- function(edge_points) {
  nodes <- dplyr::distinct(edge_points, x, y)
  nodes$node_id <- seq_len(nrow(nodes))
  return(nodes)
}

#' @noRd
to_line_segments <- function(points, nodes) {
  # label all the points using the node IDs
  points <- dplyr::left_join(points, nodes, by = c("x", "y"))

  # use the attribute `linestring_id` to identify edge start- and end-points
  is_startpoint <- !duplicated(points$linestring_id)
  is_endpoint <- !duplicated(points$linestring_id, fromLast = TRUE)
  # we build the array of line segments, with shape (nsegements, 2)
  # values are the node IDs
  start <- points[!is_endpoint, "node_id"]
  end <- points[!is_startpoint, "node_id"]
  segments <- cbind(start, end)
  return(segments)
}

#' @noRd
get_links <- function(segments) {
  nsegments <- nrow(segments)
  links <- data.frame(node_id = as.vector(segments)) |>
    dplyr::group_by(node_id) |>
    dplyr::group_rows()  |>
    lapply(function(x) (x - 1) %% nsegments + 1)
  return(links)
}

#' @noRd
best_link <- function(nodes, segments, links, angle_threshold = 0) {

  get_linked_segments <- function(segment_id, node_id) {
    # find the segments connected to the given one via the given node
    # 1. find all segments connected to the node
    segs <- links[[node_id]]
    # 2. exclude the given segment from the list
    is_current_segment <- segs == segment_id
    linked_segments <- segs[!is_current_segment]
    return(linked_segments)
  }

  get_linked_nodes <- function(node_id, segment_id) {
    # find the node connected to the given one via the given segment(s)
    # 1. get the nodes that are part of the given segment(s)
    nds <- segments[segment_id, ]
    # 2. flatten the array row by row (i.e. along the node dimension)
    nds <- as.vector(t(nds))
    # 3. exclude the given node from the list
    is_current_node <- nds %in% node_id
    linked_nodes <- nds[!is_current_node]
    return(linked_nodes)
  }

  get_angle <- function(vertex_id, p1_id, p2_id) {
    # calculate interior angle
    angle <- interior_angle(nodes[vertex_id, ], nodes[p1_id, ], nodes[p2_id, ])
    return(angle)
  }

  best_links <- array(integer(), dim = dim(segments))
  colnames(best_links) <- c("start", "end")

  angle_threshold_rad <- angle_threshold / 180 * pi  # convert to radians

  for (iseg in seq_len(nrow(segments))) {
    start_node <- segments[iseg, "start"]
    end_node <- segments[iseg, "end"]

    # find angles formed with all segments linked at start point
    linked_segs <- get_linked_segments(iseg, start_node)
    linked_nodes <- get_linked_nodes(start_node, linked_segs)
    angles <- get_angle(start_node, end_node, linked_nodes)
    best_link <- get_best_link(angles, linked_segs, angle_threshold_rad)
    if (length(best_link) > 0) best_links[iseg, "start"] <- best_link

    # find angles formed with all segments linked at end point
    linked_segs <- get_linked_segments(iseg, end_node)
    linked_nodes <- get_linked_nodes(end_node, linked_segs)
    angles <- get_angle(end_node, start_node, linked_nodes)
    best_link <- get_best_link(angles, linked_segs, angle_threshold_rad)
    if (length(best_link) > 0) best_links[iseg, "end"] <- best_link
  }
  return(best_links)
}

#' @noRd
interior_angle <- function(v, p1, p2) {
  # compute convex angle between three points:
  # p1--v--p2 ("v" is the vertex)
  dx1 <- p1$x - v$x
  dx2 <- p2$x - v$x
  dy1 <- p1$y - v$y
  dy2 <- p2$y - v$y
  dot_product <- dx1 * dx2 + dy1 * dy2
  norm1 <- sqrt(dx1^2 + dy1^2)
  norm2 <- sqrt(dx2^2 + dy2^2)
  cos_theta <- dot_product / (norm1 * norm2)
  angle <- acos(cos_theta)
  return(angle)
}

#' @noRd
get_best_link <- function(angles, links, angle_threshold = 0) {
  if (length(angles) == 0) return(NA)
  is_above_threshold <- angles > angle_threshold
  is_best_link <- which.max(angles[is_above_threshold])
  best_link <- links[is_best_link]
  return(best_link)
}

#' @noRd
cross_check_links <- function(best_links, flow_mode = FALSE) {

  links <- array(integer(), dim = dim(best_links))
  colnames(links) <- c("start", "end")

  # find the best link of the best links
  bl <- best_links[best_links[, "start"], ]
  # we check both ends to see whether the best link is reciprocal
  is_best_link <- bl == seq_len(nrow(bl))
  # if we have a match on either of the sides, we keep the link
  is_reciprocal <- apply(is_best_link, 1, any)
  is_reciprocal[is.na(is_reciprocal)] <- FALSE  # fix for NA values
  links[is_reciprocal, "start"] <- best_links[is_reciprocal, "start"]

  # exact same as above, for the other side
  bl <- best_links[best_links[, "end"], ]
  is_best_link <- bl == seq_len(nrow(bl))
  is_reciprocal <- apply(is_best_link, 1, any)
  is_reciprocal[is.na(is_reciprocal)] <- FALSE  # fix for NA values
  links[is_reciprocal, "end"] <- best_links[is_reciprocal, "end"]

  return(links)
}

#' @noRd
merge_lines  <- function(nodes, segments, links, from_edge = NULL) {

  get_linked_nodes <- function(node_id, segment_id) {
    # find the node connected to the given one via the given segment(s)
    # 1. get the nodes that are part of the given segment(s)
    nds <- segments[segment_id, ]
    # 2. exclude the given node from the list
    is_current_node <- nds == node_id
    linked_nodes <- nds[!is_current_node]
    return(linked_nodes)
  }

  to_linestring <- function(node_id) {
    points <- nodes[node_id, ]
    linestring <- sfheaders::sfc_linestring(points, x = "x", y = "y")
    return(linestring)
  }

  is_segment_used <- array(FALSE, dim = nrow(segments))
  strokes <- sf::st_sfc()
  for (iseg in seq_len(nrow(segments))) {
    if (is_segment_used[iseg]) next
    stroke  <- c()

    point <- segments[iseg, "start"]
    link <- links[iseg, "start"]
    current <- iseg
    is_closed_loop <- FALSE
    while (TRUE) {
      stroke <- c(point, stroke)
      is_segment_used[current] <- TRUE
      if (is.na(link) || is_closed_loop) break
      point <- get_linked_nodes(point, link)
      is_closed_loop <- point %in% stroke
      current <- link
      link <- links[current, names(point)]
    }

    point <- segments[iseg, "end"]
    link <- links[iseg, "end"]
    current <- iseg
    is_closed_loop <- FALSE
    while (TRUE) {
      stroke <- c(stroke, point)
      is_segment_used[current] <- TRUE
      if (is.na(link) || is_closed_loop) break
      point <- get_linked_nodes(point, link)
      is_closed_loop <- point %in% stroke
      current <- link
      link <- links[current, names(point)]
    }
    strokes <- c(strokes, to_linestring(stroke))
  }
  return(strokes)
}
