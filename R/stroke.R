#' Identify continuous lines in a network
#'
#' Apply the Continuity in Street Network (COINS) method to identify
#' sequences of edges that form naturally continuous strokes in a network.
#'
#' @param edges An object of class \code{\link[sf]{sfc}} (or compatible),
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
#' @return An object of class \code{\link[sf]{sfc}} (if
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
  if (!is.null(from_edge) && (attributes || flow_mode)) {
    stop("from_edge is not compatible with attributes or flow_mode")
  }

  edges_sfc <- to_sfc(edges)
  check_geometry(edges_sfc)

  # extract CRS from the edges
  crs <- sf::st_crs(edges_sfc)

  # split the edges into their constituent points
  edge_pts <- sfheaders::sfc_to_df(edges_sfc)

  # find unique points ("nodes") and assign them IDs
  nodes <- unique_nodes(edge_pts)

  # build array of line segments, referring to points using their IDs
  line_segments <- to_line_segments(edge_pts, nodes)
  segments <- line_segments$segments
  edge_ids <- line_segments$edge_id

  # build connectivity table: for each node, find intersecting line segments
  links <- get_links(segments)

  # calculate interior angles between segment pairs, identify best links
  best_links <- best_link(
    nodes, segments, links, edge_ids, flow_mode, angle_threshold
  )

  if (is.null(from_edge)) {
    # verify that the best links identified fulfill input requirements
    final_links <- cross_check_links(best_links)
    segments_ids <- seq_len(nrow(segments))

  } else {
    # map edge IDs to segment IDs
    segments_ids <- which(edge_ids %in% from_edge)

    # if we are looking for strokes starting from a specific edge, we use
    # `best_links`
    final_links <- best_links
  }

  # merge line segments into strokes following the predetermined connectivity
  strokes  <- merge_lines(nodes, segments, final_links, segments_ids, from_edge)

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
  edge_ids <- points[!is_endpoint, "linestring_id"]
  segments <- cbind(start, end)
  return(list(segments = segments, edge_ids = edge_ids))
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
get_linked_segments <- function(segment_id, node_id, links) {
  # find the segments connected to the given one via the given node
  # 1. find all segments connected to the node
  segs <- links[[node_id]]
  # 2. exclude the given segment from the list
  is_current_segment <- segs == segment_id
  linked_segments <- segs[!is_current_segment]
  return(linked_segments)
}

#' @noRd
get_linked_nodes <- function(node_id, segment_id, segments) {
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

#' @noRd
best_link <- function(
  nodes, segments, links, edge_ids, flow_mode, angle_threshold = 0
) {

  # convert nodes to a matrix for faster indexing
  nodes <- as.matrix(nodes[c("x", "y")])

  best_links <- array(integer(), dim = dim(segments))
  colnames(best_links) <- c("start", "end")

  angle_threshold_rad <- angle_threshold / 180 * pi  # convert to radians

  for (iseg in seq_len(nrow(segments))) {
    start_node <- segments[iseg, "start"]
    end_node <- segments[iseg, "end"]
    edge_id <- edge_ids[iseg]

    # find angles formed with all segments linked at start point
    linked_segs <- get_linked_segments(iseg, start_node, links)

    # if flow_mode, we choose the link on the same edge
    if (flow_mode) {
      best_link <- get_link_on_same_edge(linked_segs, edge_ids, edge_id)
    }

    # if not flow_mode or no link on the same edge, we calculate the angle
    if (length(best_link) == 0 || !flow_mode) {
      linked_nodes <- get_linked_nodes(start_node, linked_segs, segments)
      angles <- interior_angle(nodes[start_node, ],
                               nodes[end_node, , drop = FALSE],
                               nodes[linked_nodes, , drop = FALSE])
      best_link <- get_best_link(angles, linked_segs, angle_threshold_rad)
    }

    if (length(best_link) > 0) best_links[iseg, "start"] <- best_link

    # find angles formed with all segments linked at end point
    linked_segs <- get_linked_segments(iseg, end_node, links)

    # if flow_mode, we choose the link on the same edge
    if (flow_mode) {
      best_link <- get_link_on_same_edge(linked_segs, edge_ids, edge_id)
    }

    # if not flow_mode or no link on the same edge, we calculate the angle
    if (length(best_link) == 0 || !flow_mode) {
      linked_nodes <- get_linked_nodes(end_node, linked_segs, segments)
      angles <- interior_angle(nodes[end_node, ],
                               nodes[start_node, , drop = FALSE],
                               nodes[linked_nodes, , drop = FALSE])
      best_link <- get_best_link(angles, linked_segs, angle_threshold_rad)
    }


    if (length(best_link) > 0) best_links[iseg, "end"] <- best_link
  }
  return(best_links)
}

#' @noRd
interior_angle <- function(v, p1, p2) {
  # compute convex angle between three points:
  # p1--v--p2 ("v" is the vertex)
  # NOTE: multiple points are supported as p1 and p2
  dx1 <- p1[, "x"] - v[["x"]]
  dx2 <- p2[, "x"] - v[["x"]]
  dy1 <- p1[, "y"] - v[["y"]]
  dy2 <- p2[, "y"] - v[["y"]]
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
  idx_above_threshold <- which(angles > angle_threshold)
  is_best_link <- which.max(angles[idx_above_threshold])
  best_link <- links[idx_above_threshold[is_best_link]]
  return(best_link)
}

#' @noRd
get_link_on_same_edge <- function(links, edge_ids, edge_id) {
  is_same_edge <- edge_ids[links] == edge_id
  link_on_same_edge <- links[is_same_edge]
  return(link_on_same_edge)
}

#' @noRd
cross_check_links <- function(best_links) {

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
get_next <- function(node, link, segments, links) {
  # find the node and segment connected to the current ones via the given link
  # 1. get the nodes and segments connected to the given link
  nodes <- segments[link, ]
  segs <- links[link, ]
  # 2. identify the position of the current node in the arrays (the current
  #    segment will be in the same position
  is_current <- nodes == node
  # 3. exclude the current node and segment from the respective lists to find
  #    the new elements
  return(list(node = nodes[!is_current], link = segs[!is_current]))
}

#' @noRd
to_linestring <- function(node_id, nodes) {
  points <- nodes[node_id, ]
  linestring <- sfheaders::sfc_linestring(points, x = "x", y = "y")
  return(linestring)
}

#' @noRd
merge_lines <- function(
  nodes, segments, links, segments_ids, from_edge = NULL
) {
  is_segment_used <- array(FALSE, dim = nrow(segments))
  strokes <- sf::st_sfc()

  for (iseg in segments_ids) {
    if (is_segment_used[iseg]) next

    stroke <- segments[iseg, ]

    is_segment_used[iseg] <- TRUE

    node <- segments[iseg, "start"]
    link <- links[iseg, "start"]

    while (TRUE) {
      # one segment can appear in multiple strokes when using from_edge
      if (is.na(link) || (is_segment_used[link] && is.null(from_edge))) break
      new <- get_next(node, link, segments, links)
      is_segment_used[link] <- TRUE
      node <- new$node
      link <- new$link
      stroke <- c(node, stroke)
    }

    node <- segments[iseg, "end"]
    link <- links[iseg, "end"]

    while (TRUE) {
      # one segment can appear in multiple strokes when using from_edge
      if (is.na(link) || (is_segment_used[link] && is.null(from_edge))) break
      new <- get_next(node, link, segments, links)
      is_segment_used[link] <- TRUE
      node <- new$node
      link <- new$link
      stroke <- c(stroke, node)
    }
    strokes <- c(strokes, to_linestring(stroke, nodes))
  }
  return(strokes)
}
