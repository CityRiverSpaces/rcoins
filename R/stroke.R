#' Identify continuous lines in a network
#'
#' Apply the Continuity in Street Network (COINS) method to identify
#' sequences of edges that form naturally continuous strokes in a network.
#'
#' @param edges An object of class \code{\link[sf]{sfc}} (or compatible),
#' including the edge geometries (should be of type LINESTRING or
#' MULTILINESTRING).
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
  if (flow_mode) stop("flow mode not implemented.")
  if (!is.null(from_edge)) stop("from_edge mode not implemented")

  edges_sfc <- to_sfc(edges)
  check_geometry(edges_sfc)

  # extract CRS from the edges
  crs <- sf::st_crs(edges_sfc)
  
  # convert angle threshold to radians
  angle_threshold_rad <- angle_threshold / 180 * pi

  # split the edges into their constituent points
  edge_pts <- sfheaders::sfc_to_df(edges_sfc)

  # find unique points ("nodes") and assign them IDs
  nodes <- unique_nodes(edge_pts)

  # build array of line segments, referring to points using their IDs
  segments <- to_line_segments(edge_pts, nodes)

  # build connectivity table: for each node, find intersecting line segments
  links <- get_links(segments)

  # calculate interior angles between segment pairs, identify best links
  best_links <- best_link(nodes, segments, links, angle_threshold_rad)

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
best_link <- function(nodes, segments, links, angle_threshold = 0) {
  # convert nodes to a matrix for faster indexing
  nodes <- as.matrix(nodes[c("x", "y")])

  best_links <- array(integer(), dim = dim(segments))
  colnames(best_links) <- c("start", "end")

  for (iseg in seq_len(nrow(segments))) {
    start_node <- segments[iseg, "start"]
    end_node <- segments[iseg, "end"]

    best_link_start <- find_best_link(start_node, end_node, 
                                      iseg, segments, links)
    if (length(best_link_start) > 0)
        best_links[iseg, "start"] <- best_link_start

    best_link_end <- find_best_link(end_node, start_node, 
                                    iseg, segments, links)
    if (length(best_link_end) > 0)
        best_links[iseg, "end"] <- best_link_end
  }
  return(best_links)
}

#' @noRd
find_best_link <- function(node, opposite_node, current_segment, segments, links) {
  linked_segs <- get_linked_segments(current_segment, node, links)
  linked_nodes <- get_linked_nodes(node, linked_segs, segments)
  angles <- interior_angle(nodes[node, ], 
                           nodes[opposite_node, , drop = FALSE], 
                           nodes[linked_nodes, , drop = FALSE])
  best_link <- get_best_link(angles, linked_segs, angle_threshold_rad)
  return(best_link)
}

#' @noRd
interior_angle <- function(v, p1, p2) {
  # compute convex angle between three points:
  # p1--v--p2 ("v" is the vertex)
  # NOTE: multiple points are supported as p1 and p2
  dx1 <- p1[, "x"] - v["x"]
  dx2 <- p2[, "x"] - v["x"]
  dy1 <- p1[, "y"] - v["y"]
  dy2 <- p2[, "y"] - v["y"]
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
check_reciprocal <- function(links, best_links, colname) {
  # keep only indices that are between 0 and the length of best_links
  valid_indices <- best_links[, colname]
  valid_indices <- valid_indices[valid_indices > 0 &
                                     valid_indices <= nrow(best_links)]
  # find the best link of the best links
  bl <- best_links[valid_indices, , drop = FALSE]
  # we check both ends to see whether the best link is reciprocal
  is_best_link <- bl == seq_len(nrow(bl))
  # if we have a match on either of the sides, we keep the link
  is_reciprocal <- apply(is_best_link, 1, any)
  # fix for NA values
  is_reciprocal[is.na(is_reciprocal)] <- FALSE
  links[is_reciprocal, colname] <- best_links[is_reciprocal, colname]
    
    return(links)
}

#' @noRd
cross_check_links <- function(best_links, flow_mode = FALSE) {
  links <- array(integer(), dim = dim(best_links))
  colnames(links) <- c("start", "end")
    
  links <- check_reciprocal(links, best_links, "start")
  links <- check_reciprocal(links, best_links, "end")
  
  return(links)
}

#' @noRd
get_next_node <- function(node, segment, segments) {
  # find the node connected to the given one via the given segment
  # 1. get the nodes that are part of the given segment
  nodes <- segments[segment, ]
  # 2. exclude the given node from the list
  is_current <- nodes == node
  return(nodes[!is_current])
}

#' @noRd
get_next_segment <- function(segment, link, links) {
  # find the segment connected to the given one via the given link
  #  1. get the segments connected to the given link
  segments <- links[link, ]
  #  2. exclude the given segment from the list
  is_current <- segments == segment
  return(segments[!is_current])
}

#' @noRd
to_linestring <- function(node_id, nodes) {
  points <- nodes[node_id, ]
  linestring <- sfheaders::sfc_linestring(points, x = "x", y = "y")
  return(linestring)
}

#' @noRd
traverse_segments <- function(start_node, start_link, start_segment,
                              direction, segments, links, is_segment_used) {
  node <- start_node
  link <- start_link
  segment <- start_segment
  stroke <- c()

  while (TRUE) {
    if (is.na(link) || is_segment_used[link]) break
    node <- get_next_node(node, link, segments)
    if (direction == "forward") {
      stroke <- c(node, stroke)
    } else {
      stroke <- c(stroke, node)
    }
    # Modify the local is_segment_used
    is_segment_used[link] <- TRUE
    new_link <- get_next_segment(segment, link, links)
    segment <- link
    link <- new_link
  }
  # Return updated is_segment_used
  return(list(stroke = stroke, is_segment_used = is_segment_used))
}

#' @noRd
merge_lines  <- function(nodes, segments, links, from_edge = NULL) {
  is_segment_used <- array(FALSE, dim = nrow(segments))
  strokes <- sf::st_sfc()
  for (iseg in seq_len(nrow(segments))) {
    if (is_segment_used[iseg]) next

    stroke <- segments[iseg, ]

    is_segment_used[iseg] <- TRUE

    # Traverse forwards from the start node
    node  <- segments[iseg, "start"]
    link <- links[iseg, "start"]
    segment <- iseg
    forward_result <- traverse_segments(node, link, segment, "forward",
                                        segments, links, is_segment_used)
    forward_stroke <- forward_result$stroke
    is_segment_used <- forward_result$is_segment_used

    # Traverse backwards from the end node
    node  <- segments[iseg, "end"]
    link <- links[iseg, "end"]
    segment <- iseg
    backward_result <- traverse_segments(node, link, segment, "backward",
                                         segments, links, is_segment_used)
    backward_stroke <- backward_result$stroke
    is_segment_used <- backward_result$is_segment_used

    # Combine strokes and add to results
    stroke <- c(forward_stroke, stroke, backward_stroke)
    strokes <- c(strokes, to_linestring(stroke, nodes))
  }
  return(strokes)
}
