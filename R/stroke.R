#' Identify continuous lines in a network
#'
#' Apply the Continuity in Street Network (COINS) method to identify
#' sequences of edges that form naturally continuous strokes in a network.
#'
#' @param edges An object of class \code{\link[sf]{sfc}} (or compatible),
#' including the edge geometries (should be of type LINESTRING).
#'
#' @param angle_threshold Consecutive line segments can be considered part of
#' the same stroke if the internal angle they form is larger than
#' \code{angle_threshold} (in degrees). It should fall in the range
#' \code{0 <= angle_threshold < 180}.
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

  if (!is.null(from_edge) && (attributes || flow_mode)) {
    stop("from_edge is not compatible with attributes or flow_mode")
  }

  edges_sfc <- to_sfc(edges)
  check_geometry(edges_sfc)

  # split the edges into their constituent points
  edge_pts <- sfheaders::sfc_to_df(edges_sfc)

  # find unique points ("nodes")
  nodes <- unique_nodes(edge_pts)

  # build array of line segments, referring to points using the node IDs
  line_segments <- to_line_segments(edge_pts, nodes)
  segments <- line_segments$segments
  edge_ids <- line_segments$edge_id

  # build connectivity table: for each node, find intersecting line segments
  links <- get_links(segments)

  # identify best links by calculating interior angles between segment pairs
  angle_threshold_rad <- angle_threshold / 180 * pi
  best_links <- best_link(nodes, segments, links, edge_ids, flow_mode,
                          angle_threshold_rad)

  # only when considering all edges we verify that best links are reciprocal
  if (is.null(from_edge)) {
    final_links <- cross_check_links(best_links)
  } else {
    final_links <- best_links
  }

  # merge line segments into strokes following the predetermined connectivity
  crs <- sf::st_crs(edges_sfc)
  merge_lines(nodes, segments, final_links, edge_ids, from_edge, attributes,
              crs)
}

#' Find unique nodes
#' @noRd
#' @importFrom rlang .data
unique_nodes <- function(edge_points) {
  nodes <- dplyr::distinct(edge_points, .data$x, .data$y)
  # convert to matrix for faster indexing
  nodes <- as.matrix(nodes[c("x", "y")])
  return(nodes)
}

#' @noRd
to_line_segments <- function(points, nodes) {
  # label nodes with IDs
  node_id <- seq_len(nrow(nodes))
  nodes <- as.data.frame(cbind(nodes, node_id))

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
#' @importFrom rlang .data
get_links <- function(segments) {
  nsegments <- nrow(segments)
  links <- data.frame(node_id = as.vector(segments)) |>
    dplyr::group_by(.data$node_id) |>
    dplyr::group_rows()  |>
    lapply(function(x) (x - 1) %% nsegments + 1)
  return(links)
}

#' @noRd
best_link <- function(nodes, segments, links, edge_ids, flow_mode,
                      angle_threshold = 0) {

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

  get_link_on_same_edge <- function(linked_segs, current_segment) {
    is_same_edge <- edge_ids[linked_segs] == edge_ids[current_segment]
    link_on_same_edge <- linked_segs[is_same_edge]
    return(link_on_same_edge)
  }

  find_best_link <- function(node, opposite_node, current_segment) {
    linked_segs <- get_linked_segments(current_segment, node)

    # if in flow mode, we look for a link on the same edge
    if (flow_mode) {
      best_link <- get_link_on_same_edge(linked_segs, current_segment)
    }
    # if not in flow mode or if no link is found on the same edge, we look for
    # the best link by calculating the interior angles with all connections
    if (length(best_link) == 0 || !flow_mode) {
      linked_nodes <- get_linked_nodes(node, linked_segs)
      angles <- interior_angle(nodes[node, ],
                               nodes[opposite_node, , drop = FALSE],
                               nodes[linked_nodes, , drop = FALSE])
      best_link <- get_best_link(angles, linked_segs, angle_threshold)
    }
    return(best_link)
  }

  best_links <- array(integer(), dim = dim(segments))
  colnames(best_links) <- c("start", "end")

  for (iseg in seq_len(nrow(segments))) {
    start_node <- segments[iseg, "start"]
    end_node <- segments[iseg, "end"]

    best_link_start <- find_best_link(start_node, end_node, iseg)
    if (length(best_link_start) > 0)
      best_links[iseg, "start"] <- best_link_start

    best_link_end <- find_best_link(end_node, start_node, iseg)
    if (length(best_link_end) > 0)
      best_links[iseg, "end"] <- best_link_end
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
  angle <- acos(round(cos_theta, 6))
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
cross_check_links <- function(best_links) {
  links <- array(integer(), dim = dim(best_links))
  colnames(links) <- c("start", "end")

  is_start_reciprocal <- check_reciprocal(best_links, "start")
  links[is_start_reciprocal, "start"] <-
    best_links[is_start_reciprocal, "start"]

  is_end_reciprocal <- check_reciprocal(best_links, "end")
  links[is_end_reciprocal, "end"] <-
    best_links[is_end_reciprocal, "end"]

  return(links)
}

#' @noRd
check_reciprocal <- function(best_links, side) {
  # find the best link of the best links
  bl <- best_links[best_links[, side], ]
  # we check both ends to see whether the best link is reciprocal
  is_best_link <- bl == seq_len(nrow(bl))
  # if we have a match on either of the sides, we keep the link
  is_reciprocal <- apply(is_best_link, 1, any)
  # fix for NA values
  is_reciprocal[is.na(is_reciprocal)] <- FALSE

  return(is_reciprocal)
}

#' @noRd
merge_lines <- function(nodes, segments, links, edge_ids,
                        from_edge = NULL, attributes = FALSE, crs = NULL) {
  is_segment_used <- array(FALSE, dim = nrow(segments))
  stroke_labels <- array(integer(), dim = max(edge_ids))
  strokes <- sf::st_sfc()

  if (is.null(from_edge)) {
    segment_ids <- seq_len(nrow(segments))
    can_reuse_segments <- FALSE
  } else {
    segment_ids <- which(edge_ids %in% from_edge)
    can_reuse_segments <- TRUE
  }

  istroke <- 1
  for (iseg in segment_ids) {
    if (is_segment_used[iseg] && !can_reuse_segments) next

    stroke <- c(iseg)

    # traverse forwards from the end node of the current segment
    node  <- segments[iseg, "end"]
    link <- links[iseg, "end"]
    stroke <- traverse_segments(stroke, node, link, can_reuse_segments,
                                segments, links, is_segment_used)

    # revert the stroke to traverse backwards from the start node of the
    # current segment, then revert it back to the original direction
    node  <- segments[iseg, "start"]
    link <- links[iseg, "start"]
    stroke <- rev(stroke)
    stroke <- traverse_segments(stroke, node, link, can_reuse_segments,
                                segments, links, is_segment_used)
    stroke <- rev(stroke)

    # keep track of edge ids of strokes and add current stroke to strokes
    is_segment_used[stroke] <- TRUE
    stroke_labels[edge_ids[stroke]] <- istroke
    strokes <- c(strokes, to_linestring(stroke, segments, nodes))
    istroke <- istroke + 1
  }

  # only at the end, add CRS
  sf::st_crs(strokes) <- sf::st_crs(crs)
  if (attributes) {
    return(stroke_labels)
  } else {
    return(strokes)
  }
}

#' @noRd
to_linestring <- function(stroke, segments, nodes) {
  # extract the sequence of the nodes forming the stroke (with duplicates)
  segs <- segments[stroke, , drop = FALSE]
  if (nrow(segs) > 1) {
    # get all but the last segment in the stroke (i.e. the "target" segments)
    targets <- segs[-nrow(segs), , drop = FALSE]
    # get the stroke segments that immediately follow the targets (these are all
    # but the first segment in the stroke)
    linked <- segs[-1, , drop = FALSE]
    # for each pair of target/linked segments, find the duplicate nodes (we
    # check both ends of the linked segments). The sequence of repeated nodes
    # makes up the body of the stroke
    is_duplicate <- targets == linked[, 1] | targets == linked[, 2]
    # transpose both `targets` and `is_duplicate`, so that the duplicate nodes
    # appear in the correct order when applying the mask (the "segment"
    # dimension should run along the rows)
    repeated_nodes <- t(targets)[t(is_duplicate)]
    # we now have the body of the stroke, identify the first and last nodes
    first_seg <- segs[1, ]
    start <- first_seg[first_seg != repeated_nodes[1]]
    last_seg <- segs[nrow(segs), ]
    end <- last_seg[last_seg != repeated_nodes[length(repeated_nodes)]]
    # concatenate the sequence of nodes
    node_ids <- c(start, repeated_nodes, end)
  } else {
    # if we have a single segment, its two nodes already makes up the stroke
    node_ids <- segs
  }
  # build the linestring geometry from the sequence of nodes
  points <- nodes[node_ids, ]
  linestring <- sfheaders::sfc_linestring(points, x = "x", y = "y")
  return(linestring)
}

#' @noRd
traverse_segments <- function(stroke, node, link, can_reuse_segments,
                              segments, links, is_segment_used) {

  get_next <- function(node, link) {
    # find node and segment connected to the current ones via the given link
    # 1. get the nodes and segments connected to the given link
    nodes <- segments[link, ]
    segs <- links[link, ]
    # 2. identify the position of the current node in the arrays (the current
    #  segment will be in the same position
    is_current <- nodes == node
    # 3. exclude current node and segment from the respective lists to find
    #  the new elements
    return(list(node = nodes[!is_current], link = segs[!is_current]))
  }

  while (TRUE) {
    if (is.na(link) || (link %in% stroke) ||
          (is_segment_used[link] && !can_reuse_segments)) break
    stroke <- c(stroke, link)
    new <- get_next(node, link)
    node <- new$node
    link <- new$link
  }
  return(stroke)
}
