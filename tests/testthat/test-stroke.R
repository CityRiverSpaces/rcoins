#           p4
#         /
# p1 - p2 - p3
#         \ |
#           p5 - p6
p1 <- sf::st_point(c(0, 0))
p2 <- sf::st_point(c(1, 0))
p3 <- sf::st_point(c(2, 0))
p4 <- sf::st_point(c(2, 1))
p5 <- sf::st_point(c(2, -1))
p6 <- sf::st_point(c(3, -1))

test_that("a stroke is found in a very simple network", {
  l1 <- sf::st_linestring(c(p1, p2))
  l2 <- sf::st_linestring(c(p2, p3))
  l3 <- sf::st_linestring(c(p2, p4))
  sfc <- sf::st_sfc(l1, l2, l3)
  expected <- sf::st_sfc(sf::st_linestring(c(p1, p2, p3)), l3)
  actual <- stroke(sfc)
  expect_setequal(actual, expected)
})

test_that("sf objects can be used in input", {
  l1 <- sf::st_linestring(c(p1, p2))
  l2 <- sf::st_linestring(c(p2, p3))
  l3 <- sf::st_linestring(c(p2, p4))
  sfc <- sf::st_sfc(l1, l2, l3)
  expected <- sf::st_sfc(sf::st_linestring(c(p1, p2, p3)), l3)
  actual <- sf::st_as_sf(sfc) |> stroke()
  expect_setequal(actual, expected)
})

test_that("sfnetworks objects can be used in input", {

  skip_if_not_installed("sfnetworks")

  l1 <- sf::st_linestring(c(p1, p2))
  l2 <- sf::st_linestring(c(p2, p3))
  l3 <- sf::st_linestring(c(p2, p4))
  nodes <- sf::st_sfc(p1, p2, p3, p4)
  edges <- sf::st_sf(from = c(1, 2, 2),
                     to = c(2, 3, 4),
                     geometry = sf::st_sfc(l1, l2, l3))
  net <- sfnetworks::sfnetwork(nodes = nodes, edges = edges,
                               directed = FALSE, force = TRUE)
  expected <- sf::st_sfc(sf::st_linestring(c(p1, p2, p3)), l3)
  actual <- stroke(net)
  expect_setequal(actual, expected)
})

test_that("multilinestrings are not supported", {
  l1 <- sf::st_linestring(c(p1, p2))
  l2 <- sf::st_linestring(c(p2, p3))
  l3 <- sf::st_linestring(c(p2, p4))
  sfc <- sf::st_sfc(c(l1, l2), l3)
  expect_error(stroke(sfc), "MULTILINESTRING")
})

test_that("proper attributes are returned for a very simple network", {
  skip(message = "attribute mode to be implemented")
  l1 <- sf::st_linestring(c(p1, p2))
  l2 <- sf::st_linestring(c(p2, p3))
  l3 <- sf::st_linestring(c(p2, p4))
  sfc <- sf::st_sfc(l1, l2, l3)
  expected <- as.integer(c(1, 1, 2))
  actual <- stroke(sfc, attributes = TRUE, flow_mode = TRUE)
  expect_setequal(actual, expected)
})

test_that("two linesegments are always merged if threshold is zero", {
  l1 <- sf::st_linestring(c(p3, p2))
  l2 <- sf::st_linestring(c(p2, p5))
  sfc <- sf::st_sfc(l1, l2)
  expected <- sf::st_sfc(sf::st_linestring(c(p3, p2, p5)))
  actual <- stroke(sfc, angle_threshold = 0)
  expect_setequal(actual, expected)
})

test_that("a more complex network with no threshold form a stroke", {
  l1 <- sf::st_linestring(c(p1, p2))
  l2 <- sf::st_linestring(c(p2, p5))
  l3 <- sf::st_linestring(c(p5, p6))
  l4 <- sf::st_linestring(c(p5, p3))
  sfc <- sf::st_sfc(l1, l2, l3, l4)
  expected <- sf::st_sfc(sf::st_linestring(c(p1, p2, p5, p6)), l4)
  actual <- stroke(sfc, angle_threshold = 0)
  expect_setequal(actual, expected)
})

test_that("a more complex network with threshold does not form strokes", {
  l1 <- sf::st_linestring(c(p1, p2))
  l2 <- sf::st_linestring(c(p2, p5))
  l3 <- sf::st_linestring(c(p5, p6))
  l4 <- sf::st_linestring(c(p5, p3))
  sfc <- sf::st_sfc(l1, l2, l3, l4)
  expected <- sfc
  actual <- stroke(sfc, angle_threshold = 150.)
  expect_setequal(actual, expected)
})

test_that("attributes cannot be returned if not in flow mode", {
  skip(message = "attribute mode to be implemented")
  l1 <- sf::st_linestring(c(p1, p2))
  l2 <- sf::st_linestring(c(p2, p3))
  sfc <- sf::st_sfc(l1, l2)
  expect_error(stroke(sfc, attributes = TRUE, flow_mode = FALSE),
               "Stroke attributes can be returned only if `flow_mode = TRUE`)")
})

test_that("edges can be split if flow_mode is false", {
  l1 <- sf::st_linestring(c(p1, p2, p5))
  l2 <- sf::st_linestring(c(p2, p3))
  sfc <- sf::st_sfc(l1, l2)
  expected <- sf::st_sfc(sf::st_linestring(c(p1, p2, p3)),
                         sf::st_linestring(c(p2, p5)))
  actual <- stroke(sfc, flow_mode = FALSE)
  expect_setequal(actual, expected)
})

test_that("edges are not split if flow_mode is true", {
  skip(message = "flow mode to be implemented")
  l1 <- sf::st_linestring(c(p1, p2, p5))
  l2 <- sf::st_linestring(c(p2, p3))
  sfc <- sf::st_sfc(l1, l2)
  expected <- sfc
  actual <- stroke(sfc, flow_mode = TRUE)
  expect_setequal(actual, expected)
})

test_that("strokes can be formed starting from a given edge", {
  skip(message = "stroke from edge to be implemented")
  l1 <- sf::st_linestring(c(p1, p2, p3))
  l2 <- sf::st_linestring(c(p2, p5))
  l3 <- sf::st_linestring(c(p5, p6))
  sfc <- sf::st_sfc(l1, l2, l3)
  expected <- sf::st_sfc(sf::st_linestring(c(p1, p2, p5, p6)))
  actual <- stroke(sfc, flow_mode = FALSE, from_edge = 3)
  expect_setequal(actual, expected)
})

test_that("strokes can be formed starting from a given line segment", {
  skip(message = "stroke from edge to be implemented")
  l1 <- sf::st_linestring(c(p1, p2, p3))
  l2 <- sf::st_linestring(c(p2, p5, p6))
  sfc <- sf::st_sfc(l1, l2)
  expected <- sf::st_sfc(sf::st_linestring(c(p1, p2, p5, p6)))
  actual <- stroke(sfc, flow_mode = FALSE,
                   from_edge = sf::st_linestring(c(p5, p6)))
  expect_setequal(actual, expected)
})

test_that("attributes can be returned if edge is specified in flow mode", {
  skip(message = "flow mode to be implemented")
  l1 <- sf::st_linestring(c(p1, p2))
  l2 <- sf::st_linestring(c(p2, p3))
  l3 <- sf::st_linestring(c(p3, p5))
  l4 <- sf::st_linestring(c(p5, p6))
  sfc <- sf::st_sfc(l1, l2, l3, l4)
  expected <- as.integer(c(1, NA, 1, 1))
  actual <- stroke(sfc, attribute = TRUE, flow_mode = TRUE, from_edge = 3)
  expect_setequal(actual, expected)
})
