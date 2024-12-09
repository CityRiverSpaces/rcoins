test_that("the test data can be loaded", {
  streets <- test_city$streets
  expect_false(is.null(streets))
  expect_true(length(streets) > 0)

  river <- test_city$river_centerline
  expect_false(is.null(river))
  expect_true(length(river) > 0)
})
