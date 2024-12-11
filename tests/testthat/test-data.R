test_that("the test data can be loaded", {
  streets <- bucharest$streets
  expect_false(is.null(streets))
  expect_true(length(streets) > 0)

  river <- bucharest$river_centerline
  expect_false(is.null(river))
  expect_true(length(river) > 0)
})
