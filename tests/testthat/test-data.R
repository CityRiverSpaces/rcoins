test_that("the test data can be loaded", {
  streets <- bucharest$streets
  expect_true(length(streets) > 0)

  river <- bucharest$river_centerline
  expect_true(length(river) > 0)
})
