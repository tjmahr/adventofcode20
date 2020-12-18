test_that("day 17", {
  x <- example_cube_state()

  results <- run_conway_cube(x)
  expect_equal(sum(results$value), 112)

  # results2 <- run_conway_cube(x, "4d")
  # expect_equal(sum(results2$value), 848)
})
