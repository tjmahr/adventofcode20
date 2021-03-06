test_that("day 12", {
  x <- example_ship_instructions()
  final <- follow_ship_instructions(x)
  expect_equal(final, c(17, -8, 3))
  final <- follow_waypoint_instructions(x)
  expect_equal(final, c(4, -10, 214, -72))
})
