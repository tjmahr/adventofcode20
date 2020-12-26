test_that("day 24", {
  x <- example_hex_tiles() %>%
    flip_hex_tiles()
  expect_equal(x$black, 10)
  expect_equal(x$white, 5)
})
