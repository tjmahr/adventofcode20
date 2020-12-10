test_that("day 09", {
  x <- example_xmas_preamble()
  check <- find_xmas_preamble_mismatch(x, 5)
  expect_equal(check, 127)

  streak <- find_summing_streak(x, check)
  expect_equal(streak, c(15, 25, 47, 40))
  expect_equal(sum(range(streak)), 62)
})
