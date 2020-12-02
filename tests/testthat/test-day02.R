test_that("day 2 works", {

  input <- "
  1-3 a: abcde
  1-3 b: cdefg
  2-9 c: ccccccccc
  "

  x <- read_text_lines(input)
  checked <- check_for_valid_passwords(x)
  expect_equal(checked$is_valid, c(TRUE, FALSE, TRUE))

  rechecked <- check_for_valider_passwords(x)
  expect_equal(rechecked$is_valid, c(TRUE, FALSE, FALSE))
})
