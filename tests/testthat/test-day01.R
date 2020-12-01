
test_that("day 1", {
  x <- read_text_lines(
    "
    1721
    979
    366
    299
    675
    1456
    "
  )

  x <- as.numeric(x)
  pair <- find_2020_pair(x)
  trio <- find_2020_trio(x)

  expect_equal(prod(pair), 514579)
  expect_equal(prod(trio), 241861950)
})
