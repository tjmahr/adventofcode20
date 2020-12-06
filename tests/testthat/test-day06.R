test_that("day 06", {
  x <- read_text_lines(
    drop_empty = "head/tail",
    x = "
  abc

  a
  b
  c

  ab
  ac

  a
  a
  a
  a

  b
  "
  )

  sum_unique_questions <- x %>%
    find_unique_questions() %>%
    lengths() %>%
    sum()

  sum_shared_questions <- x %>%
    find_shared_questions() %>%
    lengths() %>%
    sum()

  expect_equal(sum_unique_questions, 11)
  expect_equal(sum_shared_questions, 6)
})
