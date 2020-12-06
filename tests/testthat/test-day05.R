test_that("day 05", {
  x <- c("FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL")
  x %>%
    convert_seat_string_to_number() %>%
    expect_equal(c(357, 567, 119, 820))
})
