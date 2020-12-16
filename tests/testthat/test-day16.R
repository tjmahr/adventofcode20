test_that("day 16", {
  x <- example_train_tickets(1)
  x %>%
    find_invalid_train_ticket_values() %>%
    getElement("invalid_numbers") %>%
    expect_equal(c(4, 55, 12))

  x <- example_train_tickets(2)
  x_solved <- solve_train_ticket_fields(x)

  x_solved %>%
    getElement("tickets") %>%
    expect_named(c("row", "class", "seat"))
})
