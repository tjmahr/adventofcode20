test_that("day 11", {
  m1 <- example_ferry_seats(1) %>% create_ferry_seat_matrix()
  m2 <- example_ferry_seats(2) %>% create_ferry_seat_matrix()
  m3 <- example_ferry_seats(3) %>% create_ferry_seat_matrix()
  m4 <- example_ferry_seats(4) %>% create_ferry_seat_matrix()
  m5 <- example_ferry_seats(5) %>% create_ferry_seat_matrix()
  m6 <- example_ferry_seats(6) %>% create_ferry_seat_matrix()

  m1 %>%
    update_ferry_seat_matrix() %>%
    expect_equal(m2) %>%
    update_ferry_seat_matrix() %>%
    expect_equal(m3) %>%
    update_ferry_seat_matrix() %>%
    expect_equal(m4) %>%
    update_ferry_seat_matrix() %>%
    expect_equal(m5) %>%
    update_ferry_seat_matrix() %>%
    expect_equal(m6) %>%
    # convergence
    update_ferry_seat_matrix() %>%
    expect_equal(m6)

  m_final <- example_ferry_seats(1) %>%
    simulate_ferry_seating() %>%
    expect_equal(m6)

  expect_equal(sum(m_final == "#"), 37)
})
