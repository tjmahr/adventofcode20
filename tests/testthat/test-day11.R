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

test_that("day 11b", {
  m1 <- example_ferry_seats("b1") %>% create_ferry_seat_matrix()
  m2 <- example_ferry_seats("b2") %>% create_ferry_seat_matrix()
  m3 <- example_ferry_seats("b3") %>% create_ferry_seat_matrix()

  g <- get_visible_ferry_seats(m1, 5, 4)
  expect_equal(sum(m1[g] == "#"), 8)

  g <- get_visible_ferry_seats(m2, 2, 2)
  expect_equal(sum(m2[g] == "L"), 1)
  expect_equal(sum(m2[g] == "#"), 0)

  g <- get_visible_ferry_seats(m3, 4, 4)
  expect_equal(sum(m3[g] == "L"), 0)
  expect_equal(sum(m3[g] == "#"), 0)

  m1 <- example_ferry_seats("c1") %>% create_ferry_seat_matrix()
  m2 <- example_ferry_seats("c2") %>% create_ferry_seat_matrix()
  m3 <- example_ferry_seats("c3") %>% create_ferry_seat_matrix()
  m4 <- example_ferry_seats("c4") %>% create_ferry_seat_matrix()
  m5 <- example_ferry_seats("c5") %>% create_ferry_seat_matrix()
  m6 <- example_ferry_seats("c6") %>% create_ferry_seat_matrix()
  m7 <- example_ferry_seats("c7") %>% create_ferry_seat_matrix()

  m1 %>%
    update_ferry_seat_matrix(
      neighbor_def = get_visible_ferry_seats,
      threshold = 5
    ) %>%
    expect_equal(m2) %>%
    update_ferry_seat_matrix(
      neighbor_def = get_visible_ferry_seats,
      threshold = 5
    ) %>%
    expect_equal(m3) %>%
    update_ferry_seat_matrix(
      neighbor_def = get_visible_ferry_seats,
      threshold = 5
    ) %>%
    expect_equal(m4) %>%
    update_ferry_seat_matrix(
      neighbor_def = get_visible_ferry_seats,
      threshold = 5
    ) %>%
    expect_equal(m5) %>%
    update_ferry_seat_matrix(
      neighbor_def = get_visible_ferry_seats,
      threshold = 5
    ) %>%
    expect_equal(m6) %>%
    update_ferry_seat_matrix(
      neighbor_def = get_visible_ferry_seats,
      threshold = 5
    ) %>%
    expect_equal(m7) %>%
    update_ferry_seat_matrix(
      neighbor_def = get_visible_ferry_seats,
      threshold = 5
    ) %>%
    expect_equal(m7)

  example_ferry_seats("c1") %>%
    simulate_ferry_seating(get_visible_ferry_seats, 5) %>%
    expect_equal(m7)


})
