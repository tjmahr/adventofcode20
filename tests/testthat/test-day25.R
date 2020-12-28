test_that("day 25 works", {
  keys <- example_handshake_data()

  keys$door %>% solve_handshake_loop_size() %>% expect_equal(11)
  keys$card %>% solve_handshake_loop_size() %>% expect_equal(8)

  keys$door %>%
    calculate_handshake_encryption_key(8) %>%
    expect_equal(14897079)

  keys$card %>%
    calculate_handshake_encryption_key(11) %>%
    expect_equal(14897079)
})
