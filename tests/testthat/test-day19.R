test_that("day 19", {
  example_message(1) %>% check_messages() %>% sum() %>% expect_equal(2)
  example_message(2) %>% check_messages() %>% sum() %>% expect_equal(2)

  example_message(3) %>% check_messages() %>% sum() %>% expect_equal(3)

  example_message(3) %>%
    check_recursive_messages() %>%
    sum() %>%
    expect_equal(12)
})
