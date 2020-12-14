test_that("day 13", {
  results <- estimate_earliest_bus(example_bus_notes())
  expect_equal(unname(results), c(59, 5))
  expect_equal(prod(results), 295)

  b <- example_bus_notes("b0")
  b %>%
    estimate_earliest_shared_bus_time() %>%
    expect_equal(as.numeric(b[1]))

  b <- example_bus_notes("b1")
  b %>%
    estimate_earliest_shared_bus_time() %>%
    expect_equal(as.numeric(b[1]))

  b <- example_bus_notes("b2")
  b %>%
    estimate_earliest_shared_bus_time() %>%
    expect_equal(as.numeric(b[1]))

  b <- example_bus_notes("b3")
  b %>%
    estimate_earliest_shared_bus_time() %>%
    expect_equal(as.numeric(b[1]))

  b <- example_bus_notes("b4")
  b %>%
    estimate_earliest_shared_bus_time() %>%
    expect_equal(as.numeric(b[1]))

  b <- example_bus_notes("b5")
  b %>%
    estimate_earliest_shared_bus_time() %>%
    expect_equal(as.numeric(b[1]))
})
