test_that("day 10 a", {
  x <- example_adapter_data()
  counts <- count_adapter_differences(x)
  expect_equal(counts[1], 7)
  expect_equal(counts[3], 5)

  x <- example_adapter_data(2)
  counts <- count_adapter_differences(x)
  expect_equal(counts[1], 22)
  expect_equal(counts[3], 10)
})

test_that("day 10 b", {
  example_adapter_data() %>%
    count_adapter_orders() %>%
    expect_equal(8)

  example_adapter_data(2) %>%
    count_adapter_orders() %>%
    expect_equal(19208)
})
