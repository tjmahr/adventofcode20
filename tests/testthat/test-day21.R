test_that("day 21", {
  deduce_safe_ingredients(example_ingredients()) %>%
    expect_length(5)

  deduce_allergenic_ingredients(example_ingredients()) %>%
    expect_equal("mxmxvkd,sqjhc,fvjkl")
})
