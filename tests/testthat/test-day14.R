test_that("day 14", {
  "000000000000000000000000000001100101" %>%
    convert_binary_to_integer() %>%
    expect_equal(101) %>%
    convert_integer_to_binary() %>%
    expect_equal("000000000000000000000000000001100101")

  "000000000000000000000000000001001001" %>%
    convert_binary_to_integer() %>%
    expect_equal(73) %>%
    convert_integer_to_binary() %>%
    expect_equal("000000000000000000000000000001001001")

  "000000000000000000000000000000001011" %>%
    convert_binary_to_integer() %>%
    expect_equal(11) %>%
    convert_integer_to_binary() %>%
    expect_equal("000000000000000000000000000000001011")

  "000000000000000000000000000001000000" %>%
    convert_binary_to_integer() %>%
    expect_equal(64) %>%
    convert_integer_to_binary() %>%
    expect_equal("000000000000000000000000000001000000")

  x <- example_initialization(1)
  x %>%
    process_initialization_instructions() %>%
    sum() %>%
    expect_equal(165)

  x <- example_initialization(2)
  x %>%
    process_initialization_instructions_v2() %>%
    sum() %>%
    expect_equal(208)
})
