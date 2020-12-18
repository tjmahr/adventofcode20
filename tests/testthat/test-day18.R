test_that("day 18", {
  example_new_math(1) %>%
    eval_new_math() %>%
    expect_equal(71)

  example_new_math(2) %>%
    eval_new_math() %>%
    expect_equal(51)

  example_new_math(3) %>%
    eval_new_math() %>%
    expect_equal(26)

  example_new_math(4) %>%
    eval_new_math() %>%
    expect_equal(437)

  example_new_math(5) %>%
    eval_new_math() %>%
    expect_equal(12240)

  example_new_math(6) %>%
    eval_new_math() %>%
    expect_equal(13632)
})


test_that("day 18b", {
  example_new_math(1) %>%
    eval_new_new_math() %>%
    expect_equal(231)

  example_new_math(2) %>%
    eval_new_new_math() %>%
    expect_equal(51)

  example_new_math(3) %>%
    eval_new_new_math() %>%
    expect_equal(46)

  example_new_math(4) %>%
    eval_new_new_math() %>%
    expect_equal(1445)

  example_new_math(5) %>%
    eval_new_new_math() %>%
    expect_equal(669060)

  example_new_math(6) %>%
    eval_new_new_math() %>%
    expect_equal(23340)
})
