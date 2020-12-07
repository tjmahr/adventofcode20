test_that("day 07", {
  x <- example_bag_rules()

  results <- eval_bag_rules(x) %>%
    keep_if(function(x) is.element("shiny gold", x)) %>%
    drop_if_has_name("shiny gold")

  expect_equal(length(results), 4)

  results2 <- eval_shiny_gold_bag_rule(x) %>%
    keep_if(function(x) x != "shiny gold")

  expect_equal(length(results2), 32)

  xraw <- "
    shiny gold bags contain 2 dark red bags.
    dark red bags contain 2 dark orange bags.
    dark orange bags contain 2 dark yellow bags.
    dark yellow bags contain 2 dark green bags.
    dark green bags contain 2 dark blue bags.
    dark blue bags contain 2 dark violet bags.
    dark violet bags contain no other bags.
  "
  x <- read_text_lines(xraw)
  results <- eval_shiny_gold_bag_rule(x) %>%
    keep_if(function(x) x != "shiny gold")

  expect_equal(length(results), 126)
})
