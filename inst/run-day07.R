library(adventofcode20)
x <- readLines("./inst/input07.txt")

p1 <- eval_bag_rules(x, repetitions = FALSE) %>%
  keep_if(function(x) is.element("shiny gold", x)) %>%
  drop_if_has_name("shiny gold") %>%
  length()

p2 <- eval_shiny_gold_bag_rule(x) %>%
  keep_if(function(x) x != "shiny gold") %>%
  length()

stopifnot(p1 == aoc20_solutions$day07a)
stopifnot(p2 == aoc20_solutions$day07b)
