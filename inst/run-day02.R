library(adventofcode20)
x <- readLines("./inst/input02.txt")

p1 <- check_for_valid_passwords(x) %>%
  getElement("is_valid") %>%
  sum()

p2 <- check_for_valider_passwords(x) %>%
  getElement("is_valid") %>%
  sum()

stopifnot(p1 == aoc20_solutions$day02a)
stopifnot(p2 == aoc20_solutions$day02b)
