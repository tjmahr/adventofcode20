library(adventofcode20)
x <- readLines("./inst/input21.txt")

p1 <- length(deduce_safe_ingredients(x))
p2 <- deduce_allergenic_ingredients(x)

stopifnot(p1 == aoc20_solutions$day21a)
stopifnot(p2 == aoc20_solutions$day21b)
