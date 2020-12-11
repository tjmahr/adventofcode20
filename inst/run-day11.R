library(adventofcode20)
x <- readLines("./inst/input11.txt")

# 101 tics
# p1a <- simulate_ferry_seating(x)
# p1 <- sum(p1a == "#")
p2 <- f11b(x)

# stopifnot(p1 == aoc20_solutions$day11a)
stopifnot(p2 == aoc20_solutions$day11b)
