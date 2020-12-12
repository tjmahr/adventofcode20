library(adventofcode20)
x <- readLines("./inst/input11.txt")

p1a <- simulate_ferry_seating(x)
p1 <- sum(p1a == "#")
p2a <- simulate_ferry_seating(x, neighbor_def = get_visible_ferry_seats, 5)
p2 <- sum(p2a == "#")

stopifnot(p1 == aoc20_solutions$day11a)
stopifnot(p2 == aoc20_solutions$day11b)
