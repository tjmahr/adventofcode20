library(adventofcode20)
x <- readLines("./inst/input13.txt")

p1a <- estimate_earliest_bus(x)
p1 <- prod(p1a)
p2 <- estimate_earliest_shared_bus_time(x)

stopifnot(p1 == aoc20_solutions$day13a)
stopifnot(p2 == aoc20_solutions$day13b)
