library(adventofcode20)
x <- readLines("./inst/input06.txt")

p1 <- f06a(x)
p2 <- f06b(x)

stopifnot(p1 == aoc20_solutions$day06a)
stopifnot(p2 == aoc20_solutions$day06b)
