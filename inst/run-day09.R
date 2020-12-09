library(adventofcode20)
x <- readLines("./inst/input09.txt")

p1 <- f09a(x)
p2 <- f09b(x)

stopifnot(p1 == aoc20_solutions$day09a)
stopifnot(p2 == aoc20_solutions$day09b)
