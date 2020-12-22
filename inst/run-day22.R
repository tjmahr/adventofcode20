library(adventofcode20)
x <- readLines("./inst/input22.txt")

p1 <- f22a(x)
p2 <- f22b(x)

stopifnot(p1 == aoc20_solutions$day22a)
stopifnot(p2 == aoc20_solutions$day22b)
