library(adventofcode20)
x <- readLines("./inst/input16.txt")

p1 <- f16a(x)
p2 <- f16b(x)

stopifnot(p1 == aoc20_solutions$day16a)
stopifnot(p2 == aoc20_solutions$day16b)
