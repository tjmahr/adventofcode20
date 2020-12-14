library(adventofcode20)
x <- readLines("./inst/input12.txt")

p1a <- follow_ship_instructions(x)
p1 <- sum(abs(p1[1:2]))

stopifnot(p1 == aoc20_solutions$day12a)
stopifnot(p2 == aoc20_solutions$day12b)
