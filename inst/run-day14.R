library(adventofcode20)
x <- readLines("./inst/input14.txt")

p1a <- process_initialization_instructions(x)
p1 <- sum(p1a)
p2a <- process_initialization_instructions_v2(x)
p2 = sum(p2a)

stopifnot(p1 == aoc20_solutions$day14a)
stopifnot(p2 == aoc20_solutions$day14b)
