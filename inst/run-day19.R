library(adventofcode20)
x <- readLines("./inst/input19.txt")

p1 <- sum(check_messages(x))
# p2 <- f19b(x)

stopifnot(p1 == aoc20_solutions$day19a)
# stopifnot(p2 == aoc20_solutions$day19b)
