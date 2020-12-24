library(adventofcode20)
x <- readLines("./inst/input23.txt")

p1a <- play_crab_cups(x, 100)
pl <- sort_crab_cups(p1a)
p2 <- f23b(x)

stopifnot(p1 == aoc20_solutions$day23a)
stopifnot(p2 == aoc20_solutions$day23b)
