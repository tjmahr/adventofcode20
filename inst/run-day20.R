library(adventofcode20)
x <- readLines("./inst/input20.txt")

p1 <- prod(find_map_corners(x))
p2 <- f20b(x)
#
stopifnot(p1 == aoc20_solutions$day20a)
stopifnot(p2 == aoc20_solutions$day20b)
