library(adventofcode20)
x <- readLines("./inst/input24.txt")

p1 <- flip_hex_tiles(x)
p2 <- f24b(x)

stopifnot(p1$black == aoc20_solutions$day24a)
stopifnot(p2 == aoc20_solutions$day24b)
