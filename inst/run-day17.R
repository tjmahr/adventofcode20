library(adventofcode20)
x <- readLines("./inst/input17.txt")

p1a <- run_conway_cube(x)
p1 <- sum(p1a$value)
stopifnot(p1 == aoc20_solutions$day17a)

if (FALSE) {
  p2a <- run_conway_cube(x, "4d", TRUE)
  p2 <- sum(p2a$value)
  stopifnot(p2 == aoc20_solutions$day17b)
}
