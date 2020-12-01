library(adventofcode20)
x <- readLines("./inst/input01.txt")
x <- as.numeric(x)

p1 <- prod(find_2020_pair(x))
p2 <- prod(find_2020_trio(x))

stopifnot(p1 == aoc20_solutions$day01a)
stopifnot(p2 == aoc20_solutions$day01b)
