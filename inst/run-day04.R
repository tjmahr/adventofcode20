library(adventofcode20)
x <- readLines("./inst/input04.txt")

p1 <- check_for_valid_passports(x)
p2 <- check_for_valider_passports(x)

stopifnot(sum(p1) == aoc20_solutions$day04a)
stopifnot(p2 == aoc20_solutions$day04b)
