library(adventofcode20)
x <- as.numeric(readLines("./inst/input09.txt"))

p1 <- find_xmas_preamble_mismatch(x, 25)
p2_raw <- find_summing_streak(x, p1)
p2 <- sum(range(p2_raw))

stopifnot(p1 == aoc20_solutions$day09a)
stopifnot(p2 == aoc20_solutions$day09b)
