library(adventofcode20)
x <- readLines("./inst/input18.txt")

p1 <- sum(eval_new_math(x))
p2 <- sum(eval_new_new_math(x))
print(p2, digits = 20)
stopifnot(p1 == aoc20_solutions$day18a)
stopifnot(p2 == aoc20_solutions$day18b)
