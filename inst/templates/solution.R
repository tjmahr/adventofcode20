library(adventofcode20)
x <- readLines("./inst/input{{{ dd_number }}}.txt")

p1 <- f{{{ dd_number }}}a(x)
p2 <- f{{{ dd_number }}}b(x)

stopifnot(p1 == aoc20_solutions$day{{{ dd_number }}}a)
stopifnot(p2 == aoc20_solutions$day{{{ dd_number }}}b)
