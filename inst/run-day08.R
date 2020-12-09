library(adventofcode20)
x <- readLines("./inst/input08.txt")

p1 <- run_boot_code_and_get_acc_value(x)
p2 <- check_boot_lines(x)

stopifnot(p1 == aoc20_solutions$day08a)
stopifnot(p2 == aoc20_solutions$day08b)
