library(adventofcode20)
x <- as.numeric(readLines("./inst/input10.txt"))

p1a <- count_adapter_differences(x)
p1 <- p1a[1] * p1a[3]
p2 <- count_adapter_orders(x)
print(p2, digits = 20)
stopifnot(p1 == aoc20_solutions$day10a)
stopifnot(p2 == aoc20_solutions$day10b)
