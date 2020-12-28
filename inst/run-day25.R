library(adventofcode20)
x <- readLines("./inst/input25.txt") %>% as.numeric()

p1 <- solve_handshake_encryption_key(x)
# p2 <- f25b(x)

stopifnot(p1 == aoc20_solutions$day25a)
# stopifnot(p2 == aoc20_solutions$day25b)
