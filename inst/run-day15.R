library(adventofcode20)
x <- readLines("./inst/input15.txt")

p1 <- play_memory_game(x, 2020L)
stopifnot(p1$last_num == aoc20_solutions$day15a)

p2 <- play_memory_game(x, 30000000L)
stopifnot(p2$last_num == aoc20_solutions$day15b)
