library(adventofcode20)
x <- readLines("./inst/input03.txt")

p1 <- count_trees_visited(x, move_x = 3, move_y = 1)

p2a <- p1
p2b <- count_trees_visited(x, move_x = 1, move_y = 1)
p2c <- count_trees_visited(x, move_x = 5, move_y = 1)
p2d <- count_trees_visited(x, move_x = 7, move_y = 1)
p2e <- count_trees_visited(x, move_x = 1, move_y = 2)

p2 <- prod(p2a, p2b, p2c, p2d, p2e)

stopifnot(p1 == aoc20_solutions$day03a)
stopifnot(p2 == aoc20_solutions$day03b)

