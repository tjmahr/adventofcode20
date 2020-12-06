library(adventofcode20)
x <- readLines("./inst/input06.txt")

p1 <- x %>% find_unique_questions() %>% lengths() %>% sum()
p2 <- x %>% find_shared_questions() %>% lengths() %>% sum()

stopifnot(p1 == aoc20_solutions$day06a)
stopifnot(p2 == aoc20_solutions$day06b)
