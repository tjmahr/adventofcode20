library(adventofcode20)
x <- readLines("./inst/input05.txt")

seats <- x %>%
  convert_seat_string_to_number() %>%
  sort()

p1 <- max(seats)
p2 <- setdiff(seq(min(seats), max(seats)), seats)

stopifnot(p1 == aoc20_solutions$day05a)
stopifnot(p2 == aoc20_solutions$day05b)
