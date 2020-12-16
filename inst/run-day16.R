library(adventofcode20)
x <- readLines("./inst/input16.txt")

p1a <- find_invalid_train_ticket_values(x)
p1 <- sum(p1a$invalid_numbers)

p2a <- solve_train_ticket_fields(x)
p2 <- p2a$tickets[1, startsWith(names(p2a$tickets), "departure")] %>% prod()

stopifnot(p1 == aoc20_solutions$day16a)
stopifnot(p2 == aoc20_solutions$day16b)
