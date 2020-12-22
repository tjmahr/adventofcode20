library(adventofcode20)
x <- readLines("./inst/input22.txt")

p1 <- sort_card_decks(x)
stopifnot(p1$.score == aoc20_solutions$day22a)

p2 <- play_recursive_combat_game(x)
stopifnot(p2$.score == aoc20_solutions$day22b)
