#' Day 15: Rambunctious Recitation
#'
#' [Rambunctious Recitation](https://adventofcode.com/2020/day/15)
#'
#' @name day15
#' @rdname day15
#' @details
#'
#' **Part One**
#'
#' You catch the airport shuttle and try to book a new flight to your
#' vacation island. Due to the storm, all direct flights have been
#' cancelled, but a route is available to get around the storm. You take
#' it.
#'
#' While you wait for your flight, you decide to check in with the Elves
#' back at the North Pole. They're playing a *memory game* and are [ever
#' so excited]{title="Of course they are."} to explain the rules!
#'
#' In this game, the players take turns saying *numbers*. They begin by
#' taking turns reading from a list of *starting numbers* (your puzzle
#' input). Then, each turn consists of considering the *most recently
#' spoken number*:
#'
#' -   If that was the *first* time the number has been spoken, the current
#'     player says *`0`*.
#' -   Otherwise, the number had been spoken before; the current player
#'     announces *how many turns apart* the number is from when it was
#'     previously spoken.
#'
#' So, after the starting numbers, each turn results in that player
#' speaking aloud either *`0`* (if the last number is new) or an *age* (if
#' the last number is a repeat).
#'
#' For example, suppose the starting numbers are `0,3,6`:
#'
#' -   *Turn 1*: The `1`st number spoken is a starting number, *`0`*.
#' -   *Turn 2*: The `2`nd number spoken is a starting number, *`3`*.
#' -   *Turn 3*: The `3`rd number spoken is a starting number, *`6`*.
#' -   *Turn 4*: Now, consider the last number spoken, `6`. Since that was
#'     the first time the number had been spoken, the `4`th number spoken
#'     is *`0`*.
#' -   *Turn 5*: Next, again consider the last number spoken, `0`. Since it
#'     *had* been spoken before, the next number to speak is the difference
#'     between the turn number when it was last spoken (the previous turn,
#'     `4`) and the turn number of the time it was most recently spoken
#'     before then (turn `1`). Thus, the `5`th number spoken is `4 - 1`,
#'     *`3`*.
#' -   *Turn 6*: The last number spoken, `3` had also been spoken before,
#'     most recently on turns `5` and `2`. So, the `6`th number spoken is
#'     `5 - 2`, *`3`*.
#' -   *Turn 7*: Since `3` was just spoken twice in a row, and the last two
#'     turns are `1` turn apart, the `7`th number spoken is *`1`*.
#' -   *Turn 8*: Since `1` is new, the `8`th number spoken is *`0`*.
#' -   *Turn 9*: `0` was last spoken on turns `8` and `4`, so the `9`th
#'     number spoken is the difference between them, *`4`*.
#' -   *Turn 10*: `4` is new, so the `10`th number spoken is *`0`*.
#'
#' (The game ends when the Elves get sick of playing or dinner is ready,
#' whichever comes first.)
#'
#' Their question for you is: what will be the *`2020`th* number spoken? In
#' the example above, the `2020`th number spoken will be `436`.
#'
#' Here are a few more examples:
#'
#' -   Given the starting numbers `1,3,2`, the `2020`th number spoken is
#'     `1`.
#' -   Given the starting numbers `2,1,3`, the `2020`th number spoken is
#'     `10`.
#' -   Given the starting numbers `1,2,3`, the `2020`th number spoken is
#'     `27`.
#' -   Given the starting numbers `2,3,1`, the `2020`th number spoken is
#'     `78`.
#' -   Given the starting numbers `3,2,1`, the `2020`th number spoken is
#'     `438`.
#' -   Given the starting numbers `3,1,2`, the `2020`th number spoken is
#'     `1836`.
#'
#' Given your starting numbers, *what will be the `2020`th number spoken?*
#'
#' **Part Two**
#'
#' Impressed, the Elves issue you a challenge: determine the `30000000`th
#' number spoken. For example, given the same starting numbers as above:
#'
#' -   Given `0,3,6`, the `30000000`th number spoken is `175594`.
#' -   Given `1,3,2`, the `30000000`th number spoken is `2578`.
#' -   Given `2,1,3`, the `30000000`th number spoken is `3544142`.
#' -   Given `1,2,3`, the `30000000`th number spoken is `261214`.
#' -   Given `2,3,1`, the `30000000`th number spoken is `6895259`.
#' -   Given `3,2,1`, the `30000000`th number spoken is `18`.
#' -   Given `3,1,2`, the `30000000`th number spoken is `362`.
#'
#' Given your starting numbers, *what will be the `30000000`th number
#' spoken?*
#'
#' @param x the initial setup of the memory game
#' @return For Part One, `play_memory_game(x)` returns a list of vectors with
#'   game data. For Part Two, `f15b(x)` returns ....
#' @export
#' @examples
#' game <- play_memory_game(example_memory_game(1))
#' game$last_num
play_memory_game <- function(x, max_turn) {
  game <- setup_memory_game(x, max_turn)
  turn <- game$turn
  last_num <- game$last_num
  visits_1 <- game$visits_1
  visits_2 <- game$visits_2

  while (turn <= max_turn) {
    # Performance tweaks:
    # * turning the game list into an environment
    # * making sure everything was an integer
    # * inlined the step_memory_game() function body to reduce overhead
    # * made sure that the first visit vector was only checked if needed
    # * unpacked the game environment before the loop to remove lookups
    index <- last_num + 1L
    v2 <- visits_2[index]

    this_num <- if (is.na(v2)) {
      0L
    } else {
      visits_1[index] - v2
    }

    new_index <- this_num + 1L
    visits_2[new_index] <- visits_1[new_index]
    visits_1[new_index] <- turn

    last_num <- this_num
    turn <- turn + 1L
  }

  game$turn <- turn
  game$last_num <- last_num
  game$visits_1 <- visits_1
  game$visits_2 <- visits_2
  game
}


step_memory_game <- function(game) {
  index <- game$last_num + 1L
  this_num <- game$visits_1[index] - game$visits_2[index]
  this_num <- if (is.na(this_num)) 0L else this_num

  new_index <- this_num + 1L
  game$visits_2[new_index] <- game$visits_1[new_index]
  game$visits_1[new_index] <- game$turn

  game$last_num <- this_num
  game$turn <- game$turn + 1L
  game
}


setup_memory_game <- function(x, max_turn) {
  numbers <- x %>% strsplit(",") %>% unlist() %>% as.integer()
  visits_1 <- rep(NA_integer_, max_turn + 1L)
  visits_2 <- visits_1

  for (a in seq_along(numbers)) {
    num <- numbers[a]
    visits_1[num + 1] <- a
  }

  # this setup assumes that there are no repeats in the initial setup numbers

  # Try to increase performance
  list2env(
    list(
      seed = numbers,
      turn = length(numbers) + 1L,
      visits_1 = visits_1,
      visits_2 = visits_2,
      max_turn = max_turn,
      last_num = num
    ),
  )
}


#' @rdname day15
#' @export
example_memory_game <- function(example = 1) {
  l <- list(
    a1 = "0,3,6",
    a2 = "1,3,2",
    a3 = "2,1,3",
    a4 = "1,2,3",
    a5 = "2,3,1",
    a6 = "3,2,1",
    a7 = "3,1,2"
  )
  l[[example]]
}
