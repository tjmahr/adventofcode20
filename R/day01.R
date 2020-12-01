#' Day 01: Report Repair
#'
#' [Report Repair](https://adventofcode.com/2020/day/1)
#'
#' @name day01
#' @rdname day01
#' @details
#'
#' **Part One**
#'
#' After saving Christmas [five years in a row](/events), you\'ve decided
#' to take a vacation at a nice resort on a tropical island.
#' [Surely]{title="WHAT COULD GO WRONG"}, Christmas will go on without you.
#'
#' The tropical island has its own currency and is entirely cash-only. The
#' gold coins used there have a little picture of a starfish; the locals
#' just call them *stars*. None of the currency exchanges seem to have
#' heard of them, but somehow, you\'ll need to find fifty of these coins by
#' the time you arrive so you can pay the deposit on your room.
#'
#' To save your vacation, you need to get all *fifty stars* by December
#' 25th.
#'
#' Collect stars by solving puzzles. Two puzzles will be made available on
#' each day in the Advent calendar; the second puzzle is unlocked when you
#' complete the first. Each puzzle grants *one star*. Good luck!
#'
#' Before you leave, the Elves in accounting just need you to fix your
#' *expense report* (your puzzle input); apparently, something isn\'t quite
#' adding up.
#'
#' Specifically, they need you to *find the two entries that sum to `2020`*
#' and then multiply those two numbers together.
#'
#' For example, suppose your expense report contained the following:
#'
#'     1721
#'     979
#'     366
#'     299
#'     675
#'     1456
#'
#' In this list, the two entries that sum to `2020` are `1721` and `299`.
#' Multiplying them together produces `1721 * 299 = 514579`, so the correct
#' answer is `514579`.
#'
#' Of course, your expense report is much larger. *Find the two entries
#' that sum to `2020`; what do you get if you multiply them together?*
#'
#' **Part Two**
#'
#' The Elves in accounting are thankful for your help; one of them even
#' offers you a starfish coin they had left over from a past vacation. They
#' offer you a second one if you can find *three* numbers in your expense
#' report that meet the same criteria.
#'
#' Using the above example again, the three entries that sum to `2020` are
#' `979`, `366`, and `675`. Multiplying them together produces the answer,
#' `241861950`.
#'
#' In your expense report, *what is the product of the three entries that
#' sum to `2020`?*
#'
#' @param x some data
#' @return For Part One, `find_2020_pair(x)` returns the pair of numbers that
#'   sum to 2020. For Part Two, `find_2020_trio(x)` returns the three numbers
#'   that sum to 2020.
#' @export
#' @examples
#' x <- c(1721, 979, 366, 299, 675, 1456)
#' find_2020_pair(x)
#' x %>% find_2020_pair() %>% prod()
#' find_2020_trio(x)
find_2020_pair <- function(x) {
  x <- sort(x)
  pair <- find_pair_summing_to_n(x, 2020)
  stopifnot(length(pair) == 2)
  pair
}


#' @rdname day01
#' @export
find_2020_trio <- function(x) {
  x <- sort(x)

  # 2020 = a + b + c
  # 2020 - a = b + c
  # So find a pair that adds up to 2020 - a, for each possible a.
  new_targets <- 2020 - x

  for (target_i in seq_along(new_targets)) {
    original_n <- x[target_i]
    these_x <- x[-target_i]
    this_n <- new_targets[target_i]
    result <- find_pair_summing_to_n(these_x, this_n)
    if (length(result != 0)) break
  }

  c(original_n, result)
}

find_pair_summing_to_n <- function(x, n) {
  pair <- x[(n - x) %in% x]
  pair
}

