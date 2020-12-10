#' Day 09: Encoding Error
#'
#' [Encoding Error](https://adventofcode.com/2020/day/9)
#'
#' @name day09
#' @rdname day09
#' @details
#'
#' **Part One**
#'
#' attention to an open data port on the little screen in the seat in front
#' of you.
#'
#' Though the port is non-standard, you manage to connect it to your
#' computer through the clever use of several paperclips. Upon connection,
#' the port outputs a series of numbers (your puzzle input).
#'
#' The data appears to be encrypted with the eXchange-Masking Addition
#' System ([XMAS]{title="No relation."}) which, conveniently for you, is an
#' old cypher with an important weakness.
#'
#' XMAS starts by transmitting a *preamble* of 25 numbers. After that, each
#' number you receive should be the sum of any two of the 25 immediately
#' previous numbers. The two numbers will have different values, and there
#' might be more than one such pair.
#'
#' For example, suppose your preamble consists of the numbers `1` through
#' `25` in a random order. To be valid, the next number must be the sum of
#' two of those numbers:
#'
#' -   `26` would be a *valid* next number, as it could be `1` plus `25`
#'     (or many other pairs, like `2` and `24`).
#' -   `49` would be a *valid* next number, as it is the sum of `24` and
#'     `25`.
#' -   `100` would *not* be valid; no two of the previous 25 numbers sum to
#'     `100`.
#' -   `50` would also *not* be valid; although `25` appears in the
#'     previous 25 numbers, the two numbers in the pair must be different.
#'
#' Suppose the 26th number is `45`, and the first number (no longer an
#' option, as it is more than 25 numbers ago) was `20`. Now, for the next
#' number to be valid, there needs to be some pair of numbers among
#' `1`-`19`, `21`-`25`, or `45` that add up to it:
#'
#' -   `26` would still be a *valid* next number, as `1` and `25` are still
#'     within the previous 25 numbers.
#' -   `65` would *not* be valid, as no two of the available numbers sum to
#'     it.
#' -   `64` and `66` would both be *valid*, as they are the result of
#'     `19+45` and `21+45` respectively.
#'
#' Here is a larger example which only considers the previous *5* numbers
#' (and has a preamble of length 5):
#'
#'     35
#'     20
#'     15
#'     25
#'     47
#'     40
#'     62
#'     55
#'     65
#'     95
#'     102
#'     117
#'     150
#'     182
#'     127
#'     219
#'     299
#'     277
#'     309
#'     576
#'
#' In this example, after the 5-number preamble, almost every number is the
#' sum of two of the previous 5 numbers; the only number that does not
#' follow this rule is *`127`*.
#'
#' The first step of attacking the weakness in the XMAS data is to find the
#' first number in the list (after the preamble) which is *not* the sum of
#' two of the 25 numbers before it. *What is the first number that does not
#' have this property?*
#'
#' **Part Two**
#'
#' The final step in breaking the XMAS encryption relies on the invalid
#' number you just found: you must *find a contiguous set of at least two
#' numbers* in your list which sum to the invalid number from step 1.
#'
#' Again consider the above example:
#'
#'     35
#'     20
#'     15
#'     25
#'     47
#'     40
#'     62
#'     55
#'     65
#'     95
#'     102
#'     117
#'     150
#'     182
#'     127
#'     219
#'     299
#'     277
#'     309
#'     576
#'
#' In this list, adding up all of the numbers from `15` through `40`
#' produces the invalid number from step 1, `127`. (Of course, the
#' contiguous set of numbers in your actual list might be much longer.)
#'
#' To find the *encryption weakness*, add together the *smallest* and
#' *largest* number in this contiguous range; in this example, these are
#' `15` and `47`, producing *`62`*.
#'
#' *What is the encryption weakness in your XMAS-encrypted list of
#' numbers?*
#'
#' @param x Vector of preamble numbers.
#' @param window_size size of the window for the summing rule.
#' @param target number to find the sum of in Part Two.
#' @return For Part One, `find_xmas_preamble_mismatch()` the first item in `x`
#'   where the previous `window_size` numbers do not contain a pair that sums to
#'   `x`. For Part Two, `find_summing_streak(x)` returns the streak of numbers
#'   that sum to `target`.
#' @export
#' @examples
#' x <- example_xmas_preamble()
#' result <- find_xmas_preamble_mismatch(x, 5)
#' find_summing_streak(x, result)
find_xmas_preamble_mismatch <- function(x, window_size) {
  # Generate subsets of window_size-length numbers
  windows <- x %>%
    shingle(window_size) %>%
    # Don't need the last one
    utils::head(-1)

  # Numbers that follow each subset
  to_check <- x[seq(window_size + 1, length(x))]

  first_failure <- to_check %>%
    find_value2(
      windows,
      function(n, x) !has_pair_summing_to_n(unlist(x), n)
    )

  first_failure[[1]]
}


has_pair_summing_to_n <- function(x, n) {
  grid <- outer(x, x, "+")
  # We need a pair of numbers that sum to n.
  # We don't want (1, 2, 3) to find (2, 2) for 4
  n %in% grid[lower.tri(grid)]
}


#' @rdname day09
#' @export
find_summing_streak <- function(x, target) {
  # trim off impossible values
  edge <- find_position(x, function(x) x < target, .dir = "backward")
  x2 <- x[-seq(edge + 1, to = length(x), by = 1)]

  # try progressively larger windows
  not_found <- TRUE
  size <- 2
  while (not_found) {
    find_result <- x2 %>%
      shingle(size) %>%
      find_value(function(x) sum(unlist(x)) == target)

    if (is.null(find_result)) {
      size <- size + 1
    } else (
      not_found <- FALSE
    )

    if (size > length(x2)) stop("No matching sequence")
  }

  find_result
}


shingle <- function(x, size) {
  index_starts <- seq(1, length(x) - size + 1)
  index_ends <- index_starts + size - 1

  index_starts %>%
    lapply2(index_ends, seq) %>%
    lapply(function(i) x[i])
}


#' @rdname day09
#' @export
example_xmas_preamble <- function() {
  c(
     35,  20,  15,  25,  47,
     40,  62,  55,  65,  95,
    102, 117, 150, 182, 127, # 127 is the bad one
    219, 299, 277, 309, 576
  )
}
