#' Day 10: Adapter Array
#'
#' [Adapter Array](https://adventofcode.com/2020/day/10)
#'
#' @name day10
#' @rdname day10
#' @details
#'
#' **Part One**
#'
#' Patched into the aircraft\'s data port, you discover weather forecasts
#' of a massive tropical storm. Before you can figure out whether it will
#' impact your vacation plans, however, your device suddenly turns off!
#'
#' Its battery is dead.
#'
#' You\'ll need to plug it in. There\'s only one problem: the charging
#' outlet near your seat produces the wrong number of *jolts*. Always
#' prepared, you make a list of all of the joltage adapters in your bag.
#'
#' Each of your joltage adapters is rated for a specific *output joltage*
#' (your puzzle input). Any given adapter can take an input `1`, `2`, or
#' `3` jolts *lower* than its rating and still produce its rated output
#' joltage.
#'
#' In addition, your device has a built-in joltage adapter rated for *`3`
#' jolts higher* than the highest-rated adapter in your bag. (If your
#' adapter list were `3`, `9`, and `6`, your device\'s built-in adapter
#' would be rated for `12` jolts.)
#'
#' Treat the charging outlet near your seat as having an effective joltage
#' rating of `0`.
#'
#' Since you have some time to kill, you might as well test all of your
#' adapters. Wouldn\'t want to get to your resort and realize you can\'t
#' even charge your device!
#'
#' If you *use every adapter in your bag* at once, what is the distribution
#' of joltage differences between the charging outlet, the adapters, and
#' your device?
#'
#' For example, suppose that in your bag, you have adapters with the
#' following joltage ratings:
#'
#'     16
#'     10
#'     15
#'     5
#'     1
#'     11
#'     7
#'     19
#'     6
#'     12
#'     4
#'
#' With these adapters, your device\'s built-in joltage adapter would be
#' rated for `19 + 3 = 22` jolts, 3 higher than the highest-rated adapter.
#'
#' Because adapters can only connect to a source 1-3 jolts lower than its
#' rating, in order to use every adapter, you\'d need to choose them like
#' this:
#'
#' -   The charging outlet has an effective rating of `0` jolts, so the
#'     only adapters that could connect to it directly would need to have a
#'     joltage rating of `1`, `2`, or `3` jolts. Of these, only one you
#'     have is an adapter rated `1` jolt (difference of *`1`*).
#' -   From your `1`-jolt rated adapter, the only choice is your `4`-jolt
#'     rated adapter (difference of *`3`*).
#' -   From the `4`-jolt rated adapter, the adapters rated `5`, `6`, or `7`
#'     are valid choices. However, in order to not skip any adapters, you
#'     have to pick the adapter rated `5` jolts (difference of *`1`*).
#' -   Similarly, the next choices would need to be the adapter rated `6`
#'     and then the adapter rated `7` (with difference of *`1`* and *`1`*).
#' -   The only adapter that works with the `7`-jolt rated adapter is the
#'     one rated `10` jolts (difference of *`3`*).
#' -   From `10`, the choices are `11` or `12`; choose `11` (difference of
#'     *`1`*) and then `12` (difference of *`1`*).
#' -   After `12`, only valid adapter has a rating of `15` (difference of
#'     *`3`*), then `16` (difference of *`1`*), then `19` (difference of
#'     *`3`*).
#' -   Finally, your device\'s built-in adapter is always 3 higher than the
#'     highest adapter, so its rating is `22` jolts (always a difference of
#'     *`3`*).
#'
#' In this example, when using every adapter, there are *`7`* differences
#' of 1 jolt and *`5`* differences of 3 jolts.
#'
#' Here is a larger example:
#'
#'     28
#'     33
#'     18
#'     42
#'     31
#'     14
#'     46
#'     20
#'     48
#'     47
#'     24
#'     23
#'     49
#'     45
#'     19
#'     38
#'     39
#'     11
#'     1
#'     32
#'     25
#'     35
#'     8
#'     17
#'     7
#'     9
#'     4
#'     2
#'     34
#'     10
#'     3
#'
#' In this larger example, in a chain that uses all of the adapters, there
#' are *`22`* differences of 1 jolt and *`10`* differences of 3 jolts.
#'
#' Find a chain that uses all of your adapters to connect the charging
#' outlet to your device\'s built-in adapter and count the joltage
#' differences between the charging outlet, the adapters, and your device.
#' *What is the number of 1-jolt differences multiplied by the number of
#' 3-jolt differences?*
#'
#' **Part Two**
#'
#' To completely determine whether you have enough adapters, you\'ll need
#' to figure out how many different ways they can be arranged. Every
#' arrangement needs to connect the charging outlet to your device. The
#' previous rules about when adapters can successfully connect still apply.
#'
#' The first example above (the one that starts with `16`, `10`, `15`)
#' supports the following arrangements:
#'
#'     (0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
#'     (0), 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, (22)
#'     (0), 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, (22)
#'     (0), 1, 4, 5, 7, 10, 12, 15, 16, 19, (22)
#'     (0), 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, (22)
#'     (0), 1, 4, 6, 7, 10, 12, 15, 16, 19, (22)
#'     (0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)
#'     (0), 1, 4, 7, 10, 12, 15, 16, 19, (22)
#'
#' (The charging outlet and your device\'s built-in adapter are shown in
#' parentheses.) Given the adapters from the first example, the total
#' number of arrangements that connect the charging outlet to your device
#' is *`8`*.
#'
#' The second example above (the one that starts with `28`, `33`, `18`) has
#' many arrangements. Here are a few:
#'
#'     (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
#'     32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 48, 49, (52)
#'
#'     (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
#'     32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 49, (52)
#'
#'     (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
#'     32, 33, 34, 35, 38, 39, 42, 45, 46, 48, 49, (52)
#'
#'     (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
#'     32, 33, 34, 35, 38, 39, 42, 45, 46, 49, (52)
#'
#'     (0), 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
#'     32, 33, 34, 35, 38, 39, 42, 45, 47, 48, 49, (52)
#'
#'     (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
#'     46, 48, 49, (52)
#'
#'     (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
#'     46, 49, (52)
#'
#'     (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
#'     47, 48, 49, (52)
#'
#'     (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
#'     47, 49, (52)
#'
#'     (0), 3, 4, 7, 10, 11, 14, 17, 20, 23, 25, 28, 31, 34, 35, 38, 39, 42, 45,
#'     48, 49, (52)
#'
#' In total, this set of adapters can connect the charging outlet to your
#' device in *`19208`* distinct arrangements.
#'
#' You glance back down at your bag and try to remember why you brought so
#' many adapters; there must be *more than a trillion* valid ways to
#' arrange them! Surely, there must be [an efficient
#' way]{title="Definitely itertools."} to count the arrangements.
#'
#' *What is the total number of distinct ways you can arrange the adapters
#' to connect the charging outlet to your device?*
#'
#' @param x some data
#' @return For Part One, `count_adapter_differences()` returns the counts of
#'   differences of 1, 2, 3 between adapters. For Part Two,
#'   `count_adapter_orders(x)` returns the total number of adapter orders.
#' @export
#' @examples
#' x <- example_adapter_data()
#' count_adapter_differences(x)
#' count_adapter_orders(x)
#' y <- example_adapter_data(2)
#' count_adapter_differences(y)
#' count_adapter_orders(y)
count_adapter_differences <- function(x) {
  x <- sort(x)
  outlet <- 0
  builtin <- max(x) + 3

  c(outlet, x, builtin) %>%
    diff() %>%
    # Use a factor to count 2s and have 0s in the table
    factor(levels = c(1, 2, 3)) %>%
    table() %>%
    as.numeric()
}


#' @rdname day10
#' @export
count_adapter_orders <- function(x) {
  x <- sort(x)
  outlet <- 0
  builtin <- max(x) + 3
  x <- c(outlet, x, builtin)

  get_one_lump_sizes <- function(x) {
    lumps <- rle(diff(x))
    lumps$lengths[lumps$values == 1]
  }

  lumps <- get_one_lump_sizes(x)
  lumps[lumps == 4] <- 7
  lumps[lumps == 3] <- 4
  lumps[lumps == 2] <- 2
  lumps[lumps == 1] <- 1
  prod(lumps)
}


# This will take some math huh. I'll takes my notes.
count_adapter_orders_notes <- function(x) {
  # Let's look at the what the example data does

  # (0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
  # (0), 1, 4, 5, 6, 7, 10,     12, 15, 16, 19, (22) // 11
  # (0), 1, 4, 5,    7, 10, 11, 12, 15, 16, 19, (22) // 6
  # (0), 1, 4, 5,    7, 10,     12, 15, 16, 19, (22) // 6 and 11
  # (0), 1, 4,    6, 7, 10, 11, 12, 15, 16, 19, (22) // 5
  # (0), 1, 4,    6, 7, 10,     12, 15, 16, 19, (22) // 5 and 11
  # (0), 1, 4,       7, 10, 11, 12, 15, 16, 19, (22) // 5 and 6
  # (0), 1, 4,       7, 10,     12, 15, 16, 19, (22) // 5 and 6 and 11

  # There are three gaps being toggled so 2 ^ 3 = 8. Find the gaps and win.
  # But we need to find a way that selects 11, 6, 5 but not 15, 16.

  # Let's look closer
  x <- sort(example_adapter_data())
  x <- c(0, x, max(x) + 3)
  diffs <- diff(x)
  #    data: (0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
  #    diff:      1  3  1  1  1   3   1   1   3   1   3    3
  # removable:          x  x          x
  # Maybe diffs of 1 preceded in the contexts (1, 1*, 1) or (1, 1*, 3)?
  # No, that would overgeneralize so 1, 1, 1, 1, 1, 3 could have a gap of 4

  # We could try to solve the second example. The answer is 19208 with 22 gaps
  # of 1 and 10 gaps of 3.
  # 19208 factors into (2^3) * (7^4)
  y <- sort(example_adapter_data(2))
  y <- c(0, y, max(y) + 3)
  diffs <- diff(c(0, y, max(y) + 3))

  # Let's assume that the given differences can only be 1 or 3.

  # Let's try solving the cases in the actual problem.

  # This gives us the size of the streaks of 1s
  get_one_lump_sizes <- function(x) {
    lumps <- rle(diff(x))
    lumps$lengths[lumps$values == 1]
  }
  get_one_lump_sizes(x)
  get_one_lump_sizes(y)

  # The two examples and my problem input only have lumps up size 4.

  # 0
  #  case: 3, 6
  #  diff:    3
  # order: 1

  # 1
  #  case: 3, 6, 7, 10
  #  diff:    3, 1, 3
  # order: 1

  # 2
  #  case: 3, 6, 7, 8, 11
  #  diff:    3, 1, 1,  3
  # order: 2 (drop none or 7)

  # 3
  #  case: 3, 6, 7, 8, 9, 12
  #  diff:    3, 1, 1, 1,  3
  # order: 4 (drop none, 7, 9, 7 + 9)

  # 4
  #  case: 3, 6, 7, 8, 9, 10, 13
  #  case: 3, 6, x, 8, 9, 10, 13
  #  case: 3, 6, x, x, 9, 10, 13
  #  case: 3, 6, 7, x, 9, 10, 13
  #  case: 3, 6, 7, x, x, 10, 13
  #  case: 3, 6, 7, 8, x, 10, 13
  #  case: 3, 6, x, 8, x, 10, 13
  #  diff:    3, 1, 1, 1,  1,  3
  # order: 7 (drop none, 7, 8, 9, 7+8, 8+9, 7+9)

  # There's the 7 from the factorization!!!
  # Maybe I should notice that case(4) = case(3) + case(2) + case(1)?

  lumps <- get_one_lump_sizes(y)
  lumps[lumps == 4] <- 7
  lumps[lumps == 3] <- 4
  lumps[lumps == 2] <- 2
  lumps[lumps == 1] <- 1
  prod(lumps)
}


#' @param example_number which example data to use (1 or 2). Defaults to 1.
#' @rdname day10
#' @export
example_adapter_data <- function(example_number = 1) {
  l <- list(
    c(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4),
    c(
      28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19,
      38, 39, 11,  1, 32, 25, 35,  8, 17,  7,  9,  4,  2, 34, 10,
      3
    )
  )
  l[[example_number]]
}
