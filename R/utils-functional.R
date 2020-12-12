
#' Filter values
#'
#' These are simple functions for filtering data.
#'
#' @param data a list to filter
#' @param predicate a predicate function for filtering items. Values that return
#'   `TRUE` are kept.
#' @param names names to control filtering.
#' @return the items where the predicate function is `TRUE` or items that match
#'   the name selection.
#' @export
#'
#' @details `keep_if()` is [base::Filter()] with its arguments reversed.
#'
#' @rdname filtering
keep_if <- function(data, predicate) {
  Filter(predicate, data)
}

#' @export
#' @rdname filtering
drop_if_has_name <- function(data, names) {
  data[names(data) != names]
}

#' Find first item that matches certain conditions
#'
#' The argument names here are meant to follow [purrr::detect].
#'
#' @param .x,.y Vectors to iterate over. For 2-argument version, `.x`, `.y` must
#'   have the same length.
#' @param .p A predicate function for determining matches.
#' @param ... Additional arguments to set for the predicate
#' @param .dir Direction to traverse the items. One of `"forward"` (default) or
#'   `"backward"`.
#' @param .default Value used when no match is found.
#' @return either the `.default` value or the values of the first match.
#' @export
#' @rdname finding
#' @examples
#' find_value2(1:10, -4:5, function(x, y) x + y > 10)
#' find_value2(1:10, -4:5, function(x, y) x + y > 11)
#' find_value2(1:10, -4:5, function(x, y) x + y > 100)
find_value2 <- function(
  .x,
  .y,
  .p,
  ...,
  .dir = c("forward", "backward"),
  .default = NULL
) {
  .dir <- match.arg(.dir)
  .right <- .dir == "backward"

  # Work on positions. nomatch is 0 for no valid position
  ix <- seq_along(.x)
  p2 <- function(i) .p(.x[i], .y[i], ...)
  result <- Find(p2, x = ix, right = .right, nomatch = 0)

  # Convert positions into values
  if (result == 0) {
    .default
  } else(
    list(.x[[result]], .y[[result]])
  )
}

#' @export
#' @rdname finding
find_value <- function(
  .x,
  .p,
  ...,
  .dir = c("forward", "backward"),
  .default = NULL
) {
  result <- find_position(.x, .p, ... , .dir = .dir, .default = 0)

  # Convert positions into values
  if (result == 0) {
    .default
  } else(
    .x[[result]]
  )
}

#' @export
#' @rdname finding
find_position <- function(
  .x,
  .p,
  ...,
  .dir = c("forward", "backward"),
  .default = NULL
) {
  .dir <- match.arg(.dir)
  .right <- .dir == "backward"

  # Work on positions. nomatch is 0 for no valid position
  ix <- seq_along(.x)
  p2 <- function(i) .p(.x[i], ...)
  Find(p2, x = ix, right = .right, nomatch = .default)
}

#' Map over two lists
#'
#' This function wraps over [base::Map()] to create a replacement for
#' [purrr::map2()].
#'
#' @param x,y,z lists to apply a n-argument function over.
#' @param f a function to apply.
#' @param ... other arguments to the function.
#' @return the result of applying the function to each set of items in `x` and
#'   `y` or `z`.
#' @export
#' @rdname maps
lapply2 <- function(x, y, f, ...) {
  Map(f, x, y, ...)
}

#' @export
#' @rdname maps
lapply3 <- function(x, y, z, f, ...) {
  Map(f, x, y, z, ...)
}

invoke_call <- function(args, what, ...) {
  do.call(what, args, ...)
}

