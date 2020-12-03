
#' Filter values
#'
#' This function is [base::Filter()] with its arguments reversed.
#'
#' @param data a list to filter
#' @param predicate a function for filtering items. Values that return `TRUE`
#'   are kept.
#' @return the items where the predicate function is `TRUE`
#' @export
keep_if <- function(data, predicate) {
  Filter(predicate, data)
}

#' Map over two lists
#'
#' This function wraps over [base::Map()] to create a replacement for
#' [purrr::map2()].
#'
#' @param x,y lists to apply a 2-argument function over.
#' @param f a function to apply.
#' @param ... other arguments to the function.
#' @return the result of applying the function to each pair of items in `x` and
#'   `y`.
#' @export
lapply2 <- function(x, y, f, ...) {
  Map(f, x, y, ...)
}
