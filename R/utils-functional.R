
#' Filter values
#'
#' These are simple functions for filtering data.
#'
#' @param data a list to filter
#' @param predicate a function for filtering items. Values that return `TRUE`
#'   are kept.
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

invoke_call <- function(args, what, ...) {
  do.call(what, args, ...)
}

