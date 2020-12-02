#' Split and trim new-line-delimited string of values
#'
#' @param x a string with values separated by newlines
#' @return a character vector of values split at newlines and with leading and
#'   trailing spaces removed
#' @export
#' @examples
#' read_text_lines("a\n  b  \nc    ")
read_text_lines <- function(x) {
  x %>%
    strsplit("\\n") %>%
    unlist() %>%
    stringr::str_trim() %>%
    keep_if(function(x) x != "")
}

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


lapply2 <- function(x, y, f, ...) {
  Map(f, x, y, ...)
}
