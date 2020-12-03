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
