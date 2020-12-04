#' Split and trim new-line-delimited string of values
#'
#' THis function mimics some of the nice thing glue does with trimming. The
#' option to keep internal blanks was added for puzzle 2020:04.
#'
#'
#' @param x a string with values separated by newlines
#' @param drop_empty whether to remove empty items (`""`) from the results.
#'   Defaults to `"all"`. Other options are `"head/tail"` (drops blanks from
#'   start and end) and `"none"`.
#' @return a character vector of values split at newlines and with leading and
#'   trailing spaces removed.
#' @export
#' @examples
#' read_text_lines("a\n  b  \nc    ")
#'
#' read_text_lines("\n\na\n  b  \n\nc    \n\n", drop_empty = "none")
#' read_text_lines("\n\na\n  b  \n\nc    \n\n", drop_empty = "all")
#' read_text_lines("\n\na\n  b  \n\nc    \n\n", drop_empty = "head/tail")
read_text_lines <- function(x, drop_empty = "all") {
  lines <- x %>%
    strsplit("\\n") %>%
    unlist() %>%
    stringr::str_trim()

  if (drop_empty == "all") {
    lines <- keep_if(lines, function(x) x != "")
  } else if (drop_empty == "head/tail") {
    while (lines[length(lines)] == "") {
      lines <- lines[-length(lines)]
    }

    while (lines[1] == "") {
      lines <- lines[-1]
    }
  }
  lines
}
