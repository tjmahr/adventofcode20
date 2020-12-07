#' Create the files for an Advent of Code day
#'
#' @param day integer giving the day
#' @param year year of Advent of Code containing the day. Defaults to 2020.
#' @param open whether to open the created files. Defaults to `TRUE` in an
#'   interactive R session.
#' @return `use_day()` returns `NULL`. `convert_clipboard_html_to_roxygen_md()`
#'   invisibly returns the Roxygen markdown block. It also copies Roxygen
#'   markdown block onto the clipboard.
#' @rdname use_day
#'
#' @details
#'
#' Creates a file for writing the functions to solve the problem: `R/dayxx.R`.
#' The text of the challenge is downloaded and inserted into the roxygen block.
#' One caveat is that you will have to manually add the markdown text for Part
#' Two yourself. You can use `convert_clipboard_html_to_roxygen_md()` to make
#' this easier. Once you can read the description, view the page source, copy
#' the html for that part of the problem. Run this function to create a Roxygen
#' version of the HTML.
#'
#' Also, creates a placeholder file for the problem input: `inst/inputxx.txt`.
#' Paste your input here.
#'
#' Also, creates a file for unit tests: `tests/testthat/test-dayxx.R`. This is
#' good place to test that the examples in the problem description work.
#'
#' Finally, creates a solution file: `inst/run-dayxx.R`. You should download
#' your personalized challenge input as `inst/inputxx.txt`. Your solution file
#' should read in this file and apply your functions to it. Once your solution
#' passes on the site, store it in `R/data-solutions.R`. Then the solution file
#' can load in your previous answer, rerun your solution, and check whether your
#' code no longer obtains the same solution.
#' @export
use_day <- function(day, year = 2020, open = interactive()) {

  year_short <- substr(year, 3, 4)
  this_package <- paste0("adventofcode", year_short)

  url <- sprintf("https://adventofcode.com/%s/day/%s", year, day)

  data <- list(
    dd_number = sprintf("%02.f", day),
    url = url,
    title = NA,
    part_1 = NA
  )

  files <- get_day_files(day)
  test_name <- sprintf("day%s", data$dd_number)

  page <- xml2::read_html(url)
  article <- xml2::xml_find_first(page, "/html/body/main/article")

  title <- xml2::xml_find_first(page, "/html/body/main/article/h2")
  title <- unlist(xml2::as_list(title))
  title <- stringr::str_replace(title, "--- Day \\d+: ", "")
  title <- stringr::str_replace(title, " ---", "")
  data$title <- title

  temp <- tempfile(fileext = ".html")
  xml2::write_html(article, temp)
  z <- knitr::pandoc(temp, "markdown")
  lines <- readr::read_lines(z)
  lines <- lines[-c(1, 2, 3)]
  lines <- paste0("#' ", lines, collapse = "\n")
  data$part_1 <- lines

  usethis::use_template(
    "day.R",
    save_as = files$main,
    package = this_package,
    data = data,
    open = open
  )
  todo("Write your solution code here")
  todo(
    "Once you unlock Part Two, update the Roxygen block with the description")

  usethis::use_template(
    "input.txt",
    save_as = files$input,
    package = this_package,
    data = list(x = "\n"),
    open = open
  )
  todo("Copy your problem input into this file")

  usethis::use_test(
    name = test_name
  )
  todo("Write unit tests using the examples from the problem description")

  usethis::use_template(
    "solution.R",
    save_as = files$solution,
    package = this_package,
    data = data,
    open = open
  )
  todo("Run your solution on the input here. Once it works, update R/data-solutions.R")

  invisible(NULL)
}

#' @rdname use_day
#' @export
#' @param input html code copied from the Advent of Code website
convert_clipboard_html_to_roxygen_md <- function(input = clipr::read_clip()) {
  temp <- tempfile(fileext = ".html")
  writeLines(input, temp)
  z <- knitr::pandoc(temp, "markdown")
  lines <- readr::read_lines(z)
  lines <- paste0("#' ", lines, collapse = "\n")
  clipr::write_clip(lines)
  usethis::ui_done("Roxygen markdown block is on the clipboard")
  invisible(lines)
}

remove_day <- function(day) {
  file.remove(unlist(get_day_files(day)))
}

get_day_files <- function(day) {
  dd_number <- sprintf("%02.f", day)
  list(
    main = sprintf("R/day%s.R", dd_number),
    solution = sprintf("inst/run-day%s.R", dd_number),
    test = sprintf("tests/testthat/test-day%s.R", dd_number),
    input = sprintf("inst/input%s.txt", dd_number)
  )
}


# These are all copied from usethis in order to match that package's style

todo <- function(..., .envir = parent.frame()) {
  out <- glue::glue(..., .envir = .envir)
  cat_line(bulletize(out, bullet = todo_bullet()))
}

todo_bullet <- function () crayon::red(clisymbols::symbol$bullet)

done <- function (..., .envir = parent.frame()) {
  out <- glue::glue(..., .envir = .envir)
  cat_line(bulletize(out, bullet = done_bullet()))
}

done_bullet <- function () crayon::green(clisymbols::symbol$tick)

bulletize <- function(line, bullet = "*") {
  paste0(bullet, " ", line)
}

cat_line <- function(..., quiet = getOption("usethis.quiet", default = FALSE)) {
  if (quiet) return(invisible())
  cat(..., "\n", sep = "")
}

