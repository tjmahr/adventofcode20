#' Day 02: Password Philosophy
#'
#' [Password Philosophy](https://adventofcode.com/2020/day/2)
#'
#' @name day02
#' @rdname day02
#' @details
#'
#' **Part One**
#'
#' Your flight departs in a few days from the coastal airport; the easiest way
#' down to the coast from here is via
#' [toboggan](https://en.wikipedia.org/wiki/Toboggan).
#'
#' The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day.
#' \"Something\'s wrong with our computers; we can\'t log in!\" You ask if you
#' can take a look.
#'
#' Their password database seems to be a little corrupted: some of the passwords
#' wouldn\'t have been allowed by the [Official Toboggan Corporate
#' Policy]{title="To ensure your safety, your password must be the following
#' string..."} that was in effect when they were chosen.
#'
#' To try to debug the problem, they have created a list (your puzzle input) of
#' *passwords* (according to the corrupted database) and *the corporate policy
#' when that password was set*.
#'
#' For example, suppose you have the following list:
#'
#' 1-3 a: abcde 1-3 b: cdefg 2-9 c: ccccccccc
#'
#' Each line gives the password policy and then the password. The password
#' policy indicates the lowest and highest number of times a given letter must
#' appear for the password to be valid. For example, `1-3 a` means that the
#' password must contain `a` at least `1` time and at most `3` times.
#'
#' In the above example, `2` passwords are valid. The middle password, `cdefg`,
#' is not; it contains no instances of `b`, but needs at least `1`. The first
#' and third passwords are valid: they contain one `a` or nine `c`, both within
#' the limits of their respective policies.
#'
#' *How many passwords are valid* according to their policies?
#'
#' **Part Two**
#'
#' While it appears you validated the passwords correctly, they don\'t seem
#' to be what the Official Toboggan Corporate Authentication System is
#' expecting.
#'
#' The shopkeeper suddenly realizes that he just accidentally explained the
#' password policy rules from his old job at the sled rental place down the
#' street! The Official Toboggan Corporate Policy actually works a little
#' differently.
#'
#' Each policy actually describes two *positions in the password*, where
#' `1` means the first character, `2` means the second character, and so
#' on. (Be careful; Toboggan Corporate Policies have no concept of \"index
#' zero\"!) *Exactly one of these positions* must contain the given letter.
#' Other occurrences of the letter are irrelevant for the purposes of
#' policy enforcement.
#'
#' Given the same example list from above:
#'
#' -   `1-3 a: abcde` is *valid*: position `1` contains `a` and position
#'     `3` does not.
#' -   `1-3 b: cdefg` is *invalid*: neither position `1` nor position `3`
#'     contains `b`.
#' -   `2-9 c: ccccccccc` is *invalid*: both position `2` and position `9`
#'     contain `c`.
#'
#' *How many passwords are valid* according to the new interpretation of
#' the policies?
#'
#' @param x password data
#' @return For Part One, `check_for_valid_passwords(x)` returns a dataframe of
#'   parsed and validated passwords. For Part Two,
#'   `check_for_valider_passwords(x)` returns a dataframe of parsed and
#'   validated passwords.
#' @export
#' @examples
#' input <- "
#'   1-3 a: abcde
#'   1-3 b: cdefg
#'   2-9 c: ccccccccc
#'   "
#'
#' x <- read_text_lines(input)
#' check_for_valid_passwords(x)
#'
#' check_for_valider_passwords(x)
check_for_valid_passwords <- function(x) {
  x_parsed <- parse_password_lines(x)

  # I could use stringr::str_count()
  #   stringr::str_count(x_parsed$password, x_parsed$target)
  # but let's keep it going

  x_parsed[["target_count"]] <- count_target_letters(
    x_parsed[["password"]],
    x_parsed[["target"]]
  )

  under <- x_parsed[["target_count"]] <= x_parsed[["max"]]
  over <- x_parsed[["min"]] <= x_parsed[["target_count"]]

  x_parsed[["is_valid"]] <- under & over
  x_parsed
}

#' @rdname day02
#' @export
check_for_valider_passwords <- function(x) {
  x_parsed <- parse_password_lines(x)

  # Part 1 does a fine job,  so let's change the strings and reuse the letter
  # counting code.
  char1 <- substr(x_parsed[["password"]], x_parsed[["min"]], x_parsed[["min"]])
  char2 <- substr(x_parsed[["password"]], x_parsed[["max"]], x_parsed[["max"]])
  x_parsed[["reduced"]] <- paste0(char1, char2)

  x_parsed[["target_count"]] <- count_target_letters(
    x_parsed[["reduced"]],
    x_parsed[["target"]]
  )

  x_parsed[["is_valid"]] <- x_parsed[["target_count"]] == 1
  x_parsed
}

count_target_letters <- function(xs, ys) {
  xs %>%
    lapply(count_letters_in_string) %>%
    lapply2(ys, function(x, y) x[y]) %>%
    unlist()
}

count_letters_in_string <- function(x) {
  x_counts <- x %>%
    strsplit("") %>%
    unlist() %>%
    # use a factor plus table() to get 0 counts
    factor(letters) %>%
    table()

  x_counts %>%
    as.integer() %>%
    stats::setNames(names(x_counts))
}

parse_password_lines <- function(x) {
  x <- x %>%
    stringr::str_match("(\\d+)-(\\d+) (\\w): (\\w+)") %>%
    as.data.frame() %>%
    stats::setNames(
      c("input", "min", "max", "target", "password")
    )

  x[["min"]] <- as.numeric(x[["min"]])
  x[["max"]] <- as.numeric(x[["max"]])
  x
}
