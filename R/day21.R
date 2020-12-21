#' Day 21: Allergen Assessment
#'
#' [Allergen Assessment](https://adventofcode.com/2020/day/21)
#'
#' @name day21
#' @rdname day21
#' @details
#'
#' **Part One**
#'
#' You reach the train\'s last stop and the closest you can get to your
#' vacation island without getting wet. There aren\'t even any boats here,
#' but nothing can stop you now: you build a raft. You just need a few
#' days\' worth of food for your journey.
#'
#' You don\'t speak the local language, so you can\'t read any ingredients
#' lists. However, sometimes, allergens are listed in a language you *do*
#' understand. You should be able to use this information to determine
#' which ingredient contains which allergen and [work out which foods are
#' safe]{title="I actually considered doing this once. I do not recommend it."}
#' to take with you on your trip.
#'
#' You start by compiling a list of foods (your puzzle input), one food per
#' line. Each line includes that food\'s *ingredients list* followed by
#' some or all of the allergens the food contains.
#'
#' Each allergen is found in exactly one ingredient. Each ingredient
#' contains zero or one allergen. *Allergens aren\'t always marked*; when
#' they\'re listed (as in `(contains nuts, shellfish)` after an ingredients
#' list), the ingredient that contains each listed allergen will be
#' *somewhere in the corresponding ingredients list*. However, even if an
#' allergen isn\'t listed, the ingredient that contains that allergen could
#' still be present: maybe they forgot to label it, or maybe it was labeled
#' in a language you don\'t know.
#'
#' For example, consider the following list of foods:
#'
#'     mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
#'     trh fvjkl sbzzf mxmxvkd (contains dairy)
#'     sqjhc fvjkl (contains soy)
#'     sqjhc mxmxvkd sbzzf (contains fish)
#'
#' The first food in the list has four ingredients (written in a language
#' you don\'t understand): `mxmxvkd`, `kfcds`, `sqjhc`, and `nhms`. While
#' the food might contain other allergens, a few allergens the food
#' definitely contains are listed afterward: `dairy` and `fish`.
#'
#' The first step is to determine which ingredients *can\'t possibly*
#' contain any of the allergens in any food in your list. In the above
#' example, none of the ingredients `kfcds`, `nhms`, `sbzzf`, or `trh` can
#' contain an allergen. Counting the number of times any of these
#' ingredients appear in any ingredients list produces *`5`*: they all
#' appear once each except `sbzzf`, which appears twice.
#'
#' Determine which ingredients cannot possibly contain any of the allergens
#' in your list. *How many times do any of those ingredients appear?*
#'
#' **Part Two**
#'
#' Now that you\'ve isolated the inert ingredients, you should have enough
#' information to figure out which ingredient contains which allergen.
#'
#' In the above example:
#'
#' -   `mxmxvkd` contains `dairy`.
#' -   `sqjhc` contains `fish`.
#' -   `fvjkl` contains `soy`.
#'
#' Arrange the ingredients *alphabetically by their allergen* and separate
#' them by commas to produce your *canonical dangerous ingredient list*.
#' (There should *not be any spaces* in your canonical dangerous ingredient
#' list.) In the above example, this would be *`mxmxvkd,sqjhc,fvjkl`*.
#'
#' Time to stock your raft with supplies. *What is your canonical dangerous
#' ingredient list?*
#'
#' @param x some data
#' @return For Part One, `deduce_safe_ingredients(x)` returns a character vector
#'   with all of the safe ingredients. For Part Two,
#'   `deduce_allergenic_ingredients(x)` returns a comma-separated string of the
#'   allergen names.
#' @export
#' @examples
#' deduce_safe_ingredients(example_ingredients())
#' deduce_allergenic_ingredients(example_ingredients())
deduce_safe_ingredients <- function(x) {
  # x <- example_ingredients()
  x0 <- parse_ingredients(x)
  x <- x0

  while (!x$solved) {
    x <- simplify_ingredients(x)
  }

  original <- unlist(x0$ingredients)
  solved <- unlist(x$flag_intersects, use.names = FALSE)
  original[! original %in% solved]
}


#' @rdname day21
#' @export
deduce_allergenic_ingredients <- function(x) {
  x <- parse_ingredients(x)

  while (!x$solved) {
    x <- simplify_ingredients(x)
  }

  allergens <- x$flag_intersects
  allergens[sort(names(allergens))] %>%
    unlist(use.names = FALSE) %>%
    paste0(collapse = ",")
}


simplify_ingredients <- function(x) {
  # Remove any uniquely specified ingredients from the original text
  for (i in seq_along(x$flag_intersects)) {
    this_flag <- names(x$flag_intersects[i])
    if (length(x$flag_intersects[[i]]) == 1) {
      this_ingredient <- x$flag_intersects[[i]]
      re_this_flag <- paste0(" ", this_flag, " ")
      re_this_ingredient <- paste0(" ", this_ingredient, " ")
      x$lines <- x$lines %>%
        stringr::str_replace_all(re_this_flag, " ") %>%
        stringr::str_replace_all(re_this_ingredient, " ") %>%
        c(
          paste0(re_this_ingredient, ":", re_this_flag)
        )
    }
  }

  # Here is where we would implement the indeterminate case if needed:
  #
  # Technically, we could get into the case with
  #
  # spr frt something : dairy shellfish
  # spr frt otherthing : dairy shellfish
  #
  # where the deduction is that spr frt are unsafe but we cannot determine which
  # one is shellfish. We could still simplify all of the cases to remove these
  # ingredients and flags.
  #
  # But that is not necessary for our test data

  parse_ingredients(unique(x$lines))
}


parse_ingredients <- function(x) {
  x <- normalize_ingredients(x)

  ingredients <- x %>%
    stringr::str_extract_all(".*:") %>%
    stringr::str_extract_all("\\w+")

  flags <- x %>%
    stringr::str_extract_all(":.*") %>%
    stringr::str_extract_all("\\w+")

  unique_flags <- unique(unlist(flags))
  unique_ingredients <- unique(unlist(ingredients))

  # Which ingredient lists mention each flag
  which_with_flags <- unique_flags %>%
    lapply(function(x) {
      flags %>%
        lapply(match, table = x) %>%
        lapply(any) %>%
        unlist() %>%
        which()
    }) %>%
    stats::setNames(unique_flags)

  # Take intersection of all lists that contain a given flag
  flag_intersects <- which_with_flags %>%
    lapply(function(i) ingredients[i]) %>%
    lapply(function(i) Reduce(intersect, i)) %>%
    lapply(sort)

  solved <- all(lengths(flag_intersects) == 1)

  list(
    lines = x,
    flags = flags,
    ingredients = ingredients,
    unique_ingredients = unique_ingredients,
    unique_flags = unique_flags,
    flag_intersects = flag_intersects,
    solved = solved
  )
}


# Make strings easier repeated calls
normalize_ingredients <- function(x) {
  x %>%
    stringr::str_remove_all(",") %>%
    stringr::str_remove_all("contains ") %>%
    stringr::str_remove_all("[)]") %>%
    stringr::str_replace_all("[(]", " : ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim() %>%
    paste0(" ", ., " ")
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day21
#' @export
example_ingredients <- function(example = 1) {
  l <- list(
    a = c(
      "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
      "trh fvjkl sbzzf mxmxvkd (contains dairy)",
      "sqjhc fvjkl (contains soy)",
      "sqjhc mxmxvkd sbzzf (contains fish)"
    )
  )
  l[[example]]
}
