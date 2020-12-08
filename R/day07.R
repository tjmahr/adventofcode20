#' Day 07: Handy Haversacks
#'
#' [Handy Haversacks](https://adventofcode.com/2020/day/7)
#'
#' @name day07
#' @rdname day07
#' @details
#'
#' **Part One**
#'
#' You land at the regional airport in time for your next flight. In fact, it
#' looks like you'll even have time to grab some food: all flights are
#' currently delayed due to *issues in luggage processing*.
#'
#' Due to recent aviation regulations, many rules (your puzzle input) are being
#' enforced about bags and their contents; bags must be color-coded and must
#' contain specific quantities of other color-coded bags. Apparently, nobody
#' responsible for these regulations considered how long they would take to
#' enforce!
#'
#' For example, consider the following rules:
#'
#' light red bags contain 1 bright white bag, 2 muted yellow bags. dark orange
#' bags contain 3 bright white bags, 4 muted yellow bags. bright white bags
#' contain 1 shiny gold bag. muted yellow bags contain 2 shiny gold bags, 9
#' faded blue bags. shiny gold bags contain 1 dark olive bag, 2 vibrant plum
#' bags. dark olive bags contain 3 faded blue bags, 4 dotted black bags. vibrant
#' plum bags contain 5 faded blue bags, 6 dotted black bags. faded blue bags
#' contain no other bags. dotted black bags contain no other bags.
#'
#' These rules specify the required contents for 9 bag types. In this example,
#' every `faded blue` bag is empty, every `vibrant plum` bag contains 11 bags (5
#' `faded blue` and 6 `dotted black`), and so on.
#'
#' You have a `shiny gold` bag. If you wanted to carry it in at least one other
#' bag, how many different bag colors would be valid for the outermost bag? (In
#' other words: how many colors can, eventually, contain at least one `shiny
#' gold` bag?)
#'
#' In the above rules, the following options would be available to you:
#'
#' - A `bright white` bag, which can hold your `shiny gold` bag directly.
#' - A `muted yellow` bag, which can hold your `shiny gold` bag directly, plus
#'   some other bags.
#' - A `dark orange` bag, which can hold `bright white` and `muted yellow` bags,
#'   either of which could then hold your `shiny gold` bag.
#' - A `light red` bag, which can hold `bright white` and `muted yellow` bags,
#'   either of which could then hold your `shiny gold` bag.
#'
#' So, in this example, the number of bag colors that can eventually contain at
#' least one `shiny gold` bag is `4`.
#'
#' *How many bag colors can eventually contain at least one `shiny gold` bag?*
#' (The list of rules is quite long; make sure you get all of it.)
#'
#' **Part Two**
#'
#' It's getting pretty expensive to fly these days - not because of ticket
#' prices, but because of the ridiculous number of bags you need to buy!
#'
#' Consider again your `shiny gold` bag and the rules from the above
#' example:
#'
#' -   `faded blue` bags contain `0` other bags.
#' -   `dotted black` bags contain `0` other bags.
#' -   `vibrant plum` bags contain `11` other bags: 5 `faded blue` bags and
#'     6 `dotted black` bags.
#' -   `dark olive` bags contain `7` other bags: 3 `faded blue` bags and 4
#'     `dotted black` bags.
#'
#' So, a single `shiny gold` bag must contain 1 `dark olive` bag (and the 7
#' bags within it) plus 2 `vibrant plum` bags (and the 11 bags within
#' *each* of those): `1 + 1*7 + 2 + 2*11` = `32` bags!
#'
#' Of course, the actual rules have a small ("100%") chance of going
#' several levels deeper than this example; be sure to count all of the
#' bags, even if the nesting becomes topologically impractical!
#'
#' Here's another example:
#'
#'     shiny gold bags contain 2 dark red bags.
#'     dark red bags contain 2 dark orange bags.
#'     dark orange bags contain 2 dark yellow bags.
#'     dark yellow bags contain 2 dark green bags.
#'     dark green bags contain 2 dark blue bags.
#'     dark blue bags contain 2 dark violet bags.
#'     dark violet bags contain no other bags.
#'
#' In this example, a single `shiny gold` bag must contain `126` other
#' bags.
#'
#' *How many individual bags are required inside your single `shiny gold`
#' bag?*
#'
#' @param x character vector of bagging rules
#' @param rule a single bag rule
#' @param repetitions whether to change bag counts to 1 during parsing the
#'   rules. For example, `9 faded blue bags` would be treated as `1 faded blue
#'   bag`. We do not need the repetitions for the part 1.
#' @return For Part One, `eval_bag_rules(x)` returns a list that says the bags
#'   that would be opened based on each rule. For Part Two,
#'   `eval_shiny_gold_bag_rule(x)` returns the bags that would be opened when a
#'   `"shiny gold"` bag is opened (including itself).
#' @export
#' @examples
#' xraw <- "
#'   light red bags contain 1 bright white bag, 2 muted yellow bags.
#'   dark orange bags contain 3 bright white bags, 4 muted yellow bags.
#'   bright white bags contain 1 shiny gold bag.
#'   muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
#'   shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
#'   dark olive bags contain 3 faded blue bags, 4 dotted black bags.
#'   vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
#'   faded blue bags contain no other bags.
#'   dotted black bags contain no other bags.
#' "
#' x <- read_text_lines(xraw)
#' eval_bag_rules(x)[6]
#' eval_shiny_gold_bag_rule(x)
eval_bag_rules <- function(x, repetitions = TRUE) {
  # Create an environment with functions defined from each rule
  e <- setup_bag_environment(x, repetitions)

  # Evaluate them in order
  code_results <- as.list(seq_along(x))
  for (line_i in seq_along(x)) {
    next_function <- parse_bag_rule_to_function(x[line_i])
    code_results[[line_i]] <- do.call(next_function$f_name, list(), envir = e)
    names(code_results)[line_i] <- next_function$this_color
  }

  code_results
}

#' @rdname day07
#' @export
eval_shiny_gold_bag_rule <- function(x, repetitions = TRUE) {
  e <- setup_bag_environment(x, repetitions)
  e$shiny_gold_bag()
}

#' @rdname day07
#' @export
setup_bag_environment <- function(x, repetitions = TRUE) {
  e <- new.env()

  # Create functions from each rule
  for (line in x) {
    next_function <- parse_bag_rule_to_function(line, repetitions)
    e[[next_function$f_name]] <- rlang::new_function(
      NULL,
      body = next_function$body,
      env = e
    )
  }

  # Create a base case rule
  e$no_other_bag <- rlang::new_function(NULL, body = character(), env = e)
  e
}

#' @rdname day07
#' @export
parse_bag_rule_to_function <- function(rule, repetitions = TRUE) {
  top_level <- rule %>%
    stringr::str_replace_all("bags", "bag") %>%
    stringr::str_split(" contain ") %>%
    unlist()

  this_color <- top_level[1] %>%
    stringr::str_remove(" bag")

  this_function <- top_level[1] %>%
    stringr::str_replace_all(" ", "_")

  call_lines <- top_level[2] %>%
    stringr::str_replace("no other bag", "1 no other bag") %>%
    stringr::str_remove_all("[.]") %>%
    stringr::str_split(", ") %>%
    unlist()

  reps <- call_lines %>%
    stringr::str_extract("^\\d+") %>%
    as.numeric()

  if (!repetitions) {
    reps <- rep(1, length(reps))
  }

  calls <- call_lines %>%
    rep(reps) %>%
    stringr::str_remove_all("\\d+ ") %>%
    stringr::str_replace_all(" ", "_")

  body <- expr(c(!! this_color, !!! lapply(calls, rlang::call2)))

  list(
    f_name = this_function,
    body = body,
    this_color = this_color
  )
}

#' @rdname day07
#' @export
example_bag_rules <- function() {
  c(
    "light red bags contain 1 bright white bag, 2 muted yellow bags.",
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
    "bright white bags contain 1 shiny gold bag.",
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
    "faded blue bags contain no other bags.",
    "dotted black bags contain no other bags."
  )
}
