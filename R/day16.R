#' Day 16: Ticket Translation
#'
#' [Ticket Translation](https://adventofcode.com/2020/day/16)
#'
#' @name day16
#' @rdname day16
#' @details
#'
#' **Part One**
#'
#' one of the legs of your re-routed trip coming up is on a high-speed
#' train. However, the train ticket you were given is in a language you
#' don\'t understand. You should probably figure out what it says before
#' you get to the train station after the next flight.
#'
#' Unfortunately, you [can\'t actually *read* the words on the
#' ticket]{title="This actually happened to me once, but I solved it by just asking someone."}.
#' You can, however, read the numbers, and so you figure out *the fields
#' these tickets must have* and *the valid ranges* for values in those
#' fields.
#'
#' You collect the *rules for ticket fields*, the *numbers on your ticket*,
#' and the *numbers on other nearby tickets* for the same train service
#' (via the airport security cameras) together into a single document you
#' can reference (your puzzle input).
#'
#' The *rules for ticket fields* specify a list of fields that exist
#' *somewhere* on the ticket and the *valid ranges of values* for each
#' field. For example, a rule like `class: 1-3 or 5-7` means that one of
#' the fields in every ticket is named `class` and can be any value in the
#' ranges `1-3` or `5-7` (inclusive, such that `3` and `5` are both valid
#' in this field, but `4` is not).
#'
#' Each ticket is represented by a single line of comma-separated values.
#' The values are the numbers on the ticket in the order they appear; every
#' ticket has the same format. For example, consider this ticket:
#'
#'     .--------------------------------------------------------.
#'     | ????: 101    ?????: 102   ??????????: 103     ???: 104 |
#'     |                                                        |
#'     | ??: 301  ??: 302             ???????: 303      ??????? |
#'     | ??: 401  ??: 402           ???? ????: 403    ????????? |
#'     '--------------------------------------------------------'
#'
#' Here, `?` represents text in a language you don\'t understand. This
#' ticket might be represented as
#' `101,102,103,104,301,302,303,401,402,403`; of course, the actual train
#' tickets you\'re looking at are *much* more complicated. In any case,
#' you\'ve extracted just the numbers in such a way that the first number
#' is always the same specific field, the second number is always a
#' different specific field, and so on - you just don\'t know what each
#' position actually means!
#'
#' Start by determining which tickets are *completely invalid*; these are
#' tickets that contain values which *aren\'t valid for any field*. Ignore
#' *your ticket* for now.
#'
#' For example, suppose you have the following notes:
#'
#'     class: 1-3 or 5-7
#'     row: 6-11 or 33-44
#'     seat: 13-40 or 45-50
#'
#'     your ticket:
#'     7,1,14
#'
#'     nearby tickets:
#'     7,3,47
#'     40,4,50
#'     55,2,20
#'     38,6,12
#'
#' It doesn\'t matter which position corresponds to which field; you can
#' identify invalid *nearby tickets* by considering only whether tickets
#' contain *values that are not valid for any field*. In this example, the
#' values on the first *nearby ticket* are all valid for at least one
#' field. This is not true of the other three *nearby tickets*: the values
#' `4`, `55`, and `12` are are not valid for any field. Adding together all
#' of the invalid values produces your *ticket scanning error rate*:
#' `4 + 55 + 12` = *`71`*.
#'
#' Consider the validity of the *nearby tickets* you scanned. *What is your
#' ticket scanning error rate?*
#'
#' **Part Two**
#'
#' Now that you've identified which tickets contain invalid values,
#' *discard those tickets entirely*. Use the remaining valid tickets to
#' determine which field is which.
#'
#' Using the valid ranges for each field, determine what order the fields
#' appear on the tickets. The order is consistent between all tickets: if
#' `seat` is the third field, it is the third field on every ticket,
#' including *your ticket*.
#'
#' For example, suppose you have the following notes:
#'
#'     class: 0-1 or 4-19
#'     row: 0-5 or 8-19
#'     seat: 0-13 or 16-19
#'
#'     your ticket:
#'     11,12,13
#'
#'     nearby tickets:
#'     3,9,18
#'     15,1,5
#'     5,14,9
#'
#' Based on the *nearby tickets* in the above example, the first position
#' must be `row`, the second position must be `class`, and the third
#' position must be `seat`; you can conclude that in *your ticket*, `class`
#' is `12`, `row` is `11`, and `seat` is `13`.
#'
#' Once you work out which field is which, look for the six fields on *your
#' ticket* that start with the word `departure`. *What do you get if you
#' multiply those six values together?*
#'
#' @param x Character vector with the train data.
#' @return For Part One, `find_invalid_train_ticket_values(x)` returns a list
#'   with the problem data which includes a field for the numbers from invalid
#'   tickets and list of tickets with the invalid ones removed. For Part Two,
#'   `solve_train_ticket_fields(x)` solves the train tickets, returning a list
#'   with a dataframe of the ticket field constraints and a dataframe of train
#'   tickets with the solvable fields set as column names. The first row of the
#'   tickets dataframe is the player's ticket.
#' @export
#' @examples
#' find_invalid_train_ticket_values(example_train_tickets())
#' solve_train_ticket_fields(example_train_tickets(2))
find_invalid_train_ticket_values <- function(x) {
  data <- setup_train_data(x)

  in_range <- function(x, l, u) l <= x & x <= u
  in_any_range <- function(x, ls, us) any(in_range(x, ls, us))

  ls <- c(data$rules$x1, data$rules$y1)
  us <- c(data$rules$x2, data$rules$y2)

  invalid_numbers <- data$nearby_tickets %>%
    lapply(
      keep_if, function(x) !in_any_range(x, ls, us)
    )

  # Knock out invalid tickets
  data$nearby_tickets <- data$nearby_tickets[lengths(invalid_numbers) == 0]
  data$invalid_numbers <- unlist(invalid_numbers)
  data
}


#' @rdname day16
#' @export
solve_train_ticket_fields <- function(x) {
  all_in_range_pair_v <- function(xs, v) {
    all((v[1] <= xs & xs <= v[2]) | (v[3] <= xs & xs <= v[4]))
  }

  # Set the initial values for the candidate names.
  set_candidate_names <- function(rules, tickets) {
    unmatched_names <- tickets %>% names() %>% stringr::str_subset("V\\d+")
    rules$candidates <- seq_along(rules$field) %>%
      as.list() %>%
      lapply(
        function(x) unmatched_names
      )
    rules
  }

  # Find which columns of tickets satisfying the given ranges. This is meant to
  # applied to a single row of rules.
  check_candidates <- function(rule, tickets) {
    candidates <- rule$candidates[[1]]
    v <- unlist(rule[1, c("x1", "x2", "y1", "y2")])
    satisfying_cols <- tickets[, candidates, drop = FALSE] %>%
      keep_if(function(x) all_in_range_pair_v(x, v)) %>%
      names()
    rule[["candidates"]][[1]] <- satisfying_cols
    rule
  }

  # For each rule with one candidate, rename the tickets column to the field
  # name. Then remove that candidate from consideration. Repeat.
  apply_candidates <- function(rules, tickets) {
    resolved <- which(lengths(rules$candidates) == 1)
    while (length(resolved) >= 1) {
      for (rule_i in resolved) {
        # Update tickets columns
        this_key <- rules$field[[rule_i]]
        this_candidate <- rules$candidates[[rule_i]]
        which_name_to_change <- which(names(tickets) == this_candidate)
        names(tickets)[which_name_to_change] <- this_key
        # Update candidates
        rules$candidates <- rules$candidates %>%
          lapply(function(x) x[x != this_candidate])
        resolved <- which(lengths(rules$candidates) == 1)
      }
    }
    list(rules = rules, tickets = tickets)
  }

  data <- find_invalid_train_ticket_values(x)
  rules <- data$rules

  # Combine all the tickets (rowwise) into a dataframe
  tickets <- c(list(data$my_ticket), data$nearby_tickets) %>%
    lapply(t) %>%
    invoke_call(rbind) %>%
    as.data.frame()

  # Find first set of candidate columns for each field
  rules <- rules %>%
    set_candidate_names(tickets) %>%
    split(seq_len(nrow(.))) %>%
    lapply(check_candidates, tickets) %>%
    invoke_call(rbind)

  apply_candidates(rules, tickets)
}


setup_train_data <- function(x) {
  sections <- group_at_empty_lines(x)

  rules <- sections[[1]] %>%
    stringr::str_match("([A-z ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)") %>%
    as.data.frame() %>%
    stats::setNames(c("line", "field", "x1", "x2", "y1", "y2")) %>%
    utils::type.convert(as.is = TRUE)

  nearby <- sections[[3]][-1] %>%
    lapply(strsplit, ",") %>%
    lapply(unlist) %>%
    lapply(as.numeric)

  mine <- sections[[2]][-1] %>%
    lapply(strsplit, ",") %>%
    lapply(unlist) %>%
    lapply(as.numeric) %>%
    unlist()

  list(
    rules = rules,
    my_ticket = mine,
    nearby_tickets = nearby
  )
}

#' @param example which example data to use. Defaults to 1.
#' @rdname day16
#' @export
example_train_tickets <- function(example = 1) {
  l <- list(
    a1 = c(
      "class: 1-3 or 5-7",
      "row: 6-11 or 33-44",
      "seat: 13-40 or 45-50",
      "",
      "your ticket:",
      "7,1,14",
      "",
      "nearby tickets:",
      "7,3,47",
      "40,4,50",
      "55,2,20",
      "38,6,12"
    ),
    b1 = c(
      "class: 0-1 or 4-19",
      "row: 0-5 or 8-19",
      "seat: 0-13 or 16-19",
      "",
      "your ticket:",
      "11,12,13",
      "",
      "nearby tickets:",
      "3,9,18",
      "15,1,5",
      "5,14,9"
    )
  )
  l[[example]]
}
