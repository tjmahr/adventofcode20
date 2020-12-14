#' Day 13: Shuttle Search
#'
#' [Shuttle Search](https://adventofcode.com/2020/day/13)
#'
#' @name day13
#' @rdname day13
#' @details
#'
#' **Part One**
#'
#' Your ferry can make it safely to a nearby port, but it won\'t get much
#' further. When you call to book another ship, you discover that no ships
#' embark from that port to your vacation island. You\'ll need to get from
#' the port to the nearest airport.
#'
#' Fortunately, a shuttle bus service is available to bring you from the
#' sea port to the airport! Each bus has an ID number that also indicates
#' *how often the bus leaves for the airport*.
#'
#' Bus schedules are defined based on a *timestamp* that measures the
#' *number of minutes* since some fixed reference point in the past. At
#' timestamp `0`, every bus simultaneously departed from the sea port.
#' After that, each bus travels to the airport, then various other
#' locations, and finally returns to the sea port to repeat its journey
#' forever.
#'
#' The time this loop takes a particular bus is also its ID number: the bus
#' with ID `5` departs from the sea port at timestamps `0`, `5`, `10`,
#' `15`, and so on. The bus with ID `11` departs at `0`, `11`, `22`, `33`,
#' and so on. If you are there when the bus departs, you can ride that bus
#' to the airport!
#'
#' Your notes (your puzzle input) consist of two lines. The first line is
#' your estimate of the *earliest timestamp you could depart on a bus*. The
#' second line lists the bus IDs that are in service according to the
#' shuttle company; entries that show `x` must be out of service, so you
#' decide to ignore them.
#'
#' To save time once you arrive, your goal is to figure out *the earliest
#' bus you can take to the airport*. (There will be exactly one such bus.)
#'
#' For example, suppose you have the following notes:
#'
#'     939
#'     7,13,x,x,59,x,31,19
#'
#' Here, the earliest timestamp you could depart is `939`, and the bus IDs
#' in service are `7`, `13`, `59`, `31`, and `19`. Near timestamp `939`,
#' these bus IDs depart at the times marked `D`:
#'
#'     time   bus 7   bus 13  bus 59  bus 31  bus 19
#'     929      .       .       .       .       .
#'     930      .       .       .       D       .
#'     931      D       .       .       .       D
#'     932      .       .       .       .       .
#'     933      .       .       .       .       .
#'     934      .       .       .       .       .
#'     935      .       .       .       .       .
#'     936      .       D       .       .       .
#'     937      .       .       .       .       .
#'     938      D       .       .       .       .
#'     939      .       .       .       .       .
#'     940      .       .       .       .       .
#'     941      .       .       .       .       .
#'     942      .       .       .       .       .
#'     943      .       .       .       .       .
#'     944      .       .       D       .       .
#'     945      D       .       .       .       .
#'     946      .       .       .       .       .
#'     947      .       .       .       .       .
#'     948      .       .       .       .       .
#'     949      .       D       .       .       .
#'
#' The earliest bus you could take is bus ID `59`. It doesn\'t depart until
#' timestamp `944`, so you would need to wait `944 - 939 = 5` minutes
#' before it departs. Multiplying the bus ID by the number of minutes
#' you\'d need to wait gives *`295`*.
#'
#' *What is the ID of the earliest bus you can take to the airport
#' multiplied by the number of minutes you\'ll need to wait for that bus?*
#'
#' **Part Two**
#'
#' The shuttle company is running a
#' [contest]{title="This is why you should never let me design a contest for a shuttle company."}:
#' one gold coin for anyone that can find the earliest timestamp such that
#' the first bus ID departs at that time and each subsequent listed bus ID
#' departs at that subsequent minute. (The first line in your input is no
#' longer relevant.)
#'
#' For example, suppose you have the same list of bus IDs as above:
#'
#'     7,13,x,x,59,x,31,19
#'
#' An `x` in the schedule means there are no constraints on what bus IDs
#' must depart at that time.
#'
#' This means you are looking for the earliest timestamp (called `t`) such
#' that:
#'
#' -   Bus ID `7` departs at timestamp `t`.
#' -   Bus ID `13` departs one minute after timestamp `t`.
#' -   There are no requirements or restrictions on departures at two or
#'     three minutes after timestamp `t`.
#' -   Bus ID `59` departs four minutes after timestamp `t`.
#' -   There are no requirements or restrictions on departures at five
#'     minutes after timestamp `t`.
#' -   Bus ID `31` departs six minutes after timestamp `t`.
#' -   Bus ID `19` departs seven minutes after timestamp `t`.
#'
#' The only bus departures that matter are the listed bus IDs at their
#' specific offsets from `t`. Those bus IDs can depart at other times, and
#' other bus IDs can depart at those times. For example, in the list above,
#' because bus ID `19` must depart seven minutes after the timestamp at
#' which bus ID `7` departs, bus ID `7` will always *also* be departing
#' with bus ID `19` at seven minutes after timestamp `t`.
#'
#' In this example, the earliest timestamp at which this occurs is
#' *`1068781`*:
#'
#'     time     bus 7   bus 13  bus 59  bus 31  bus 19
#'     1068773    .       .       .       .       .
#'     1068774    D       .       .       .       .
#'     1068775    .       .       .       .       .
#'     1068776    .       .       .       .       .
#'     1068777    .       .       .       .       .
#'     1068778    .       .       .       .       .
#'     1068779    .       .       .       .       .
#'     1068780    .       .       .       .       .
#'     1068781    D       .       .       .       .
#'     1068782    .       D       .       .       .
#'     1068783    .       .       .       .       .
#'     1068784    .       .       .       .       .
#'     1068785    .       .       D       .       .
#'     1068786    .       .       .       .       .
#'     1068787    .       .       .       D       .
#'     1068788    D       .       .       .       D
#'     1068789    .       .       .       .       .
#'     1068790    .       .       .       .       .
#'     1068791    .       .       .       .       .
#'     1068792    .       .       .       .       .
#'     1068793    .       .       .       .       .
#'     1068794    .       .       .       .       .
#'     1068795    D       D       .       .       .
#'     1068796    .       .       .       .       .
#'     1068797    .       .       .       .       .
#'
#' In the above example, bus ID `7` departs at timestamp `1068788` (seven
#' minutes after `t`). This is fine; the only requirement on that minute is
#' that bus ID `19` departs then, and it does.
#'
#' Here are some other examples:
#'
#' -   The earliest timestamp that matches the list `17,x,13,19` is
#'     *`3417`*.
#' -   `67,7,59,61` first occurs at timestamp *`754018`*.
#' -   `67,x,7,59,61` first occurs at timestamp *`779210`*.
#' -   `67,7,x,59,61` first occurs at timestamp *`1261476`*.
#' -   `1789,37,47,1889` first occurs at timestamp *`1202161486`*.
#'
#' However, with so many bus IDs in your list, surely the actual earliest
#' timestamp will be larger than `100000000000000`!
#'
#' *What is the earliest timestamp such that all of the listed bus IDs
#' depart at offsets matching their positions in the list?*
#'
#' @param x 2-element character vector of bus notes.
#' @return For Part One, `estimate_earliest_bus(x)` returns the timestamp of the
#'   earliest bus. For Part Two, `estimate_earliest_shared_bus_time(x)` returns
#'   the time satisfying the constraints in the bus notes.
#' @export
#' @examples
#' estimate_earliest_bus(example_bus_notes())
#' estimate_earliest_shared_bus_time(example_bus_notes())
estimate_earliest_bus <- function(x) {
  notes <- prepare_bus_notes(x)
  # How long each bus has been going at the initial timestamp
  running_time <- notes$time %% notes$ids_known
  remaining_time <- notes$ids_known - running_time
  which_first <- which.min(remaining_time)
  c(
    id = notes$ids_known[which_first],
    wait = remaining_time[which_first]
  )
}

#' @rdname day13
#' @export
estimate_earliest_shared_bus_time <- function(x) {
  # We are solving a system congruences with coprime numbers.
  # We use the sieve algorithm
  # https://en.wikipedia.org/wiki/Chinese_remainder_theorem

  # Solve a pair in the sieving algorithm
  # Finds first x that is congruent to a1 mod n1 and a2 mod n2
  solve_sieve_pair <- function(a1, n1, a2, n2) {
    multiplier <- -1
    done <- FALSE

    while (!done) {
      multiplier <- multiplier + 1
      new_a <- (a1 + (multiplier * n1))
      done <- new_a %% n2 == a2
    }
    new_n <- n1 * n2
    c(new_a, new_n)
  }

  x <- prepare_bus_notes(x)

  # Each step is a minute of the starting time, so these are remainders
  remainders <- x$which_known - 1

  # What to avoid case where remainder is greater than divisor
  remainders <- remainders %% x$ids_known

  # Run sieve on largest numbers first
  ns <- x$ids_known %>% sort() %>% rev()
  as <- remainders[order(x$ids_known, decreasing = TRUE)]

  # Set up first pass
  a1 <- as[1]
  n1 <- ns[1]
  as <- as[-1]
  ns <- ns[-1]
  for (i in seq_along(ns)) {
    result <- solve_sieve_pair(a1, n1, as[i], ns[i])
    a1 <- result[1]
    n1 <- result[2]
  }
  result[2] - result[1]
}


prepare_bus_notes <- function(x) {
  notes <- list()
  notes$time <- as.numeric(x[1])
  ids <- unlist(strsplit(x[2], ","))

  notes$ids_raw <- ids
  notes$ids_seq <- seq_along(ids)
  notes$which_blank <- which(ids == "x")
  notes$which_known <- which(ids != "x")
  notes$ids_known <- as.numeric(ids[notes$which_known])

  notes
}


#' @rdname day13
#' @export
example_bus_notes <- function(example = 1) {
  l <- list(
    a1 = c("939", "7,13,x,x,59,x,31,19"),
    b0 = c("1068781", "7,13,x,x,59,x,31,19"),
    b1 = c("3417", "17,x,13,19"),
    b2 = c("754018", "67,7,59,61"),
    b3 = c("779210", "67,x,7,59,61"),
    b4 = c("1261476", "67,7,x,59,61"),
    b5 = c("1202161486", "1789,37,47,1889")
  )
  l[[example]]
}
