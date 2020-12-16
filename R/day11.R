#' Day 11: Seating System
#'
#' [Seating System](https://adventofcode.com/2020/day/11)
#'
#' @name day11
#' @rdname day11
#' @details
#'
#' **Part One**
#'
#' Your plane lands with plenty of time to spare. The final leg of your
#' journey is a ferry that goes directly to the tropical island where you
#' can finally start your vacation. As you reach the waiting area to board
#' the ferry, you realize you're so early, nobody else has even arrived
#' yet!
#'
#' By modeling the process people use to choose (or abandon) their seat in
#' the waiting area, you're pretty sure you can predict the best place to
#' sit. You make a quick map of the seat layout (your puzzle input).
#'
#' The seat layout fits neatly on a grid. Each position is either floor
#' (`.`), an empty seat (`L`), or an occupied seat (`#`). For example, the
#' initial seat layout might look like this:
#'
#'     L.LL.LL.LL
#'     LLLLLLL.LL
#'     L.L.L..L..
#'     LLLL.LL.LL
#'     L.LL.LL.LL
#'     L.LLLLL.LL
#'     ..L.L.....
#'     LLLLLLLLLL
#'     L.LLLLLL.L
#'     L.LLLLL.LL
#'
#' Now, you just need to model the people who will be arriving shortly.
#' Fortunately, people are entirely predictable and always follow a simple
#' set of rules. All decisions are based on the *number of occupied seats*
#' adjacent to a given seat (one of the eight positions immediately up,
#' down, left, right, or diagonal from the seat). The following rules are
#' applied to every seat simultaneously:
#'
#' -   If a seat is *empty* (`L`) and there are *no* occupied seats
#'     adjacent to it, the seat becomes *occupied*.
#' -   If a seat is *occupied* (`#`) and *four or more* seats adjacent to
#'     it are also occupied, the seat becomes *empty*.
#' -   Otherwise, the seat\'s state does not change.
#'
#' [Floor (`.`) never changes]{title="Floor... floor never changes."};
#' seats don\'t move, and nobody sits on the floor.
#'
#' After one round of these rules, every seat in the example layout becomes
#' occupied:
#'
#'     #.##.##.##
#'     #######.##
#'     #.#.#..#..
#'     ####.##.##
#'     #.##.##.##
#'     #.#####.##
#'     ..#.#.....
#'     ##########
#'     #.######.#
#'     #.#####.##
#'
#' After a second round, the seats with four or more occupied adjacent
#' seats become empty again:
#'
#'     #.LL.L#.##
#'     #LLLLLL.L#
#'     L.L.L..L..
#'     #LLL.LL.L#
#'     #.LL.LL.LL
#'     #.LLLL#.##
#'     ..L.L.....
#'     #LLLLLLLL#
#'     #.LLLLLL.L
#'     #.#LLLL.##
#'
#' This process continues for three more rounds:
#'
#'     #.##.L#.##
#'     #L###LL.L#
#'     L.#.#..#..
#'     #L##.##.L#
#'     #.##.LL.LL
#'     #.###L#.##
#'     ..#.#.....
#'     #L######L#
#'     #.LL###L.L
#'     #.#L###.##
#'
#'     #.#L.L#.##
#'     #LLL#LL.L#
#'     L.L.L..#..
#'     #LLL.##.L#
#'     #.LL.LL.LL
#'     #.LL#L#.##
#'     ..L.L.....
#'     #L#LLLL#L#
#'     #.LLLLLL.L
#'     #.#L#L#.##
#'
#'     #.#L.L#.##
#'     #LLL#LL.L#
#'     L.#.L..#..
#'     #L##.##.L#
#'     #.#L.LL.LL
#'     #.#L#L#.##
#'     ..L.L.....
#'     #L#L##L#L#
#'     #.LLLLLL.L
#'     #.#L#L#.##
#'
#' At this point, something interesting happens: the chaos stabilizes and
#' further applications of these rules cause no seats to change state! Once
#' people stop moving around, you count *`37`* occupied seats.
#'
#' Simulate your seating area by applying the seating rules repeatedly
#' until no seats change state. *How many seats end up occupied?*
#'
#' **Part Two**
#'
#' As soon as people start to arrive, you realize your mistake. People
#' don't just care about adjacent seats - they care about *the first seat
#' they can see* in each of those eight directions!
#'
#' Now, instead of considering just the eight immediately adjacent seats,
#' consider the *first seat* in each of those eight directions. For
#' example, the empty seat below would see *eight* occupied seats:
#'
#'     .......#.
#'     ...#.....
#'     .#.......
#'     .........
#'     ..#L....#
#'     ....#....
#'     .........
#'     #........
#'     ...#.....
#'
#' The leftmost empty seat below would only see *one* empty seat, but
#' cannot see any of the occupied ones:
#'
#'     .............
#'     .L.L.#.#.#.#.
#'     .............
#'
#' The empty seat below would see *no* occupied seats:
#'
#'     .##.##.
#'     #.#.#.#
#'     ##...##
#'     ...L...
#'     ##...##
#'     #.#.#.#
#'     .##.##.
#'
#' Also, people seem to be more tolerant than you expected: it now takes
#' *five or more* visible occupied seats for an occupied seat to become
#' empty (rather than *four or more* from the previous rules). The other
#' rules still apply: empty seats that see no occupied seats become
#' occupied, seats matching no rule don\'t change, and floor never changes.
#'
#' Given the same starting layout as above, these new rules cause the
#' seating area to shift around as follows:
#'
#'     L.LL.LL.LL
#'     LLLLLLL.LL
#'     L.L.L..L..
#'     LLLL.LL.LL
#'     L.LL.LL.LL
#'     L.LLLLL.LL
#'     ..L.L.....
#'     LLLLLLLLLL
#'     L.LLLLLL.L
#'     L.LLLLL.LL
#'
#'     #.##.##.##
#'     #######.##
#'     #.#.#..#..
#'     ####.##.##
#'     #.##.##.##
#'     #.#####.##
#'     ..#.#.....
#'     ##########
#'     #.######.#
#'     #.#####.##
#'
#'     #.LL.LL.L#
#'     #LLLLLL.LL
#'     L.L.L..L..
#'     LLLL.LL.LL
#'     L.LL.LL.LL
#'     L.LLLLL.LL
#'     ..L.L.....
#'     LLLLLLLLL#
#'     #.LLLLLL.L
#'     #.LLLLL.L#
#'
#'     #.L#.##.L#
#'     #L#####.LL
#'     L.#.#..#..
#'     ##L#.##.##
#'     #.##.#L.##
#'     #.#####.#L
#'     ..#.#.....
#'     LLL####LL#
#'     #.L#####.L
#'     #.L####.L#
#'
#'     #.L#.L#.L#
#'     #LLLLLL.LL
#'     L.L.L..#..
#'     ##LL.LL.L#
#'     L.LL.LL.L#
#'     #.LLLLL.LL
#'     ..L.L.....
#'     LLLLLLLLL#
#'     #.LLLLL#.L
#'     #.L#LL#.L#
#'
#'     #.L#.L#.L#
#'     #LLLLLL.LL
#'     L.L.L..#..
#'     ##L#.#L.L#
#'     L.L#.#L.L#
#'     #.L####.LL
#'     ..#.#.....
#'     LLL###LLL#
#'     #.LLLLL#.L
#'     #.L#LL#.L#
#'
#'     #.L#.L#.L#
#'     #LLLLLL.LL
#'     L.L.L..#..
#'     ##L#.#L.L#
#'     L.L#.LL.L#
#'     #.LLLL#.LL
#'     ..#.L.....
#'     LLL###LLL#
#'     #.LLLLL#.L
#'     #.L#LL#.L#
#'
#' Again, at this point, people stop shifting around and the seating area
#' reaches equilibrium. Once this occurs, you count *`26`* occupied seats.
#'
#' Given the new visibility method and the rule change for occupied seats
#' becoming empty, once equilibrium is reached, *how many seats end up
#' occupied?*
#'
#' @param x Character vector of ferry seatings. One string per row.
#' @param m Character matrix representing the seats.
#' @param i,j Integers indicating the seat row and column, respectively.
#' @param neighbor_def Function for finding neighboring seats in the ferry.
#' @param coords Dataframe containing supplemental dataframe about each seat.
#'   This argument is used to speed up operations by precomputing neighbors.
#' @param threshold Integer setting the threshold for the number of neighboring
#'   sees. Defaults to 4.
#' @return `get_adjacent_ferry_seats()` and `get_visible_ferry_seats()` find the
#'   neighbors of a seat. `update_ferry_seat_matrix()` runs a single step of the
#'   ferry seating process. `simulate_ferry_seating(x)` repeated updates the
#'   ferry seats until they converge.
#' @export
#' @examples
#' x <- example_ferry_seats(1)
#' simulate_ferry_seating(x)
simulate_ferry_seating <- function(
  x,
  neighbor_def = get_adjacent_ferry_seats,
  threshold = 4
) {
  m_last <- create_ferry_seat_matrix(x)

  # I originally computed coords on each call of get_new_ferry_seat_status() and
  # the adjacent seats whenever they were checked, but the code seemed really
  # slow. We can compute these only once. Now I put these data in coords and
  # pass it along.
  coords <- create_ferry_data(m_last, neighbor_def = neighbor_def)
  m_new <- update_ferry_seat_matrix(m_last, coords, neighbor_def, threshold)

  while (any(m_last != m_new)) {
    m_last <- m_new
    m_new <- update_ferry_seat_matrix(m_last, coords, neighbor_def, threshold)
  }
  m_new
}


# This was stubborn puzzle to refactor. I wish I had just done a dataframe
# solution and did joins to get neighbor values and split-apply-combine to count
# occupancy.


# Try to precompute the grids and neighbors to save time
create_ferry_data <- function(m, neighbor_def = get_adjacent_ferry_seats) {
  rows <- seq_len(nrow(m))
  cols <- seq_len(ncol(m))
  coords <- expand.grid(row = rows, col = cols)

  coords[["neighbors"]] <- coords[["row"]] %>%
    lapply2(
      coords[["col"]],
      function(x, y) neighbor_def(m, x, y)
    )

  coords
}


#' @rdname day11
#' @export
update_ferry_seat_matrix <- function(
  m,
  coords = NULL,
  neighbor_def = get_adjacent_ferry_seats,
  threshold = 4
) {
  if (is.null(coords)) {
    coords <- create_ferry_data(m, neighbor_def)
  }

  coords[["row"]] %>%
    lapply3(
      coords[["col"]],
      coords[["neighbors"]],
      get_new_ferry_seat_status,
      m = list(m),
      threshold = threshold
    ) %>%
    unlist() %>%
    matrix(nrow = nrow(m), ncol = ncol(m))
}


create_ferry_seat_matrix <- function(x) {
  x %>%
    strsplit("") %>%
    unlist() %>%
    matrix(nrow = length(x), byrow = TRUE)
}


get_new_ferry_seat_status <- function(
  i,
  j,
  neighbors = NULL,
  m,
  threshold = 4
) {
  # "Otherwise, the seat's state does not change."
  current_status <- m[i, j]
  new_status <- current_status
  seats <- m[neighbors]

  # "If a seat is *empty* (`L`) and there are *no* occupied seats
  # adjacent to it, the seat becomes *occupied*."
  if (current_status == "L") {
    new_status <- ifelse(all(seats %in% c(".", "L")), "#", "L")
  }

  # "If a seat is *occupied* (`#`) and *four or more* seats adjacent to
  # it are also occupied, the seat becomes *empty*."
  if (current_status == "#") {
    new_status <- ifelse(sum(seats == "#") < threshold, "#", "L")
  }

  new_status
}


#' @rdname day11
#' @export
get_adjacent_ferry_seats <- function(m, i, j) {
  truncate_range <- function(xs, lower, upper) {
    xs[lower <= xs & xs <= upper]
  }
  x_span <- truncate_range(j + -1:1, 1, ncol(m))
  y_span <- truncate_range(i + -1:1, 1, nrow(m))

  # Create index matrix. See ?`[` under Matrices and arrays. Don't select self.
  g <- expand.grid(row = y_span, col = x_span)
  g <- g[g$row != i | g$col != j, 1:2]
  g <- as.matrix(g)

  g
}


#' @rdname day11
#' @export
get_visible_ferry_seats <- function(m, i, j) {
  # recursively walk along a direction
  observe_ferry_seat_direction <- function(xdir, ydir, m = m, i, j) {
    yx <- c(i + ydir, j + xdir)
    out_of_bounds <- any(yx == 0) | yx[2] > ncol(m) | yx[1] > nrow(m)
    if (out_of_bounds) {
      NULL
    } else if (m[yx[1], yx[2]] != ".") {
      matrix(yx, nrow = 1)
    } else {
      Recall(xdir, ydir, m, yx[1], yx[2])
    }
  }

  views <- list(
    observe_ferry_seat_direction (-1, -1, m, i, j),
    observe_ferry_seat_direction ( 0, -1, m, i, j),
    observe_ferry_seat_direction ( 1, -1, m, i, j),
    observe_ferry_seat_direction (-1,  0, m, i, j),
    # (don't do 0, 0)
    observe_ferry_seat_direction ( 1,  0, m, i, j),
    observe_ferry_seat_direction (-1,  1, m, i, j),
    observe_ferry_seat_direction ( 0,  1, m, i, j),
    observe_ferry_seat_direction ( 1,  1, m, i, j)
  )
  views <- do.call(rbind, views)
  if (!is.null(views)) {
    colnames(views) <- c("row", "col")
    rownames(views) <- seq_len(nrow(views))
  }
  views
}


#' @param example Which example data to use. Defaults to 1.
#' @rdname day11
#' @export
example_ferry_seats <- function(example = 1) {
  l <- list(
    m1 = c(
      "L.LL.LL.LL",
      "LLLLLLL.LL",
      "L.L.L..L..",
      "LLLL.LL.LL",
      "L.LL.LL.LL",
      "L.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLLL",
      "L.LLLLLL.L",
      "L.LLLLL.LL"
    ),
    m2 = c(
      "#.##.##.##",
      "#######.##",
      "#.#.#..#..",
      "####.##.##",
      "#.##.##.##",
      "#.#####.##",
      "..#.#.....",
      "##########",
      "#.######.#",
      "#.#####.##"
    ),
    m3 = c(
      "#.LL.L#.##",
      "#LLLLLL.L#",
      "L.L.L..L..",
      "#LLL.LL.L#",
      "#.LL.LL.LL",
      "#.LLLL#.##",
      "..L.L.....",
      "#LLLLLLLL#",
      "#.LLLLLL.L",
      "#.#LLLL.##"
    ),
    m4 = c(
      "#.##.L#.##",
      "#L###LL.L#",
      "L.#.#..#..",
      "#L##.##.L#",
      "#.##.LL.LL",
      "#.###L#.##",
      "..#.#.....",
      "#L######L#",
      "#.LL###L.L",
      "#.#L###.##"
    ),
    m5 = c(
      "#.#L.L#.##",
      "#LLL#LL.L#",
      "L.L.L..#..",
      "#LLL.##.L#",
      "#.LL.LL.LL",
      "#.LL#L#.##",
      "..L.L.....",
      "#L#LLLL#L#",
      "#.LLLLLL.L",
      "#.#L#L#.##"
    ),
    m6 = c(
      "#.#L.L#.##",
      "#LLL#LL.L#",
      "L.#.L..#..",
      "#L##.##.L#",
      "#.#L.LL.LL",
      "#.#L#L#.##",
      "..L.L.....",
      "#L#L##L#L#",
      "#.LLLLLL.L",
      "#.#L#L#.##"
    ),
    b1 = c(
      ".......#.",
      "...#.....",
      ".#.......",
      ".........",
      "..#L....#",
      "....#....",
      ".........",
      "#........",
      "...#....."
    ),
    b2 = c(
      ".............",
      ".L.L.#.#.#.#.",
      "............."
    ),
    b3 = c(
      ".##.##.",
      "#.#.#.#",
      "##...##",
      "...L...",
      "##...##",
      "#.#.#.#",
      ".##.##."
    ),

    c1 = c(
      "L.LL.LL.LL",
      "LLLLLLL.LL",
      "L.L.L..L..",
      "LLLL.LL.LL",
      "L.LL.LL.LL",
      "L.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLLL",
      "L.LLLLLL.L",
      "L.LLLLL.LL"
    ),
    c2 = c(
      "#.##.##.##",
      "#######.##",
      "#.#.#..#..",
      "####.##.##",
      "#.##.##.##",
      "#.#####.##",
      "..#.#.....",
      "##########",
      "#.######.#",
      "#.#####.##"
    ),
    c3 = c(
      "#.LL.LL.L#",
      "#LLLLLL.LL",
      "L.L.L..L..",
      "LLLL.LL.LL",
      "L.LL.LL.LL",
      "L.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLL#",
      "#.LLLLLL.L",
      "#.LLLLL.L#"
    ),
    c4 = c(
      "#.L#.##.L#",
      "#L#####.LL",
      "L.#.#..#..",
      "##L#.##.##",
      "#.##.#L.##",
      "#.#####.#L",
      "..#.#.....",
      "LLL####LL#",
      "#.L#####.L",
      "#.L####.L#"
    ),
    c5 = c(
      "#.L#.L#.L#",
      "#LLLLLL.LL",
      "L.L.L..#..",
      "##LL.LL.L#",
      "L.LL.LL.L#",
      "#.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLL#",
      "#.LLLLL#.L",
      "#.L#LL#.L#"
    ),
    c6 = c(
      "#.L#.L#.L#",
      "#LLLLLL.LL",
      "L.L.L..#..",
      "##L#.#L.L#",
      "L.L#.#L.L#",
      "#.L####.LL",
      "..#.#.....",
      "LLL###LLL#",
      "#.LLLLL#.L",
      "#.L#LL#.L#"
    ),
    c7 = c(
      "#.L#.L#.L#",
      "#LLLLLL.LL",
      "L.L.L..#..",
      "##L#.#L.L#",
      "L.L#.LL.L#",
      "#.LLLL#.LL",
      "..#.L.....",
      "LLL###LLL#",
      "#.LLLLL#.L",
      "#.L#LL#.L#"
    )
  )
  l[[example]]
}
