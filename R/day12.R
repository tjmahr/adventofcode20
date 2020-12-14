#' Day 12: Rain Risk
#'
#' [Rain Risk](https://adventofcode.com/2020/day/12)
#'
#' @name day12
#' @rdname day12
#' @details
#'
#' **Part One**
#'
#' [faster than anyone
#' expected]{title="At least it wasn't a Category Six!"}. The ferry needs
#' to take *evasive actions*!
#'
#' Unfortunately, the ship\'s navigation computer seems to be
#' malfunctioning; rather than giving a route directly to safety, it
#' produced extremely circuitous instructions. When the captain uses the
#' [PA system](https://en.wikipedia.org/wiki/Public_address_system) to ask
#' if anyone can help, you quickly volunteer.
#'
#' The navigation instructions (your puzzle input) consists of a sequence
#' of single-character *actions* paired with integer input *values*. After
#' staring at them for a few minutes, you work out what they probably mean:
#'
#' -   Action *`N`* means to move *north* by the given value.
#' -   Action *`S`* means to move *south* by the given value.
#' -   Action *`E`* means to move *east* by the given value.
#' -   Action *`W`* means to move *west* by the given value.
#' -   Action *`L`* means to turn *left* the given number of degrees.
#' -   Action *`R`* means to turn *right* the given number of degrees.
#' -   Action *`F`* means to move *forward* by the given value in the
#'     direction the ship is currently facing.
#'
#' The ship starts by facing *east*. Only the `L` and `R` actions change
#' the direction the ship is facing. (That is, if the ship is facing east
#' and the next instruction is `N10`, the ship would move north 10 units,
#' but would still move east if the following action were `F`.)
#'
#' For example:
#'
#'     F10
#'     N3
#'     F7
#'     R90
#'     F11
#'
#' These instructions would be handled as follows:
#'
#' -   `F10` would move the ship 10 units east (because the ship starts by
#'     facing east) to *east 10, north 0*.
#' -   `N3` would move the ship 3 units north to *east 10, north 3*.
#' -   `F7` would move the ship another 7 units east (because the ship is
#'     still facing east) to *east 17, north 3*.
#' -   `R90` would cause the ship to turn right by 90 degrees and face
#'     *south*; it remains at *east 17, north 3*.
#' -   `F11` would move the ship 11 units south to *east 17, south 8*.
#'
#' At the end of these instructions, the ship\'s [Manhattan
#' distance](https://en.wikipedia.org/wiki/Manhattan_distance) (sum of the
#' absolute values of its east/west position and its north/south position)
#' from its starting position is `17 + 8` = *`25`*.
#'
#' Figure out where the navigation instructions lead. *What is the
#' Manhattan distance between that location and the ship\'s starting
#' position?*
#'
#' **Part Two**
#'
#' Before you can give the destination to the captain, you realize that the
#' actual action meanings were printed on the back of the instructions the
#' whole time.
#'
#' Almost all of the actions indicate how to move a *waypoint* which is
#' relative to the ship's position:
#'
#' -   Action *`N`* means to move the waypoint *north* by the given value.
#' -   Action *`S`* means to move the waypoint *south* by the given value.
#' -   Action *`E`* means to move the waypoint *east* by the given value.
#' -   Action *`W`* means to move the waypoint *west* by the given value.
#' -   Action *`L`* means to rotate the waypoint around the ship *left*
#'     (*counter-clockwise*) the given number of degrees.
#' -   Action *`R`* means to rotate the waypoint around the ship *right*
#'     (*clockwise*) the given number of degrees.
#' -   Action *`F`* means to move *forward* to the waypoint a number of
#'     times equal to the given value.
#'
#' The waypoint starts *10 units east and 1 unit north* relative to the
#' ship. The waypoint is relative to the ship; that is, if the ship moves,
#' the waypoint moves with it.
#'
#' For example, using the same instructions as above:
#'
#' -   `F10` moves the ship to the waypoint 10 times (a total of *100 units
#'     east and 10 units north*), leaving the ship at *east 100, north 10*.
#'     The waypoint stays 10 units east and 1 unit north of the ship.
#' -   `N3` moves the waypoint 3 units north to *10 units east and 4 units
#'     north of the ship*. The ship remains at *east 100, north 10*.
#' -   `F7` moves the ship to the waypoint 7 times (a total of *70 units
#'     east and 28 units north*), leaving the ship at *east 170, north 38*.
#'     The waypoint stays 10 units east and 4 units north of the ship.
#' -   `R90` rotates the waypoint around the ship clockwise 90 degrees,
#'     moving it to *4 units east and 10 units south of the ship*. The ship
#'     remains at *east 170, north 38*.
#' -   `F11` moves the ship to the waypoint 11 times (a total of *44 units
#'     east and 110 units south*), leaving the ship at *east 214, south
#'     72*. The waypoint stays 4 units east and 10 units south of the ship.
#'
#' After these operations, the ship\'s Manhattan distance from its starting
#' position is `214 + 72` = *`286`*.
#'
#' Figure out where the navigation instructions actually lead. *What is the
#' Manhattan distance between that location and the ship\'s starting
#' position?*
#'
#' @param x ship instructions
#' @return For Part One, `follow_ship_instructions(x)` returns the east/west
#'   position, the north/south position and the angle of the ship (divided by
#'   90). For Part Two, `follow_waypoint_instructions(x)` returns the waypoint
#'   position followed by the ship position.
#' @export
#' @examples
#' follow_ship_instructions(example_ship_instructions())
#' follow_waypoint_instructions(example_ship_instructions())
follow_ship_instructions <- function(x) {
  actions <- substr(x, 1, 1)
  values <- as.numeric(substr(x, 2, nchar(x)))

  # Use positive multiples of 90
  values <- ifelse(actions %in% c("L", "R"), (values / 90) %% 4, values)

  angle_to_dir <- function(angle) {
    c("E", "N", "W", "S")[angle + 1]
  }

  perform_ship_action <- function(action, value, x, y, angle) {
    if (action == "F") action <- angle_to_dir(angle)
    result <- switch(
      action,
      # purdy spacing
      `E` = c( value,      0,      0),
      `W` = c(-value,      0,      0),
      `N` = c(     0,  value,      0),
      `S` = c(     0, -value,      0),
      `L` = c(     0,      0,  value),
      `R` = c(     0,      0, -value)
    )
    updated <- c(x, y, angle) + result
    updated[3] <- updated[3] %% 4
    updated
  }

  perform_all_ship_actions <- function(actions, values, x, y, angle) {
    if (length(actions) == 0) {
      final_result <- c(x, y, angle)
    } else {
      result <- perform_ship_action(actions[1], values[1], x, y, angle)
      actions <- actions[-1]
      values <- values[-1]
      final_result <- Recall(actions, values, result[1], result[2], result[3])
    }
    final_result
  }

  perform_all_ship_actions(actions, values, 0, 0, 0)
}


#' @rdname day12
#' @export
follow_waypoint_instructions <- function(x) {
  actions <- substr(x, 1, 1)
  values <- as.numeric(substr(x, 2, nchar(x)))

  # Use positive multiples of 90
  values <- ifelse(actions %in% c("L", "R"), (values / 90) %% 4, values)

  # Use the rotation matrix
  # https://en.wikipedia.org/wiki/Rotation_matrix
  rotate_waypoint <- function(waypoint, angle) {
    a <- angle * pi / 2
    m <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), ncol = 2)
    as.vector(m %*% waypoint)
  }

  perform_action <- function(action, value, waypoint, ship) {
    if (action == "F") ship <- ship + waypoint * value
    waypoint <- switch(
      action,
      `E` = waypoint + c(value, 0),
      `W` = waypoint - c(value, 0),
      `N` = waypoint + c(0, value),
      `S` = waypoint - c(0, value),
      `L` = rotate_waypoint(waypoint,  value),
      `R` = rotate_waypoint(waypoint, -value),
      waypoint
    )
    c(waypoint, ship)
  }

  perform_all_ship_actions <- function(actions, values, waypoint, ship) {
    if (length(actions) == 0) {
      final_result <- c(waypoint, ship)
    } else {
      result <- perform_action(actions[1], values[1], waypoint, ship)
      actions <- actions[-1]
      values <- values[-1]
      final_result <- Recall(actions, values, result[1:2], result[3:4])
    }
    final_result
  }

  ship <- c(0, 0)
  waypoint <- c(10, 1)
  perform_all_ship_actions(actions, values, waypoint, ship)
}


#' @rdname day12
#' @export
example_ship_instructions <- function() {
  c(
    "F10",
    "N3",
    "F7",
    "R90",
    "F11"
  )
}
