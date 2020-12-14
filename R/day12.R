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
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x ship instructions
#' @return For Part One, `follow_ship_instructions(x)` returns the east/west
#'   position, the north/south position and the angle of the ship (divided by
#'   90). For Part Two, `f12b(x)` returns ....
#' @export
#' @examples
#' follow_ship_instructions(example_ship_instructions())
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


f12b <- function(x) {

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
