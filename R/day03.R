#' Day 03: Toboggan Trajectory
#'
#' [Toboggan Trajectory](https://adventofcode.com/2020/day/3)
#'
#' @name day03
#' @rdname day03
#' @details
#'
#' **Part One**
#'
#' With the toboggan login problems resolved, you set off toward the
#' airport. While travel by toboggan might be easy, it\'s certainly not
#' safe: there\'s [very minimal
#' steering]{title="It looks like the toboggan steering system even runs on Intcode! Good thing you don't have to modify it."}
#' and the area is covered in trees. You\'ll need to see which angles will
#' take you near the fewest trees.
#'
#' Due to the local geology, trees in this area only grow on exact integer
#' coordinates in a grid. You make a map (your puzzle input) of the open
#' squares (`.`) and trees (`#`) you can see. For example:
#'
#'     ..##.......
#'     #...#...#..
#'     .#....#..#.
#'     ..#.#...#.#
#'     .#...##..#.
#'     ..#.##.....
#'     .#.#.#....#
#'     .#........#
#'     #.##...#...
#'     #...##....#
#'     .#..#...#.#
#'
#' These aren\'t the only trees, though; due to something you read about
#' once involving arboreal genetics and biome stability, the same pattern
#' repeats to the right many times:
#'
#'     ..##.........##.........##.........##.........##.........##.......  --->
#'     #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
#'     .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
#'     ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
#'     .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
#'     ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
#'     .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
#'     .#........#.#........#.#........#.#........#.#........#.#........#
#'     #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
#'     #...##....##...##....##...##....##...##....##...##....##...##....#
#'     .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
#'
#' You start on the open square (`.`) in the top-left corner and need to
#' reach the bottom (below the bottom-most row on your map).
#'
#' The toboggan can only follow a few specific slopes (you opted for a
#' cheaper model that prefers rational numbers); start by *counting all the
#' trees* you would encounter for the slope *right 3, down 1*:
#'
#' From your starting position at the top-left, check the position that is
#' right 3 and down 1. Then, check the position that is right 3 and down 1
#' from there, and so on until you go past the bottom of the map.
#'
#' The locations you\'d check in the above example are marked here with `O`
#' where there was an open square and `X` where there was a tree:
#'
#'     ..##.........##.........##.........##.........##.........##.......  --->
#'     #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
#'     .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
#'     ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
#'     .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
#'     ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
#'     .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
#'     .#........#.#........X.#........#.#........#.#........#.#........#
#'     #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
#'     #...##....##...##....##...#X....##...##....##...##....##...##....#
#'     .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
#'
#' In this example, traversing the map using this slope would cause you to
#' encounter `7` trees.
#'
#' Starting at the top-left corner of your map and following a slope of
#' right 3 and down 1, *how many trees would you encounter?*
#'
#' **Part Two**
#'
#' Time to check the rest of the slopes - you need to minimize the
#' probability of a sudden arboreal stop, after all.
#'
#' Determine the number of trees you would encounter if, for each of the
#' following slopes, you start at the top-left corner and traverse the map
#' all the way to the bottom:
#'
#' -   Right 1, down 1.
#' -   Right 3, down 1. (This is the slope you already checked.)
#' -   Right 5, down 1.
#' -   Right 7, down 1.
#' -   Right 1, down 2.
#'
#' In the above example, these slopes would find `2`, `7`, `3`, `4`, and
#' `2` tree(s) respectively; multiplied together, these produce the answer
#' `336`.
#'
#' *What do you get if you multiply together the number of trees
#' encountered on each of the listed slopes?*
#'
#' @param x some data
#' @param move_x x position moved per step. Defaults to 3.
#' @param move_y y position moved per step. Defaults to 1.
#' @return For Part One, `count_trees_visited(x)` returns the number of trees
#'   visited by the sled. For Part Two, `f03b(x)` returns ....
#' @export
#' @examples
#' x <- read_text_lines(
#'   "
#'   ..##.......
#'   #...#...#..
#'   .#....#..#.
#'   ..#.#...#.#
#'   .#...##..#.
#'   ..#.##.....
#'   .#.#.#....#
#'   .#........#
#'   #.##...#...
#'   #...##....#
#'   .#..#...#.#
#'   "
#' )
#' count_trees_visited(x, move_x = 3, move_y = 1)
#' count_trees_visited(x, move_x = 1, move_y = 2)
count_trees_visited <- function(x, move_x = 3, move_y = 1) {
  # Find which x positions are visited
  width <- nchar(x[1])
  height <- length(x)
  x_spots <- cumsum(c(1, rep(move_x, height - 1)))
  x_spots_wrapped <- x_spots %% width
  x_spots_wrapped[x_spots_wrapped == 0] <- width

  # Find x positions of trees in each row visited
  y_spots <- cumsum(c(1, rep(move_y, height - 1)))
  tree_spots <- which_spots_are_trees(x[y_spots])

  tree_count <- x_spots_wrapped %>%
    as.list() %>%
    lapply2(tree_spots, `%in%`) %>%
    unlist() %>%
    sum()

  tree_count
}

which_spots_are_trees <- function(x) {
  x %>%
    strsplit("") %>%
    lapply(function(x) which(x == "#"))
}
