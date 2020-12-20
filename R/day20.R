#' Day 20: Jurassic Jigsaw
#'
#' [Jurassic Jigsaw](https://adventofcode.com/2020/day/20)
#'
#' @name day20
#' @rdname day20
#' @details
#'
#' **Part One**
#'
#' The high-speed train leaves the forest and quickly carries you south.
#' You can even see a desert in the distance! Since you have some spare
#' time, you [might as
#' well]{title="Just in case. Maybe they missed something."} see if there
#' was anything interesting in the image the Mythical Information Bureau
#' satellite captured.
#'
#' After decoding the satellite messages, you discover that the data
#' actually contains many small images created by the satellite\'s *camera
#' array*. The camera array consists of many cameras; rather than produce a
#' single square image, they produce many smaller square image *tiles* that
#' need to be *reassembled back into a single image*.
#'
#' Each camera in the camera array returns a single monochrome *image tile*
#' with a random unique *ID number*. The tiles (your puzzle input) arrived
#' in a random order.
#'
#' Worse yet, the camera array appears to be malfunctioning: each image
#' tile has been *rotated and flipped to a random orientation*. Your first
#' task is to reassemble the original image by orienting the tiles so they
#' fit together.
#'
#' To show how the tiles should be reassembled, each tile\'s image data
#' includes a border that should line up exactly with its adjacent tiles.
#' All tiles have this border, and the border lines up exactly when the
#' tiles are both oriented correctly. Tiles at the edge of the image also
#' have this border, but the outermost edges won\'t line up with any other
#' tiles.
#'
#' For example, suppose you have the following nine tiles:
#'
#'     Tile 2311:
#'     ..##.#..#.
#'     ##..#.....
#'     #...##..#.
#'     ####.#...#
#'     ##.##.###.
#'     ##...#.###
#'     .#.#.#..##
#'     ..#....#..
#'     ###...#.#.
#'     ..###..###
#'
#'     Tile 1951:
#'     #.##...##.
#'     #.####...#
#'     .....#..##
#'     #...######
#'     .##.#....#
#'     .###.#####
#'     ###.##.##.
#'     .###....#.
#'     ..#.#..#.#
#'     #...##.#..
#'
#'     Tile 1171:
#'     ####...##.
#'     #..##.#..#
#'     ##.#..#.#.
#'     .###.####.
#'     ..###.####
#'     .##....##.
#'     .#...####.
#'     #.##.####.
#'     ####..#...
#'     .....##...
#'
#'     Tile 1427:
#'     ###.##.#..
#'     .#..#.##..
#'     .#.##.#..#
#'     #.#.#.##.#
#'     ....#...##
#'     ...##..##.
#'     ...#.#####
#'     .#.####.#.
#'     ..#..###.#
#'     ..##.#..#.
#'
#'     Tile 1489:
#'     ##.#.#....
#'     ..##...#..
#'     .##..##...
#'     ..#...#...
#'     #####...#.
#'     #..#.#.#.#
#'     ...#.#.#..
#'     ##.#...##.
#'     ..##.##.##
#'     ###.##.#..
#'
#'     Tile 2473:
#'     #....####.
#'     #..#.##...
#'     #.##..#...
#'     ######.#.#
#'     .#...#.#.#
#'     .#########
#'     .###.#..#.
#'     ########.#
#'     ##...##.#.
#'     ..###.#.#.
#'
#'     Tile 2971:
#'     ..#.#....#
#'     #...###...
#'     #.#.###...
#'     ##.##..#..
#'     .#####..##
#'     .#..####.#
#'     #..#.#..#.
#'     ..####.###
#'     ..#.#.###.
#'     ...#.#.#.#
#'
#'     Tile 2729:
#'     ...#.#.#.#
#'     ####.#....
#'     ..#.#.....
#'     ....#..#.#
#'     .##..##.#.
#'     .#.####...
#'     ####.#.#..
#'     ##.####...
#'     ##..#.##..
#'     #.##...##.
#'
#'     Tile 3079:
#'     #.#.#####.
#'     .#..######
#'     ..#.......
#'     ######....
#'     ####.#..#.
#'     .#...#.##.
#'     #.#####.##
#'     ..#.###...
#'     ..#.......
#'     ..#.###...
#'
#' By rotating, flipping, and rearranging them, you can find a square
#' arrangement that causes all adjacent borders to line up:
#'
#'     #...##.#.. ..###..### #.#.#####.
#'     ..#.#..#.# ###...#.#. .#..######
#'     .###....#. ..#....#.. ..#.......
#'     ###.##.##. .#.#.#..## ######....
#'     .###.##### ##...#.### ####.#..#.
#'     .##.#....# ##.##.###. .#...#.##.
#'     #...###### ####.#...# #.#####.##
#'     .....#..## #...##..#. ..#.###...
#'     #.####...# ##..#..... ..#.......
#'     #.##...##. ..##.#..#. ..#.###...
#'
#'     #.##...##. ..##.#..#. ..#.###...
#'     ##..#.##.. ..#..###.# ##.##....#
#'     ##.####... .#.####.#. ..#.###..#
#'     ####.#.#.. ...#.##### ###.#..###
#'     .#.####... ...##..##. .######.##
#'     .##..##.#. ....#...## #.#.#.#...
#'     ....#..#.# #.#.#.##.# #.###.###.
#'     ..#.#..... .#.##.#..# #.###.##..
#'     ####.#.... .#..#.##.. .######...
#'     ...#.#.#.# ###.##.#.. .##...####
#'
#'     ...#.#.#.# ###.##.#.. .##...####
#'     ..#.#.###. ..##.##.## #..#.##..#
#'     ..####.### ##.#...##. .#.#..#.##
#'     #..#.#..#. ...#.#.#.. .####.###.
#'     .#..####.# #..#.#.#.# ####.###..
#'     .#####..## #####...#. .##....##.
#'     ##.##..#.. ..#...#... .####...#.
#'     #.#.###... .##..##... .####.##.#
#'     #...###... ..##...#.. ...#..####
#'     ..#.#....# ##.#.#.... ...##.....
#'
#' For reference, the IDs of the above tiles are:
#'
#'     1951    2311    3079
#'     2729    1427    2473
#'     2971    1489    1171
#'
#' To check that you\'ve assembled the image correctly, multiply the IDs of
#' the four corner tiles together. If you do this with the assembled tiles
#' from the example above, you get `1951 * 3079 * 2971 * 1171` =
#' *`20899048083289`*.
#'
#' Assemble the tiles into an image. *What do you get if you multiply
#' together the IDs of the four corner tiles?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f20a(x)` returns .... For Part Two,
#'   `f20b(x)` returns ....
#' @export
#' @examples
#' f20a(example_data_20())
#'
f20a <- function(x) {

}



f20b <- function(x) {
  x <- example_map_tiles(1)
  x <- prepare_map_tiles(x)

  m <- x[[1]]
  long_edges <- x %>%
    lapply(
      function(m) {
        m[, c(1, ncol(m))] %>%
          # reversed copies of the columns
          cbind(m[rev(seq_len(nrow(m))), c(1, ncol(m))]) %>%
          apply(2, paste0, collapse = "")
      }
    )
  long_edges
}


prepare_map_tiles <- function(x) {
  # x <- example_map_tiles(1)
  # x <- readLines("inst/input20.txt")
  # Make sure last tile ends with a blank
  x <- if (x[length(x)] != "") c(x, "") else x

  tile_headers <- which(startsWith(x, "Tile"))
  numbers <- x[tile_headers] %>% stringr::str_extract("\\d+")
  first_lines <- tile_headers + 1
  last_lines <- which(x == "") - 1

  matrices <- first_lines %>%
    lapply2(last_lines, seq) %>%
    lapply(function(l) x[l]) %>%
    lapply(strsplit, "") %>%
    lapply(function(x) do.call(rbind, x)) %>%
    stats::setNames(numbers)

  matrices
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day20
#' @export
example_map_tiles <- function(example = 1) {
  l <- list(
    a = c(
      "Tile 2311:",
      "..##.#..#.",
      "##..#.....",
      "#...##..#.",
      "####.#...#",
      "##.##.###.",
      "##...#.###",
      ".#.#.#..##",
      "..#....#..",
      "###...#.#.",
      "..###..###",
      "",
      "Tile 1951:",
      "#.##...##.",
      "#.####...#",
      ".....#..##",
      "#...######",
      ".##.#....#",
      ".###.#####",
      "###.##.##.",
      ".###....#.",
      "..#.#..#.#",
      "#...##.#..",
      "",
      "Tile 1171:",
      "####...##.",
      "#..##.#..#",
      "##.#..#.#.",
      ".###.####.",
      "..###.####",
      ".##....##.",
      ".#...####.",
      "#.##.####.",
      "####..#...",
      ".....##...",
      "",
      "Tile 1427:",
      "###.##.#..",
      ".#..#.##..",
      ".#.##.#..#",
      "#.#.#.##.#",
      "....#...##",
      "...##..##.",
      "...#.#####",
      ".#.####.#.",
      "..#..###.#",
      "..##.#..#.",
      "",
      "Tile 1489:",
      "##.#.#....",
      "..##...#..",
      ".##..##...",
      "..#...#...",
      "#####...#.",
      "#..#.#.#.#",
      "...#.#.#..",
      "##.#...##.",
      "..##.##.##",
      "###.##.#..",
      "",
      "Tile 2473:",
      "#....####.",
      "#..#.##...",
      "#.##..#...",
      "######.#.#",
      ".#...#.#.#",
      ".#########",
      ".###.#..#.",
      "########.#",
      "##...##.#.",
      "..###.#.#.",
      "",
      "Tile 2971:",
      "..#.#....#",
      "#...###...",
      "#.#.###...",
      "##.##..#..",
      ".#####..##",
      ".#..####.#",
      "#..#.#..#.",
      "..####.###",
      "..#.#.###.",
      "...#.#.#.#",
      "",
      "Tile 2729:",
      "...#.#.#.#",
      "####.#....",
      "..#.#.....",
      "....#..#.#",
      ".##..##.#.",
      ".#.####...",
      "####.#.#..",
      "##.####...",
      "##..#.##..",
      "#.##...##.",
      "",
      "Tile 3079:",
      "#.#.#####.",
      ".#..######",
      "..#.......",
      "######....",
      "####.#..#.",
      ".#...#.##.",
      "#.#####.##",
      "..#.###...",
      "..#.......",
      "..#.###..."
    )
  )
  l[[example]]
}

