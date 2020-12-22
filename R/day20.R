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
#' Now, you\'re ready to *check the image for sea monsters*.
#'
#' The borders of each tile are not part of the actual image; start by
#' removing them.
#'
#' In the example above, the tiles become:
#'
#'     .#.#..#. ##...#.# #..#####
#'     ###....# .#....#. .#......
#'     ##.##.## #.#.#..# #####...
#'     ###.#### #...#.## ###.#..#
#'     ##.#.... #.##.### #...#.##
#'     ...##### ###.#... .#####.#
#'     ....#..# ...##..# .#.###..
#'     .####... #..#.... .#......
#'
#'     #..#.##. .#..###. #.##....
#'     #.####.. #.####.# .#.###..
#'     ###.#.#. ..#.#### ##.#..##
#'     #.####.. ..##..## ######.#
#'     ##..##.# ...#...# .#.#.#..
#'     ...#..#. .#.#.##. .###.###
#'     .#.#.... #.##.#.. .###.##.
#'     ###.#... #..#.##. ######..
#'
#'     .#.#.### .##.##.# ..#.##..
#'     .####.## #.#...## #.#..#.#
#'     ..#.#..# ..#.#.#. ####.###
#'     #..####. ..#.#.#. ###.###.
#'     #####..# ####...# ##....##
#'     #.##..#. .#...#.. ####...#
#'     .#.###.. ##..##.. ####.##.
#'     ...###.. .##...#. ..#..###
#'
#' Remove the gaps to form the actual image:
#'
#'     .#.#..#.##...#.##..#####
#'     ###....#.#....#..#......
#'     ##.##.###.#.#..######...
#'     ###.#####...#.#####.#..#
#'     ##.#....#.##.####...#.##
#'     ...########.#....#####.#
#'     ....#..#...##..#.#.###..
#'     .####...#..#.....#......
#'     #..#.##..#..###.#.##....
#'     #.####..#.####.#.#.###..
#'     ###.#.#...#.######.#..##
#'     #.####....##..########.#
#'     ##..##.#...#...#.#.#.#..
#'     ...#..#..#.#.##..###.###
#'     .#.#....#.##.#...###.##.
#'     ###.#...#..#.##.######..
#'     .#.#.###.##.##.#..#.##..
#'     .####.###.#...###.#..#.#
#'     ..#.#..#..#.#.#.####.###
#'     #..####...#.#.#.###.###.
#'     #####..#####...###....##
#'     #.##..#..#...#..####...#
#'     .#.###..##..##..####.##.
#'     ...###...##...#...#..###
#'
#' Now, you\'re ready to search for sea monsters! Because your image is
#' monochrome, a sea monster will look like this:
#'
#'                       #
#'     #    ##    ##    ###
#'      #  #  #  #  #  #
#'
#' When looking for this pattern in the image, *the spaces can be
#' anything*; only the `#` need to match. Also, you might need to rotate or
#' flip your image before it\'s oriented correctly to find sea monsters. In
#' the above image, *after flipping and rotating it* to the appropriate
#' orientation, there are *two* sea monsters (marked with `O`):
#'
#'     .####...#####..#...###..
#'     #####..#..#.#.####..#.#.
#'     .#.#...#.###...#.##.O#..
#'     #.O.##.OO#.#.OO.##.OOO##
#'     ..#O.#O#.O##O..O.#O##.##
#'     ...#.#..##.##...#..#..##
#'     #.##.#..#.#..#..##.#.#..
#'     .###.##.....#...###.#...
#'     #.####.#.#....##.#..#.#.
#'     ##...#..#....#..#...####
#'     ..#.##...###..#.#####..#
#'     ....#.##.#.#####....#...
#'     ..##.##.###.....#.##..#.
#'     #...#...###..####....##.
#'     .#.##...#.##.#.#.###...#
#'     #.###.#..####...##..#...
#'     #.###...#.##...#.##O###.
#'     .O##.#OO.###OO##..OOO##.
#'     ..O#.O..O..O.#O##O##.###
#'     #.#..##.########..#..##.
#'     #.#####..#.#...##..#....
#'     #....##..#.#########..##
#'     #...#.....#..##...###.##
#'     #..###....##.#...##.##.#
#'
#' Determine how rough the waters are in the sea monsters\' habitat by
#' counting the number of `#` that are *not* part of a sea monster. In the
#' above example, the habitat\'s water roughness is *`273`*.
#'
#' *How many `#` are not part of a sea monster?*
#'
#' @param x some data
#' @return For Part One, `find_map_corners(x)` return the IDs of the map
#'   corners.... For Part Two, `f20b(x)` returns ....
#' @export
#' @examples
#' find_map_corners(example_map_tiles())
assemble_map_tiles <- function(x) {
  x <- example_map_tiles()
  tiles <- prepare_map_tiles(x)

  edge_counts <- count_compatible_map_tile_edges(x)
  edge_list <- edge_counts %>% lapply(function(x) names(x))

  corners <- find_map_corners(x) %>% as.character()

  first_corner <- corners[1]

  tiles[[first_corner]]

  edge_counts[[first_corner]]

  tile <- first_corner
  find_neighbors <- function(tile, edge_list, tile_list) {

    on_corner1 <- edge_list %>%
      keep_if(function(x) any(x %in% edge_list[[tile]]))
    on_corner1

    tile

  }
  edges[corners][[1]]
  edges[corners][[4]]



  tiles[names(on_corner1)]
}




#' @rdname day20
#' @export
find_map_corners <- function(x) {
  x %>%
    count_compatible_map_tile_edges() %>%
    lapply(table) %>%
    keep_if(function(x) x["2"] == 4) %>%
    names() %>%
    as.numeric()
}

count_compatible_map_tile_edges <- function(x) {
  # x <- example_map_tiles(1)
  x <- prepare_map_tiles(x)

  edges <- x %>%
    lapply(
      function(m) {
        tb <-   m[             , c(1, ncol(m))]
        lr <- t(m[c(1, nrow(m)),              ])
        cbind(tb, lr) %>%
          # reversed copies of the columns
          apply(2, paste0, collapse = "") %>%
          c(., stringi::stri_reverse(.))
      }
    )
  e <- edges %>% unlist(use.names = FALSE) %>% table() %>% unclass()

  #     1951    2311    3079
  #     2729    1427    2473
  #     2971    1489    1171
  edge_counts <- edges %>% lapply(function(x) e[x])

  # center all 2s
  # corners have four 2s
  # walls have three 2s

  edge_counts
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
    # lapply(unlist) %>%
    # lapply(matrix, ncol = nchar(x[2]), byrow = TRUE) %>%
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
