#' Day 24: Lobby Layout
#'
#' [Lobby Layout](https://adventofcode.com/2020/day/24)
#'
#' @name day24
#' @rdname day24
#' @details
#'
#' **Part One**
#'
#' Your raft makes it to the tropical island; it turns out that the small
#' crab was an excellent navigator. You make your way to the resort.
#'
#' As you enter the lobby, you discover a small problem: the floor is being
#' renovated. You can\'t even reach the check-in desk until they\'ve
#' finished installing the *new tile floor*.
#'
#' The tiles are all *hexagonal*; they need to be arranged in a [hex
#' grid](https://en.wikipedia.org/wiki/Hexagonal_tiling) with a very
#' specific color pattern. Not in the mood to wait, you offer to help
#' figure out the pattern.
#'
#' The tiles are all *white* on one side and *black* on the other. They
#' start with the white side facing up. The lobby is large enough to fit
#' whatever pattern might need to appear there.
#'
#' A member of the renovation crew gives you a *list of the tiles that need
#' to be flipped over* (your puzzle input). Each line in the list
#' identifies a single tile that needs to be flipped by giving a series of
#' steps starting from a *reference tile* in the very center of the room.
#' (Every line starts from the same reference tile.)
#'
#' Because the tiles are hexagonal, every tile has *six neighbors*: east,
#' southeast, southwest, west, northwest, and northeast. These directions
#' are given in your list, respectively, as `e`, `se`, `sw`, `w`, `nw`, and
#' `ne`. A tile is identified by a series of these directions with *no
#' delimiters*; for example, `esenee` identifies the tile you land on if
#' you start at the reference tile and then move one tile east, one tile
#' southeast, one tile northeast, and one tile east.
#'
#' Each time a tile is identified, it flips from white to black or from
#' black to white. Tiles might be flipped more than once. For example, a
#' line like `esew` flips a tile immediately adjacent to the reference
#' tile, and a line like `nwwswee` flips the reference tile itself.
#'
#' Here is a larger example:
#'
#'     sesenwnenenewseeswwswswwnenewsewsw
#'     neeenesenwnwwswnenewnwwsewnenwseswesw
#'     seswneswswsenwwnwse
#'     nwnwneseeswswnenewneswwnewseswneseene
#'     swweswneswnenwsewnwneneseenw
#'     eesenwseswswnenwswnwnwsewwnwsene
#'     sewnenenenesenwsewnenwwwse
#'     wenwwweseeeweswwwnwwe
#'     wsweesenenewnwwnwsenewsenwwsesesenwne
#'     neeswseenwwswnwswswnw
#'     nenwswwsewswnenenewsenwsenwnesesenew
#'     enewnwewneswsewnwswenweswnenwsenwsw
#'     sweneswneswneneenwnewenewwneswswnese
#'     swwesenesewenwneswnwwneseswwne
#'     enesenwswwswneneswsenwnewswseenwsese
#'     wnwnesenesenenwwnenwsewesewsesesew
#'     nenewswnwewswnenesenwnesewesw
#'     eneswnwswnwsenenwnwnwwseeswneewsenese
#'     neswnwewnwnwseenwseesewsenwsweewe
#'     wseweeenwnesenwwwswnew
#'
#' In the above example, 10 tiles are flipped once (to black), and 5 more
#' are flipped twice (to black, then back to white). After all of these
#' instructions have been followed, a total of *`10`* tiles are *black*.
#'
#' Go through the renovation crew\'s list and determine which tiles they
#' need to flip. After all of the instructions have been followed, *how
#' many tiles are left with the black side up?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `flip_hex_tiles(x)` returns the number of once visited
#'   and twice visited tiles. For Part Two, `f24b(x)` returns ....
#' @export
#' @examples
#' flip_hex_tiles(example_hex_tiles())
#' f24b()
flip_hex_tiles <- function(x) {
  last_steps <- x %>%
    walk_hex_tiles() %>%
    split(.$step) %>%
    lapply(tail, 1) %>%
    invoke_call(rbind)

  counts <- last_steps[c("x", "y")] %>% table()

  list(
    black = sum(counts == 1),
    white = sum(counts == 2)
  )
}


#' @rdname day24
#' @export
f24b <- function(x) {

}


# Add up distances accumulated by a series of hexagon steps
convert_hex_steps_to_distances <- function(steps) {
  mapping <- matrix(
    c(
       1.0,  0.0,
      -1.0,  0.0,
       0.5,  0.5,
      -0.5,  0.5,
       0.5, -0.5,
      -0.5, -0.5
    ),
    ncol = 2,
    byrow = TRUE
  )

  colnames(mapping) <- c("x", "y")
  rownames(mapping) <- c("e", "w", "ne", "nw", "se", "sw")
  mapping[steps, , drop = FALSE] %>%
    apply(2, cumsum)
}


walk_hex_tiles <- function(x) {
  parse_hex_directions <- function(x) {
    x %>%
      stringr::str_replace_all("(e|w)", "\\1,") %>%
      stringr::str_remove_all(",$") %>%
      stringr::str_split(",")
  }

  d <- parse_hex_directions(x)

  d %>%
    lapply(convert_hex_steps_to_distances) %>%
    lapply2(., x, function(x, y) {
      d <- as.data.frame(x)
      d$step <- y
      d
    }) %>%
    invoke_call(rbind)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day24
#' @export
example_hex_tiles <- function(example = 1) {
  l <- list(
    a = c(
      "sesenwnenenewseeswwswswwnenewsewsw",
      "neeenesenwnwwswnenewnwwsewnenwseswesw",
      "seswneswswsenwwnwse",
      "nwnwneseeswswnenewneswwnewseswneseene",
      "swweswneswnenwsewnwneneseenw",
      "eesenwseswswnenwswnwnwsewwnwsene",
      "sewnenenenesenwsewnenwwwse",
      "wenwwweseeeweswwwnwwe",
      "wsweesenenewnwwnwsenewsenwwsesesenwne",
      "neeswseenwwswnwswswnw",
      "nenwswwsewswnenenewsenwsenwnesesenew",
      "enewnwewneswsewnwswenweswnenwsenwsw",
      "sweneswneswneneenwnewenewwneswswnese",
      "swwesenesewenwneswnwwneseswwne",
      "enesenwswwswneneswsenwnewswseenwsese",
      "wnwnesenesenenwwnenwsewesewsesesew",
      "nenewswnwewswnenesenwnesewesw",
      "eneswnwswnwsenenwnwnwwseeswneewsenese",
      "neswnwewnwnwseenwseesewsenwsweewe",
      "wseweeenwnesenwwwswnew"
    )
  )
  l[[example]]
}



