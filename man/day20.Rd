% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/day20.R
\name{day20}
\alias{day20}
\alias{assemble_map_tiles}
\alias{find_map_corners}
\alias{example_map_tiles}
\title{Day 20: Jurassic Jigsaw}
\usage{
assemble_map_tiles(x)

find_map_corners(x)

example_map_tiles(example = 1)
}
\arguments{
\item{x}{some data}

\item{example}{Which example data to use (by position or name). Defaults to
1.}
}
\value{
For Part One, \code{find_map_corners(x)} return the IDs of the map
corners.... For Part Two, \code{f20b(x)} returns ....
}
\description{
\href{https://adventofcode.com/2020/day/20}{Jurassic Jigsaw}
}
\details{
\strong{Part One}

The high-speed train leaves the forest and quickly carries you south.
You can even see a desert in the distance! Since you have some spare
time, you [might as
well]{title="Just in case. Maybe they missed something."} see if there
was anything interesting in the image the Mythical Information Bureau
satellite captured.

After decoding the satellite messages, you discover that the data
actually contains many small images created by the satellite\'s \emph{camera
array}. The camera array consists of many cameras; rather than produce a
single square image, they produce many smaller square image \emph{tiles} that
need to be \emph{reassembled back into a single image}.

Each camera in the camera array returns a single monochrome \emph{image tile}
with a random unique \emph{ID number}. The tiles (your puzzle input) arrived
in a random order.

Worse yet, the camera array appears to be malfunctioning: each image
tile has been \emph{rotated and flipped to a random orientation}. Your first
task is to reassemble the original image by orienting the tiles so they
fit together.

To show how the tiles should be reassembled, each tile\'s image data
includes a border that should line up exactly with its adjacent tiles.
All tiles have this border, and the border lines up exactly when the
tiles are both oriented correctly. Tiles at the edge of the image also
have this border, but the outermost edges won\'t line up with any other
tiles.

For example, suppose you have the following nine tiles:\preformatted{Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
}

By rotating, flipping, and rearranging them, you can find a square
arrangement that causes all adjacent borders to line up:\preformatted{#...##.#.. ..###..### #.#.#####.
..#.#..#.# ###...#.#. .#..######
.###....#. ..#....#.. ..#.......
###.##.##. .#.#.#..## ######....
.###.##### ##...#.### ####.#..#.
.##.#....# ##.##.###. .#...#.##.
#...###### ####.#...# #.#####.##
.....#..## #...##..#. ..#.###...
#.####...# ##..#..... ..#.......
#.##...##. ..##.#..#. ..#.###...

#.##...##. ..##.#..#. ..#.###...
##..#.##.. ..#..###.# ##.##....#
##.####... .#.####.#. ..#.###..#
####.#.#.. ...#.##### ###.#..###
.#.####... ...##..##. .######.##
.##..##.#. ....#...## #.#.#.#...
....#..#.# #.#.#.##.# #.###.###.
..#.#..... .#.##.#..# #.###.##..
####.#.... .#..#.##.. .######...
...#.#.#.# ###.##.#.. .##...####

...#.#.#.# ###.##.#.. .##...####
..#.#.###. ..##.##.## #..#.##..#
..####.### ##.#...##. .#.#..#.##
#..#.#..#. ...#.#.#.. .####.###.
.#..####.# #..#.#.#.# ####.###..
.#####..## #####...#. .##....##.
##.##..#.. ..#...#... .####...#.
#.#.###... .##..##... .####.##.#
#...###... ..##...#.. ...#..####
..#.#....# ##.#.#.... ...##.....
}

For reference, the IDs of the above tiles are:\preformatted{1951    2311    3079
2729    1427    2473
2971    1489    1171
}

To check that you\'ve assembled the image correctly, multiply the IDs of
the four corner tiles together. If you do this with the assembled tiles
from the example above, you get \code{1951 * 3079 * 2971 * 1171} =
\emph{\code{20899048083289}}.

Assemble the tiles into an image. \emph{What do you get if you multiply
together the IDs of the four corner tiles?}

\strong{Part Two}

Now, you\'re ready to \emph{check the image for sea monsters}.

The borders of each tile are not part of the actual image; start by
removing them.

In the example above, the tiles become:\preformatted{.#.#..#. ##...#.# #..#####
###....# .#....#. .#......
##.##.## #.#.#..# #####...
###.#### #...#.## ###.#..#
##.#.... #.##.### #...#.##
...##### ###.#... .#####.#
....#..# ...##..# .#.###..
.####... #..#.... .#......

#..#.##. .#..###. #.##....
#.####.. #.####.# .#.###..
###.#.#. ..#.#### ##.#..##
#.####.. ..##..## ######.#
##..##.# ...#...# .#.#.#..
...#..#. .#.#.##. .###.###
.#.#.... #.##.#.. .###.##.
###.#... #..#.##. ######..

.#.#.### .##.##.# ..#.##..
.####.## #.#...## #.#..#.#
..#.#..# ..#.#.#. ####.###
#..####. ..#.#.#. ###.###.
#####..# ####...# ##....##
#.##..#. .#...#.. ####...#
.#.###.. ##..##.. ####.##.
...###.. .##...#. ..#..###
}

Remove the gaps to form the actual image:\preformatted{.#.#..#.##...#.##..#####
###....#.#....#..#......
##.##.###.#.#..######...
###.#####...#.#####.#..#
##.#....#.##.####...#.##
...########.#....#####.#
....#..#...##..#.#.###..
.####...#..#.....#......
#..#.##..#..###.#.##....
#.####..#.####.#.#.###..
###.#.#...#.######.#..##
#.####....##..########.#
##..##.#...#...#.#.#.#..
...#..#..#.#.##..###.###
.#.#....#.##.#...###.##.
###.#...#..#.##.######..
.#.#.###.##.##.#..#.##..
.####.###.#...###.#..#.#
..#.#..#..#.#.#.####.###
#..####...#.#.#.###.###.
#####..#####...###....##
#.##..#..#...#..####...#
.#.###..##..##..####.##.
...###...##...#...#..###
}

Now, you\'re ready to search for sea monsters! Because your image is
monochrome, a sea monster will look like this:\preformatted{                  #
#    ##    ##    ###
 #  #  #  #  #  #
}

When looking for this pattern in the image, \emph{the spaces can be
anything}; only the \verb{#} need to match. Also, you might need to rotate or
flip your image before it\'s oriented correctly to find sea monsters. In
the above image, \emph{after flipping and rotating it} to the appropriate
orientation, there are \emph{two} sea monsters (marked with \code{O}):\preformatted{.####...#####..#...###..
#####..#..#.#.####..#.#.
.#.#...#.###...#.##.O#..
#.O.##.OO#.#.OO.##.OOO##
..#O.#O#.O##O..O.#O##.##
...#.#..##.##...#..#..##
#.##.#..#.#..#..##.#.#..
.###.##.....#...###.#...
#.####.#.#....##.#..#.#.
##...#..#....#..#...####
..#.##...###..#.#####..#
....#.##.#.#####....#...
..##.##.###.....#.##..#.
#...#...###..####....##.
.#.##...#.##.#.#.###...#
#.###.#..####...##..#...
#.###...#.##...#.##O###.
.O##.#OO.###OO##..OOO##.
..O#.O..O..O.#O##O##.###
#.#..##.########..#..##.
#.#####..#.#...##..#....
#....##..#.#########..##
#...#.....#..##...###.##
#..###....##.#...##.##.#
}

Determine how rough the waters are in the sea monsters\' habitat by
counting the number of \verb{#} that are \emph{not} part of a sea monster. In the
above example, the habitat\'s water roughness is \emph{\code{273}}.

\emph{How many \verb{#} are not part of a sea monster?}
}
\examples{
find_map_corners(example_map_tiles())
}
