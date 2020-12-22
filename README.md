
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adventofcode20

<!-- badges: start -->
<!-- badges: end -->

These are my solutions to [Advent of Code
2020](http://adventofcode.com/2020), a series of 25 programming puzzles.

## Installation

You can install adventofcode20 from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/adventofcode20")
```

There is probably only one reason to install this package:

### Templates

`use_day()` is a [usethis](https://usethis.r-lib.org/)-style function to
create placeholder files for each day.

``` r
use_day(1)
#> Executing: pandoc -t markdown -o 
#> "C:\Users\Tristan\AppData\Local\Temp\RtmpmajieH\file21d052ca5862.markdown" 
#> "C:\Users\Tristan\AppData\Local\Temp\RtmpmajieH\file21d052ca5862.html"
#> √ Writing 'R/day01.R'
#> * Modify 'R/day01.R'
#> ● Write your solution code here
#> ● Once you unlock Part Two, update the Roxygen block with the description
#> √ Writing 'inst/input01.txt'
#> * Modify 'inst/input01.txt'
#> ● Copy your problem input into this file
#> √ Writing 'tests/testthat/test-day01.R'
#> * Modify 'tests/testthat/test-day01.R'
#> ● Write unit tests using the examples from the problem description
#> √ Writing 'inst/run-day01.R'
#> * Modify 'inst/run-day01.R'
#> ● Run your solution on the input here. Once it works, update 
#>   R/data-solutions.R
```

This recipe, called on day 1:

-   downloads the available puzzle description for day 1 into R/day01.R
-   puts the puzzle description into a roxygen documentation block
-   creates placeholder functions for the solutions to day 1
-   creates a unit test for day (useful for the example in the puzzle
    description)
-   creates a file to contain the solution for day 1
-   creates a file to hold the input for day 1

## Package overview

The `/R` folder contains the R functions I wrote for each day. I used
some light test-driven development for the puzzles. That is, each puzzle
description provides some example inputs and outputs. Before tackling
the main test input, I write a unit-test in `/tests` that confirms that
my solution can correctly reproduce the examples. The `/inst` directory
contains the code to handle the main test input for each day.

I limited the amount of package dependencies used for these puzzles to
maximize future compatibility and to make sure that it is mostly *my
code* that solves the problems. For example, if a puzzle requires
answering questions about the data in a tree-like structure, it would be
kind of cheating for me to find a library for building and traversing
trees to tackle the problem. It’s *advent of code*, not *advent of
load*.

I have allowed myself to use:

-   base, stats, utils, tools, and so on: the base R packages
-   magrittr: for the pipe `%>%` syntax
-   rlang: for language and code evaluation
-   stringr: for regular-expression-related functions

I’ve put my R source code under a GPL-3 license. It should not apply to
the puzzle descriptions in the code comments at the top of each file. I
did not write those descriptions.

## Coding approaches

Here are the programming tasks and techniques I used for the various
puzzles.

-   01a/b *Find subsets that sum to a value:* Math. Filtering values.
-   02a/b *Count characters in a string:* Regular expressions.
-   03a/b *Count points visited in a repeating grid:* Math (modular
    arithmetic). Book-keeping.
-   04a/b *Validating data:* Functional programming. Nonstandard
    evaluation.
-   05a/b *Find missing value in binary sequence:* Math (binary, sets).
-   06a/b *Find unique/shared letters in words over various sentences:*
    Functional programming.
-   07a/b *Find how many bags are contained in other bags (tree
    traversal):* Domain specific language.
-   08a/b *Determining whether a simple program loops forever or
    terminates:* Object-orienting programming.
-   09a/b *Find sequences of numbers that do or do not sum to a value:*
    Functional programming.
-   10a/b *Count subsets of numbers that keep the gap between successive
    numbers less than or equal to 3:* Math.
-   11a/b *A Game of Life-like simulation:* Book-keeping. Recursion.
-   12a/b *Moving a point around or along a vector:* Recursion. Math
    (rotation matrix).
-   13a/b *Find bus arrival times:* Math (Chinese remainder theorem).
-   14a/b *Masking bits:* Math (binary).
-   15a/b *Keeping track of previous operations:* Book-keeping.
    Performance tricks.
-   16a/b *Validating data and finding a satisfying assignment of
    names:* Book-keeping.
-   17a/b *A 3D Game of Life-like simulation:* Table joins.
-   18a/b *Different mathematical precedence rules:* Rewriting and
    evaluating code. S3 classes for method dispatch.
-   19a/b
-   20a/b
-   21a/b *Deducing which values goes together by process of
    elimination:* Logical resolution (making an inference and adding the
    inference to a list of clauses).
-   22a/b *Sorting a deck of cards / Playing a recursive card game:*
    Recursion.

By “book-keeping”, I mean basic programming where I keep track of some
changing state like a position in a vector.

By “math”, I mean studying the problem and using math to find a shortcut
that lets me skip some computations.

## Helpful builtin R functions

Here are some functions that have I discovered, rediscovered, or
otherwise appreciate somewhat more from these exercises:

-   [`complete.cases()`](https://rdrr.io/r/stats/complete.cases.html)
-   [`intersect()`](https://rdrr.io/r/base/sets.html)
-   [`lengths()`](https://rdrr.io/r/base/lengths.html)
-   [`modifyList()`](https://rdrr.io/r/utils/modifyList.html)
-   [`outer()`](https://rdrr.io/r/base/outer.html)
-   [`prod()`](https://rdrr.io/r/base/prod.html)
-   [`strtoi()`](https://rdrr.io/r/base/strtoi.html)
-   [`chartr()`](https://rdrr.io/r/base/chartr.html)
-   [`Find() and Position()`](https://rdrr.io/r/base/funprog.html)
