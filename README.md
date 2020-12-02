
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adventofcode20

<!-- badges: start -->
<!-- badges: end -->

The goal of adventofcode20 is to …

## Installation

You can install adventofcode20 from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/adventofcode20")
```

But why would you do that?

## Templates

The one thing you might want from this package are the templates:

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

## Constraints

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
