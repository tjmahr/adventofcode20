#' Day 18: Operation Order
#'
#' [Operation Order](https://adventofcode.com/2020/day/18)
#'
#' @name day18
#' @rdname day18
#' @details
#'
#' **Part One**
#'
#' slowly appear over the horizon, you are interrupted by the child sitting
#' next to you. They\'re curious if you could help them with their
#' [math]{title="Or \"maths\", if you have more than one."} homework.
#'
#' Unfortunately, it seems like this \"math\" [follows different
#' rules](https://www.youtube.com/watch?v=3QtRK7Y2pPU&t=15) than you
#' remember.
#'
#' The homework (your puzzle input) consists of a series of expressions
#' that consist of addition (`+`), multiplication (`*`), and parentheses
#' (`(...)`). Just like normal math, parentheses indicate that the
#' expression inside must be evaluated before it can be used by the
#' surrounding expression. Addition still finds the sum of the numbers on
#' both sides of the operator, and multiplication still finds the product.
#'
#' However, the rules of *operator precedence* have changed. Rather than
#' evaluating multiplication before addition, the operators have the *same
#' precedence*, and are evaluated left-to-right regardless of the order in
#' which they appear.
#'
#' For example, the steps to evaluate the expression
#' `1 + 2 * 3 + 4 * 5 + 6` are as follows:
#'
#'     1 + 2 * 3 + 4 * 5 + 6
#'       3   * 3 + 4 * 5 + 6
#'           9   + 4 * 5 + 6
#'              13   * 5 + 6
#'                  65   + 6
#'                      71
#'
#' Parentheses can override this order; for example, here is what happens
#' if parentheses are added to form `1 + (2 * 3) + (4 * (5 + 6))`:
#'
#'     1 + (2 * 3) + (4 * (5 + 6))
#'     1 +    6    + (4 * (5 + 6))
#'          7      + (4 * (5 + 6))
#'          7      + (4 *   11   )
#'          7      +     44
#'                 51
#'
#' Here are a few more examples:
#'
#' -   `2 * 3 + (4 * 5)` becomes *`26`*.
#' -   `5 + (8 * 3 + 9 + 3 * 4 * 3)` becomes *`437`*.
#' -   `5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))` becomes *`12240`*.
#' -   `((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2` becomes *`13632`*.
#'
#' Before you can help with the homework, you need to understand it
#' yourself. *Evaluate the expression on each line of the homework; what is
#' the sum of the resulting values?*
#'
#' **Part Two**
#'
#' You manage to answer the child\'s questions and they finish part 1 of
#' their homework, but get stuck when they reach the next section:
#' *advanced* math.
#'
#' Now, addition and multiplication have *different* precedence levels, but
#' they\'re not the ones you\'re familiar with. Instead, addition is
#' evaluated *before* multiplication.
#'
#' For example, the steps to evaluate the expression
#' `1 + 2 * 3 + 4 * 5 + 6` are now as follows:
#'
#'     1 + 2 * 3 + 4 * 5 + 6
#'       3   * 3 + 4 * 5 + 6
#'       3   *   7   * 5 + 6
#'       3   *   7   *  11
#'          21       *  11
#'              231
#'
#' Here are the other examples from above:
#'
#' -   `1 + (2 * 3) + (4 * (5 + 6))` still becomes *`51`*.
#' -   `2 * 3 + (4 * 5)` becomes *`46`*.
#' -   `5 + (8 * 3 + 9 + 3 * 4 * 3)` becomes *`1445`*.
#' -   `5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))` becomes *`669060`*.
#' -   `((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2` becomes *`23340`*.
#'
#' *What do you get if you add up the results of evaluating the homework
#' problems using these new rules?*
#'
#' @param x some data
#' @return `eval_new_math(x)` and `eval_new_new_math()` return the values of the
#'   mathematical expressions.
#' @export
#' @examples
#' eval_new_math(example_new_math())
#' eval_new_new_math(example_new_math())
eval_new_math <- function(x) {
  Vectorize(eval_new_math_one, "x", SIMPLIFY = TRUE, USE.NAMES = FALSE)(x)
}


eval_new_math_one <- function(x) {
  `%plus%`  <- function(x, y) x + y
  `%times%` <- function(x, y) x * y

  x %>%
    stringr::str_replace_all("[*]", "%times%") %>%
    stringr::str_replace_all("[+]", "%plus%") %>%
    {eval(parse(text = .))}
}


#' @rdname day18
#' @export
eval_new_new_math <- function(x) {
  Vectorize(eval_new_new_math_one, "x", SIMPLIFY = TRUE, USE.NAMES = FALSE)(x)
}


eval_new_new_math_one <- function(x) {
  new_math <- function(x) {
    class(x) <- c("new_math")
    x
  }
  `+.new_math` <- function(x, y) new_math(unclass(x) * unclass(y))
  `*.new_math` <- function(x, y) new_math(unclass(x) + unclass(y))

  x %>%
    stringr::str_replace_all("[*]", "%times%") %>%
    stringr::str_replace_all("[+]", "*") %>%
    stringr::str_replace_all("%times%", "+") %>%
    stringr::str_replace_all("(\\d+)", "new_math(\\1)") %>%
    {eval(parse(text = .))} %>%
    unclass()
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day18
#' @export
example_new_math <- function(example = 1) {
  l <- list(
    a71 = "1 + 2 * 3 + 4 * 5 + 6",
    a51 = "1 + (2 * 3) + (4 * (5 + 6))",
    a26 = "2 * 3 + (4 * 5)",
    a437 = "5 + (8 * 3 + 9 + 3 * 4 * 3)",
    a12240 = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
    a13632 = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
  )
  l[[example]]
}
