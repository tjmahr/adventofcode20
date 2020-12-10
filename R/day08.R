#' Day 08: Handheld Halting
#'
#' [Handheld Halting](https://adventofcode.com/2020/day/8)
#'
#' @name day08
#' @rdname day08
#' @details
#'
#' **Part One**
#'
#' Your flight to the major airline hub reaches cruising altitude without
#' incident. While you consider checking the in-flight menu for one of
#' those drinks that come with a little umbrella, you are interrupted by
#' the kid sitting next to you.
#'
#' Their [handheld game
#' console](https://en.wikipedia.org/wiki/Handheld_game_console) won\'t
#' turn on! They ask if you can take a look.
#'
#' You narrow the problem down to a strange *infinite loop* in the [boot
#' code]{title="A trendy new line of encrypted footwear?"} (your puzzle
#' input) of the device. You should be able to fix it, but first you need
#' to be able to run the code in isolation.
#'
#' The boot code is represented as a text file with one *instruction* per
#' line of text. Each instruction consists of an *operation* (`acc`, `jmp`,
#' or `nop`) and an *argument* (a signed number like `+4` or `-20`).
#'
#' -   `acc` increases or decreases a single global value called the
#'     *accumulator* by the value given in the argument. For example,
#'     `acc +7` would increase the accumulator by 7. The accumulator starts
#'     at `0`. After an `acc` instruction, the instruction immediately
#'     below it is executed next.
#' -   `jmp` *jumps* to a new instruction relative to itself. The next
#'     instruction to execute is found using the argument as an *offset*
#'     from the `jmp` instruction; for example, `jmp +2` would skip the
#'     next instruction, `jmp +1` would continue to the instruction
#'     immediately below it, and `jmp -20` would cause the instruction 20
#'     lines above to be executed next.
#' -   `nop` stands for *No OPeration* - it does nothing. The instruction
#'     immediately below it is executed next.
#'
#' For example, consider the following program:
#'
#'     nop +0
#'     acc +1
#'     jmp +4
#'     acc +3
#'     jmp -3
#'     acc -99
#'     acc +1
#'     jmp -4
#'     acc +6
#'
#' These instructions are visited in this order:
#'
#'     nop +0  | 1
#'     acc +1  | 2, 8(!)
#'     jmp +4  | 3
#'     acc +3  | 6
#'     jmp -3  | 7
#'     acc -99 |
#'     acc +1  | 4
#'     jmp -4  | 5
#'     acc +6  |
#'
#' First, the `nop +0` does nothing. Then, the accumulator is increased
#' from 0 to 1 (`acc +1`) and `jmp +4` sets the next instruction to the
#' other `acc +1` near the bottom. After it increases the accumulator from
#' 1 to 2, `jmp -4` executes, setting the next instruction to the only
#' `acc +3`. It sets the accumulator to 5, and `jmp -3` causes the program
#' to continue back at the first `acc +1`.
#'
#' This is an *infinite loop*: with this sequence of jumps, the program
#' will run forever. The moment the program tries to run any instruction a
#' second time, you know it will never terminate.
#'
#' Immediately *before* the program would run an instruction a second time,
#' the value in the accumulator is *`5`*.
#'
#' Run your copy of the boot code. Immediately before any instruction is
#' executed a second time, *what value is in the accumulator?*
#'
#' **Part Two**
#'
#' After some careful analysis, you believe that *exactly one instruction
#' is corrupted*.
#'
#' Somewhere in the program, *either* a `jmp` is supposed to be a `nop`,
#' *or* a `nop` is supposed to be a `jmp`. (No `acc` instructions were
#' harmed in the corruption of this boot code.)
#'
#' The program is supposed to terminate by *attempting to execute an
#' instruction immediately after the last instruction in the file*. By
#' changing exactly one `jmp` or `nop`, you can repair the boot code and
#' make it terminate correctly.
#'
#' For example, consider the same program from above:
#'
#'     nop +0
#'     acc +1
#'     jmp +4
#'     acc +3
#'     jmp -3
#'     acc -99
#'     acc +1
#'     jmp -4
#'     acc +6
#'
#' If you change the first instruction from `nop +0` to `jmp +0`, it would
#' create a single-instruction infinite loop, never leaving that
#' instruction. If you change almost any of the `jmp` instructions, the
#' program will still eventually find another `jmp` instruction and loop
#' forever.
#'
#' However, if you change the second-to-last instruction (from `jmp -4` to
#' `nop -4`), the program terminates! The instructions are visited in this
#' order:
#'
#'     nop +0  | 1
#'     acc +1  | 2
#'     jmp +4  | 3
#'     acc +3  |
#'     jmp -3  |
#'     acc -99 |
#'     acc +1  | 4
#'     nop -4  | 5
#'     acc +6  | 6
#'
#' After the last instruction (`acc +6`), the program terminates by
#' attempting to run the instruction below the last instruction in the
#' file. With this change, after the program terminates, the accumulator
#' contains the value *`8`* (`acc +1`, `acc +1`, `acc +6`).
#'
#' Fix the program so that it terminates normally by changing exactly one
#' `jmp` (to `nop`) or `nop` (to `jmp`). *What is the value of the
#' accumulator after the program terminates?*
#'
#' @param x boot code instructions
#' @return For Part One, `run_boot_code_and_get_acc_value(x)` executes the
#'   instructions until a line is repeated and returns the final value for the
#'   accumulator. For Part Two, `check_boot_lines(x)` executes different
#'   versions of the instructions until the code terminates. It returns the
#'   final accumulator value.
#' @export
#' @examples
#' x <- example_boot_code()
#' run_boot_code_and_get_acc_value(x)
#' check_boot_lines(x)
run_boot_code_and_get_acc_value <- function(x) {
  m <- setup_boot_loader(x)
  while(!m$has_seen_next_instruction()) {
    m$perform_instruction()
  }
  m$get_acc_value()
}


#' @rdname day08
#' @export
check_boot_lines <- function(x) {
  is_acc <- function(x) stringr::str_detect(x, "acc")

  flip_instruction <- function(x) {
    x %>%
      # Use a temp unique name we don't do nop -> jmp -> nop
      stringr::str_replace("nop", "nooop") %>%
      stringr::str_replace("jmp", "jmmmp") %>%
      stringr::str_replace("jmmmp", "nop") %>%
      stringr::str_replace("nooop", "jmp")
  }

  exhaust_instructions <- function(x) {
    m <- setup_boot_loader(x)
    while(!m$has_seen_next_instruction()) {
      m$perform_instruction()
    }
    m
  }

  lines_to_check <- seq_along(x)[!is_acc(x)]

  # Try each line
  for (line_i in lines_to_check) {
    y <- x
    y[line_i] <- flip_instruction(y[line_i])

    # Escape the loop if the machine quit normally
    m <- exhaust_instructions(y)
    if (m$has_terminated()) {
      break;
    }
  }

  m$get_acc_value()
}


#' @param code boot code instructions.
#' @param acc_value_start starting value for the accumulator. Defaults to 0.
#' @param line_start startning instruction number. Defaults to 1.
#' @rdname day08
#' @export
setup_boot_loader <- function(code, acc_value_start = 0, line_start = 1) {
  # We use closures (functions that can see or modify hidden values) stored in a
  # list to implement an (object-oriented) object.

  code <- code
  line <- line_start
  acc_value <- acc_value_start
  acc_stream <- c()
  line_stream <- c()
  code_history <- rep(FALSE, length(code))
  terminated <- FALSE

  # execution history functions
  see_instruction <- function() {
    # Note that these functions don't take any parameters but work on variables.
    # The values for these variables come from the function's surrounding
    # environment. They are not free or *open* variables; they are bound or
    # *closed* by the surrounding environment. This is where the name "closure"
    # comes from.

    # Super assignment <<- will update the values in the enclosing environment
    code_history[line] <<- TRUE
    line_stream <<- c(line_stream, line)
    line
    NULL
  }

  has_seen_next_instruction <- function() {
    if (line > length(code)) {
      terminated <<- TRUE
      result <- TRUE
    } else {
      result <- code_history[line]
    }
    result
  }

  record_acc_value <- function() {
    acc_stream <<- c(acc_stream, acc_value)
    acc_value
  }

  # single instructions
  nop <- function() {
    line <<- line + 1
    record_acc_value()
  }

  jmp <- function(n) {
    line <<- line + n
    record_acc_value()
  }

  acc <- function(n) {
    line <<- line + 1
    acc_value <<- acc_value + n
    record_acc_value()
  }

  perform_instruction <- function() {
    current_instruction <- code[line]
    see_instruction()

    if (grepl("nop", current_instruction)) {
      nop()
    } else if (grepl("jmp", current_instruction)) {
      offset <- current_instruction %>%
        stringr::str_extract("[+-].+") %>%
        as.numeric()
      jmp(offset)
    } else if (grepl("acc", current_instruction)) {
      offset <- current_instruction %>%
        stringr::str_extract("[+-].+") %>%
        as.numeric()
      acc(offset)
    }
    acc_value
  }

  # getters
  get_line_stream <- function() line_stream
  get_acc_stream <- function() acc_stream
  get_next_line <- function() line
  get_acc_value <- function() acc_value
  has_terminated <- function() terminated

  list(
    has_seen_next_instruction = has_seen_next_instruction,
    perform_instruction = perform_instruction,
    get_acc_stream = get_acc_stream,
    get_acc_value = get_acc_value,
    get_line_stream = get_line_stream,
    get_next_line = get_next_line,
    has_terminated = has_terminated
  )
}


#' @rdname day08
#' @export
example_boot_code <- function() {
  c(
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -99",
    "acc +1",
    "jmp -4",
    "acc +6"
  )
}


# Another route would have been to use recursion:
# f(code, acc_value, instruction_num, instructions_visited)


# Not used: Initially, I thought about making functions that run only once.
run_once <- function(f) {
  .times_run <- 0
  function(...) {
    .times_run <<- .times_run + 1
    if (.times_run > 1) {
      NULL
    } else {
      f(...)
    }
  }
}
