#' Day 14: Docking Data
#'
#' [Docking Data](https://adventofcode.com/2020/day/14)
#'
#' @name day14
#' @rdname day14
#' @details
#'
#' **Part One**
#'
#' As your ferry approaches the sea port, the captain asks for your help
#' again. The computer system that runs this port isn't compatible with
#' the docking program on the ferry, so the docking parameters aren't
#' being correctly initialized in the docking program's memory.
#'
#' After a brief inspection, you discover that the sea port's computer
#' system uses a strange
#' [bitmask](https://en.wikipedia.org/wiki/Mask_(computing)) system in its
#' initialization program. Although you don\'t have the correct decoder
#' chip handy, you can emulate it in software!
#'
#' The initialization program (your puzzle input) can either update the
#' bitmask or write a value to memory. Values and memory addresses are both
#' 36-bit unsigned integers. For example, ignoring bitmasks for a moment, a
#' line like `mem[8] = 11` would write the value `11` to memory address
#' `8`.
#'
#' The bitmask is always given as a string of 36 bits, written with the
#' most significant bit (representing `2^35`) on the left and the least
#' significant bit (`2^0`, that is, the `1`s bit) on the right. The current
#' bitmask is applied to values immediately before they are written to
#' memory: a `0` or `1` overwrites the corresponding bit in the value,
#' while an `X` leaves the bit in the value unchanged.
#'
#' For example, consider the following program:
#'
#'     mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
#'     mem[8] = 11
#'     mem[7] = 101
#'     mem[8] = 0
#'
#' This program starts by specifying a bitmask (`mask = ....`). The mask it
#' specifies will overwrite two bits in every written value: the `2`s bit
#' is overwritten with `0`, and the `64`s bit is overwritten with `1`.
#'
#' The program then attempts to write the value `11` to memory address `8`.
#' By expanding everything out to individual bits, the mask is applied as
#' follows:
#'
#'     value:  000000000000000000000000000000001011  (decimal 11)
#'     mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
#'     result: 000000000000000000000000000001001001  (decimal 73)
#'
#' So, because of the mask, the value `73` is written to memory address `8`
#' instead. Then, the program tries to write `101` to address `7`:
#'
#'     value:  000000000000000000000000000001100101  (decimal 101)
#'     mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
#'     result: 000000000000000000000000000001100101  (decimal 101)
#'
#' This time, the mask has no effect, as the bits it overwrote were already
#' the values the mask tried to set. Finally, the program tries to write
#' `0` to address `8`:
#'
#'     value:  000000000000000000000000000000000000  (decimal 0)
#'     mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
#'     result: 000000000000000000000000000001000000  (decimal 64)
#'
#' `64` is written to address `8` instead, overwriting the value that was
#' there previously.
#'
#' To initialize your ferry\'s docking program, you need the sum of all
#' values left in memory after the initialization program completes. (The
#' entire 36-bit address space begins initialized to the value `0` at every
#' address.) In the above example, only two values in memory are not zero -
#' `101` (at address `7`) and `64` (at address `8`) - producing a sum of
#' *`165`*.
#'
#' Execute the initialization program. *What is the sum of all values left
#' in memory after it completes?*
#'
#' **Part Two**
#'
#' For some reason, the sea port's computer system still can't
#' communicate with your ferry's docking program. It must be using
#' *version 2* of the decoder chip!
#'
#' A version 2 decoder chip doesn't modify the values being written at
#' all. Instead, it acts as a [memory address
#' decoder](https://www.youtube.com/watch?v=PvfhANgLrm4). Immediately
#' before a value is written to memory, each bit in the bitmask modifies
#' the corresponding bit of the destination *memory address* in the
#' following way:
#'
#' -   If the bitmask bit is `0`, the corresponding memory address bit is
#'     *unchanged*.
#' -   If the bitmask bit is `1`, the corresponding memory address bit is
#'     *overwritten with `1`*.
#' -   If the bitmask bit is `X`, the corresponding memory address bit is
#'     [*floating*]{title="Technically, since you're on a boat, they're all floating."}.
#'
#' A *floating* bit is not connected to anything and instead fluctuates
#' unpredictably. In practice, this means the floating bits will take on
#' *all possible values*, potentially causing many memory addresses to be
#' written all at once!
#'
#' For example, consider the following program:
#'
#'     mask = 000000000000000000000000000000X1001X
#'     mem[42] = 100
#'     mask = 00000000000000000000000000000000X0XX
#'     mem[26] = 1
#'
#' When this program goes to write to memory address `42`, it first applies
#' the bitmask:
#'
#'     address: 000000000000000000000000000000101010  (decimal 42)
#'     mask:    000000000000000000000000000000X1001X
#'     result:  000000000000000000000000000000X1101X
#'
#' After applying the mask, four bits are overwritten, three of which are
#' different, and two of which are *floating*. Floating bits take on every
#' possible combination of values; with two floating bits, four actual
#' memory addresses are written:
#'
#'     000000000000000000000000000000011010  (decimal 26)
#'     000000000000000000000000000000011011  (decimal 27)
#'     000000000000000000000000000000111010  (decimal 58)
#'     000000000000000000000000000000111011  (decimal 59)
#'
#' Next, the program is about to write to memory address `26` with a
#' different bitmask:
#'
#'     address: 000000000000000000000000000000011010  (decimal 26)
#'     mask:    00000000000000000000000000000000X0XX
#'     result:  00000000000000000000000000000001X0XX
#'
#' This results in an address with three floating bits, causing writes to
#' *eight* memory addresses:
#'
#'     000000000000000000000000000000010000  (decimal 16)
#'     000000000000000000000000000000010001  (decimal 17)
#'     000000000000000000000000000000010010  (decimal 18)
#'     000000000000000000000000000000010011  (decimal 19)
#'     000000000000000000000000000000011000  (decimal 24)
#'     000000000000000000000000000000011001  (decimal 25)
#'     000000000000000000000000000000011010  (decimal 26)
#'     000000000000000000000000000000011011  (decimal 27)
#'
#' The entire 36-bit address space still begins initialized to the value 0
#' at every address, and you still need the sum of all values left in
#' memory at the end of the program. In this example, the sum is *`208`*.
#'
#' Execute the initialization program using an emulator for a version 2
#' decoder chip. *What is the sum of all values left in memory after it
#' completes?*
#'
#' @param x initialization instructions
#' @return For Part One, `process_initialization_instructions(x)` returns the
#'   values in each memory address. For Part Two,
#'   `process_initialization_instructions_v2(x)` returns the values in each
#'   memory address under the address-masking rules.
#' @export
#' @examples
#' x <- example_initialization()
#' x %>%
#'   process_initialization_instructions()
#' x <- example_initialization(2)
#' x %>%
#'   process_initialization_instructions_v2()
process_initialization_instructions <- function(x) {
  lists <- x %>%
    split(cumsum(grepl("mask", x))) %>%
    lapply(process_initialization_section) %>%
    lapply(getElement, "masked_values")

  merge_lists <- function(x, y) {
    x[names(y)] <- y
    x
  }

  lists %>%
    Reduce(merge_lists, .) %>%
    unlist(use.names = FALSE)
}


#' @rdname day14
#' @export
process_initialization_instructions_v2 <- function(x) {
  lists <- x %>%
    split(cumsum(grepl("mask", x))) %>%
    lapply(process_initialization_section_v2) %>%
    lapply(getElement, "masked_addresses")

  merge_lists <- function(x, y) {
    x[names(y)] <- y
    x
  }

  lists %>%
    Reduce(merge_lists, .) %>%
    unlist(use.names = FALSE)
}


process_initialization_section <- function(x) {
  mask <- substr(x[1], 8, nchar(x[1]))
  addresses <- x[-1] %>%
    stringr::str_extract("mem.\\d+.")
  values <- x[-1] %>%
    stringr::str_extract("= \\d+") %>%
    stringr::str_extract("\\d+") %>%
    as.numeric()

  masked_values <- values %>%
    lapply(convert_integer_to_binary) %>%
    lapply(mask_toggle_on, mask) %>%
    lapply(mask_toggle_off, mask) %>%
    lapply(convert_binary_to_integer) %>%
    stats::setNames(addresses)

  # Use last updated memory value
  masked_values <-
    masked_values[!duplicated(names(masked_values), fromLast = TRUE)]

  list(
    mask = mask,
    addresses = addresses,
    values = values,
    masked_values = masked_values
  )
}


process_initialization_section_v2 <- function(x) {
  mask <- substr(x[1], 8, nchar(x[1]))
  addresses <- x[-1] %>%
    stringr::str_extract("mem.\\d+.")

  values <- x[-1] %>%
    stringr::str_extract("= \\d+") %>%
    stringr::str_extract("\\d+") %>%
    as.numeric()

  masked_addresses <- addresses %>%
    stringr::str_extract("\\d+") %>%
    as.numeric() %>%
    lapply(convert_integer_to_binary) %>%
    lapply(mask_toggle_on, mask) %>%
    # lapply(mask_toggle_off, mask) %>%
    lapply(mask_toggle_float, mask) %>%
    lapply2(values, function(addresses, value) {
      values <- rep(value, length(addresses))
      as.list(values) %>% stats::setNames(addresses)
    }) %>%
    unlist(recursive = FALSE)

  # Use last updated value
  masked_addresses <-
    masked_addresses[!duplicated(names(masked_addresses), fromLast = TRUE)]

  list(
    mask = mask,
    addresses = addresses,
    values = values,
    masked_addresses = masked_addresses
  )
}


mask_toggle_on <- function(x, mask) {
  m <- chartr("X", "0", mask)
  bits <- x %>% strsplit("") %>% unlist() %>% as.integer()
  m <- m %>% strsplit("") %>% unlist() %>% as.integer()
  as.integer(bits | m) %>%
    paste0(collapse = "")
}


mask_toggle_off <- function(x, mask) {
  m <- chartr("X", "1", mask)
  bits <- x %>% strsplit("") %>% unlist() %>% as.integer()
  m <- m %>% strsplit("") %>% unlist() %>% as.integer()
  as.integer(bits & m) %>%
    paste0(collapse = "")
}


mask_toggle_float <- function(xs, mask) {
  chars <- mask %>% strsplit("") %>% unlist()
  if (any(chars == "X")) {
    matches <- which(chars == "X")
    new_mask <- `substr<-`(mask, matches[1], matches[1], "0")
    new_xs_0 <- `substr<-`(xs, matches[1], matches[1], "0")
    new_xs_1 <- `substr<-`(xs, matches[1], matches[1], "1")
    result <- Recall(c(new_xs_0, new_xs_1), new_mask)
  } else {
    result <- xs
  }

  result
}


# Funnily enough, 36-bit integers are bigger than R's native 32-bit integers so
# we have do it ourselves.
convert_integer_to_binary <- function(x, bits = 36) {
  powers <- rev(seq_len(bits) - 1)
  values <- rep(0, bits)
  for (pow_i in seq_along(powers)) {
    two_pow <- 2 ^ powers[pow_i]
    if (x < two_pow) next
    x <- x - two_pow
    values[pow_i] <- 1
  }
  paste0(values, collapse = "")
}


convert_binary_to_integer <- function(x) {
  twos <- 2^((nchar(x) - 1):0)
  bits <- x %>% strsplit("") %>% unlist() %>% as.numeric()
  sum(twos * bits)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day14
#' @export
example_initialization <- function(example = 1) {
  l <- list(
    a = c(
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0"
    ),
    b = c(
      "mask = 000000000000000000000000000000X1001X",
      "mem[42] = 100",
      "mask = 00000000000000000000000000000000X0XX",
      "mem[26] = 1"
    )
  )
  l[[example]]
}
