test_that("multiplication works", {
  normalize_move <- function(x) {
    l <- x %>% stringr::str_extract_all("(\\d|\\s)+[(]") %>% unlist()
    m <- x %>% stringr::str_extract_all("[(]\\d[)]") %>% unlist()
    r <- x %>% stringr::str_extract_all("[)](\\d|\\s)+") %>% unlist()
    paste0(m, r, l) %>%
      stringr::str_extract_all("\\d") %>%
      unlist() %>%
      as.numeric()
  }
  example_crab_cups() %>%
    play_crab_cups(0) %>%
    expect_equal(normalize_move("cups: (3) 8  9  1  2  5  4  6  7"))

  example_crab_cups() %>%
    play_crab_cups(1) %>%
    expect_equal(normalize_move("cups:  3 (2) 8  9  1  5  4  6  7"))

  example_crab_cups() %>%
    play_crab_cups(2) %>%
    expect_equal(normalize_move("cups:  3  2 (5) 4  6  7  8  9  1"))

  example_crab_cups() %>%
    play_crab_cups(3) %>%
    expect_equal(normalize_move("cups:  7  2  5 (8) 9  1  3  4  6"))

  example_crab_cups() %>%
    play_crab_cups(7) %>%
    expect_equal(normalize_move("cups:  8  3  6  7  4  1  9 (2) 5"))

  example_crab_cups() %>%
    play_crab_cups(10) %>%
    expect_equal(normalize_move("cups:  5 (8) 3  7  4  1  9  2  6"))

  example_crab_cups() %>%
    play_crab_cups(100) %>%
    sort_crab_cups() %>%
    expect_equal(67384529)
})
