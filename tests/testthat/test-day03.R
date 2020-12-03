test_that("day 03", {
  x <- read_text_lines(
    "
    ..##.......
    #...#...#..
    .#....#..#.
    ..#.#...#.#
    .#...##..#.
    ..#.##.....
    .#.#.#....#
    .#........#
    #.##...#...
    #...##....#
    .#..#...#.#
    "
  )

  x %>% count_trees_visited(move_x = 1) %>% expect_equal(2)
  x %>% count_trees_visited(move_x = 3) %>% expect_equal(7)
  x %>% count_trees_visited(move_x = 5) %>% expect_equal(3)
  x %>% count_trees_visited(move_x = 7) %>% expect_equal(4)

  x %>%
    count_trees_visited(move_x = 1, move_y = 2) %>%
    expect_equal(2)
})
