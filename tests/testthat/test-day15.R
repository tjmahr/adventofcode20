test_that("day 15", {
  game <- play_memory_game(example_memory_game(1), 2020)
  expect_equal(game$last_num, 436)

  game <- play_memory_game(example_memory_game(2), 2020)
  expect_equal(game$last_num, 1)

  game <- play_memory_game(example_memory_game(3), 2020)
  expect_equal(game$last_num, 10)

  game <- play_memory_game(example_memory_game(4), 2020)
  expect_equal(game$last_num, 27)

  game <- play_memory_game(example_memory_game(5), 2020)
  expect_equal(game$last_num, 78)

  game <- play_memory_game(example_memory_game(6), 2020)
  expect_equal(game$last_num, 438)

  game <- play_memory_game(example_memory_game(7), 2020)
  expect_equal(game$last_num, 1836)
})
