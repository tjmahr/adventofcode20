test_that("day 22", {
  x <- example_card_decks()
  result <- x %>% sort_card_decks()

  expect_equal(result$p2, c(3, 2, 10, 6, 8, 5, 9, 4, 7, 1))
  expect_equal(result$.score, 306)

  x0 <- example_card_decks(1) %>% prepare_card_decks()
  x1 <- x0 %>% play_recursive_combat_turn()
  x2 <- x1 %>% play_recursive_combat_turn()

  expect_equal(x1$p1, c(2, 6, 3, 1, 9, 5))
  expect_equal(x1$p2, c(8, 4, 7, 10))
  expect_equal(x2$p1, c(6, 3, 1, 9, 5))
  expect_equal(x2$p2, c(4, 7, 10, 8, 2))

  full_game <- example_card_decks(1) %>%
    play_recursive_combat_game()

  expect_equal(full_game$p2, c(7, 5, 6, 2, 4, 1, 10, 8, 9, 3))
})
