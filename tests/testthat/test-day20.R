test_that("day 20", {
  example_map_tiles() %>%
    find_map_corners() %>%
    prod() %>%
    expect_equal(20899048083289)
})
