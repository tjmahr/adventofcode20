test_that("multiplication works", {
  x <- example_boot_code()
  m <- setup_boot_loader(x)
  while(!m$has_seen_next_instruction()) {
    m$perform_instruction()
  }

  expect_equal(m$get_line_stream(), c(1, 2, 3, 7, 8, 4, 5))
  expect_equal(m$get_next_line(), 2)
  expect_equal(m$get_acc_stream(), c(0, 1, 1, 2, 2, 5, 5))
  expect_equal(m$get_acc_value(), 5)

  final_acc <- check_boot_lines(x)
  expect_equal(final_acc, 8)
})
