

filter_rows_in <- function(data, ...) {
  dots <- rlang::enquos(...)

  results <- as.list(seq_along(dots))
  for (rule_i in seq_along(dots)) {
    values <- rlang::eval_tidy(dots[[rule_i]], data = data)
    results[[rule_i]] <- values %in% TRUE
  }

  anded <- Reduce(function(x, y) x & y, results)
  data[anded, , drop = FALSE]
}
