# Run every file
create_result_df <- function(x) {
  data.frame(
    file = x$command[2],
    status = x$status
  )
}

to_run <- list.files(
  path = "./inst/",
  pattern = "run-day",
  full.names = TRUE
)

results <- lapply(to_run, callr::rscript, fail_on_status = FALSE)

results %>%
  lapply(create_result_df) %>%
  do.call(rbind, .)
