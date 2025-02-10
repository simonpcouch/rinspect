test_that("inspect_view() works with read + written eval file", {
  log_dir <- tempdir()
  if (!dir.exists(log_dir)) dir.create(log_dir)
  withr::defer(unlink(log_dir, recursive = TRUE))

  # file generated from Python Inspect
  file <- system.file(
    "logs/2025-02-08T15-51-00-06-00_simple-arithmetic_o3cKtmsvqQtmXGZhvfDrKB.json",
    package = "rinspect"
  )
  eval_log <- read_eval_log(file)
  write_eval_log(x = eval_log, dir = log_dir)

  expect_condition(inspect_view(log_dir), class = "rinspect_viewer_start")
})

test_that("inspect_view() works with template write_eval_log()", {
  log_dir <- tempdir()
  if (!dir.exists(log_dir)) dir.create(log_dir)
  withr::defer(unlink(log_dir, recursive = TRUE))

  # generated from rinspect defaults
  write_eval_log(dir = log_dir)

  expect_condition(inspect_view(log_dir), class = "rinspect_viewer_start")
})
