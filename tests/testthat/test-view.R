test_that("inspect_view() works with .json from Python Inspect", {
  log_dir <- system.file("logs", package = "rinspect")
  expect_condition(inspect_view(log_dir), class = "rinspect_viewer_start")
})

test_that("inspect_view() restarts with existing server", {
  log_dir <- system.file("logs", package = "rinspect")
  suppressMessages(inspect_view(log_dir = log_dir))
  expect_condition(inspect_view(log_dir), class = "rinspect_viewer_start")
})

test_that("inspect_view() errors informatively on failure to restart", {
  log_dir <- system.file("logs", package = "rinspect")
  suppressMessages(inspect_view(log_dir = log_dir))

  testthat::local_mocked_bindings(
    stopServer = function(...) {rlang::abort("no way!")},
    .package = "httpuv"
  )
  expect_snapshot(inspect_view(), error = TRUE)
})

test_that("inspect_view() handles nonexistent log directory", {
  log_dir <- "/path/that/does/not/exist"
  expect_snapshot(inspect_view(log_dir = log_dir), error = TRUE)
})

test_that("viewer works with default eval_log_write directory", {
  log_dir <- file.path("test_logs")
  if (!dir.exists(log_dir)) dir.create(log_dir)
  withr::defer(unlink(log_dir, recursive = TRUE))

  eval_log_write(eval_log_new(), dir = log_dir)

  expect_condition(inspect_view(log_dir), class = "rinspect_viewer_start")
})
