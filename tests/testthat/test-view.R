test_that("inspect_view() works with .json from Python Inspect", {
  log_dir <- system.file("test/inspect/logs", package = "vitals")
  expect_condition(inspect_view(log_dir), class = "vitals_viewer_start")
})

test_that("inspect_view() restarts with existing server", {
  log_dir <- system.file("test/inspect/logs", package = "vitals")
  suppressMessages(inspect_view(log_dir))
  expect_condition(inspect_view(log_dir), class = "vitals_viewer_start")
})

test_that("inspect_view() errors informatively on failure to restart", {
  log_dir <- system.file("test/inspect/logs", package = "vitals")
  suppressMessages(inspect_view(log_dir))

  testthat::local_mocked_bindings(
    stopServer = function(...) {rlang::abort("no way!")},
    .package = "httpuv"
  )
  expect_snapshot(inspect_view(log_dir), error = TRUE)
})

test_that("inspect_view() handles nonexistent log directory", {
  log_dir <- "/path/that/does/not/exist"
  expect_snapshot(inspect_view(log_dir), error = TRUE)
})
