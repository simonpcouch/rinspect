test_that("vitals_view() works with .json from Python Inspect", {
  log_dir <- system.file("test/inspect/logs", package = "vitals")
  expect_condition(vitals_view(log_dir), class = "vitals_viewer_start")
})

test_that("vitals_view() restarts with existing server", {
  log_dir <- system.file("test/inspect/logs", package = "vitals")
  suppressMessages(vitals_view(log_dir))
  expect_condition(vitals_view(log_dir), class = "vitals_viewer_start")
})

test_that("vitals_view() errors informatively on failure to restart", {
  log_dir <- system.file("test/inspect/logs", package = "vitals")
  suppressMessages(vitals_view(log_dir))

  testthat::local_mocked_bindings(
    stopServer = function(...) {
      rlang::abort("no way!")
    },
    .package = "httpuv"
  )
  expect_snapshot(vitals_view(log_dir), error = TRUE)
})

test_that("vitals_view() handles nonexistent log directory", {
  log_dir <- "/path/that/does/not/exist"
  expect_snapshot(vitals_view(log_dir), error = TRUE)
})
