test_that("vitals_view() works with .json from Python Inspect", {
  log_dir <- system.file("test/inspect/logs", package = "vitals")
  if (identical(log_dir, "")) {
    skip("Test log files not available")
  }
  expect_condition(vitals_view(log_dir), class = "vitals_viewer_start")
})

test_that("vitals_view() restarts with existing server", {
  log_dir <- system.file("test/inspect/logs", package = "vitals")
  if (identical(log_dir, "")) {
    skip("Test log files not available")
  }
  suppressMessages(vitals_view(log_dir))
  expect_condition(vitals_view(log_dir), class = "vitals_viewer_start")
})

test_that("vitals_view() errors informatively on failure to restart", {
  log_dir <- system.file("test/inspect/logs", package = "vitals")
  if (identical(log_dir, "")) {
    skip("Test log files not available")
  }
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

test_that("parse_query_string() works", {
  expect_equal(parse_query_string(NULL), list())
  expect_equal(parse_query_string(""), list())

  expect_equal(parse_query_string("?key=value"), list(key = "value"))
  expect_equal(parse_query_string("key=value"), list(key = "value"))
  expect_equal(
    parse_query_string("key1=value1&key2=value2"),
    list(key1 = "value1", key2 = "value2")
  )

  # Replaces ?file= with &file= except at start
  result <- parse_query_string("file=first?file=second")
  expect_equal(names(result), c("file", "file"))
  expect_equal(result[[1]], "first")
  expect_equal(result[[2]], "second")

  # URL decodes values
  expect_equal(
    parse_query_string("key=hello%20world"),
    list(key = "hello world")
  )

  # handles parameters without values
  expect_equal(
    parse_query_string("key1&key2=value2"),
    list(key2 = "value2")
  )

  # handles multiple values for same parameter
  result <- parse_query_string("file=first&file=second&file=third")
  expect_equal(names(result), c("file", "file", "file"))
  expect_equal(result[[1]], "first")
  expect_equal(result[[2]], "second")
  expect_equal(result[[3]], "third")
})
