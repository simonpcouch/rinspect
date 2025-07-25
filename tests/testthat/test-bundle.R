test_that("vitals_bundle creates a valid bundle", {
  output_dir <- file.path(withr::local_tempdir(), "test-inspect-bundle")
  if (dir.exists(output_dir)) {
    unlink(output_dir, recursive = TRUE)
  }

  log_dir <- system.file("test/inspect/logs", package = "vitals")
  if (identical(log_dir, "")) {
    skip("Test log files not available")
  }

  dir.create(output_dir)
  expect_snapshot(
    error = TRUE,
    vitals_bundle(log_dir = log_dir, output_dir = output_dir, overwrite = FALSE)
  )
  unlink(output_dir, recursive = TRUE)

  expect_snapshot(
    result <- vitals_bundle(
      log_dir = log_dir,
      output_dir = output_dir,
      overwrite = TRUE
    ),
    transform = function(x) gsub("'[^']*'", "'***'", x)
  )

  expect_equal(result, output_dir)

  expect_true(dir.exists(output_dir))
  expect_true(file.exists(file.path(output_dir, "index.html")))
  expect_true(file.exists(file.path(output_dir, "robots.txt")))
  expect_true(dir.exists(file.path(output_dir, "logs")))
  expect_true(
    length(list.files(file.path(output_dir, "logs"), pattern = "\\.json$")) > 0
  )

  unlink(output_dir, recursive = TRUE)
})

test_that("vitals_bundle errors informatively", {
  output_dir <- file.path(withr::local_tempdir(), "test-inspect-bundle-empty")
  if (dir.exists(output_dir)) {
    unlink(output_dir, recursive = TRUE)
  }
  withr::defer(unlink(output_dir, recursive = TRUE))

  empty_log_dir <- file.path(withr::local_tempdir(), "empty-logs")
  if (!dir.exists(empty_log_dir)) {
    dir.create(empty_log_dir)
  }
  withr::defer(unlink(empty_log_dir, recursive = TRUE))

  expect_snapshot(
    error = TRUE,
    vitals_bundle(log_dir = empty_log_dir, output_dir = output_dir),
    transform = function(x) gsub("'[^']*'", "'***'", x)
  )

  non_existent_dir <- file.path(withr::local_tempdir(), "non-existent-dir")
  if (dir.exists(non_existent_dir)) {
    unlink(non_existent_dir, recursive = TRUE)
  }

  expect_snapshot(
    error = TRUE,
    vitals_bundle(log_dir = non_existent_dir, output_dir = output_dir),
    transform = function(x) gsub("'[^']*'", "'***'", x)
  )
})
