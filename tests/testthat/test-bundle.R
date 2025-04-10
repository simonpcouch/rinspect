test_that("vitals_bundle creates a valid bundle", {
  skip("")
  
  output_dir <- file.path(withr::local_tempdir(), "test-inspect-bundle")
  if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
  
  log_dir <- system.file("logs", package = "vitals")
  
  dir.create(output_dir)
  expect_snapshot(
    error = TRUE,
    vitals_bundle(log_dir = log_dir, output_dir = output_dir, overwrite = FALSE)
  )
  unlink(output_dir, recursive = TRUE)
  
  result <- vitals_bundle(
    log_dir = log_dir,
    output_dir = output_dir,
    overwrite = TRUE
  )
  
  expect_equal(result, output_dir)
  
  expect_true(dir.exists(output_dir))
  expect_true(file.exists(file.path(output_dir, "index.html")))
  expect_true(file.exists(file.path(output_dir, "robots.txt")))
  expect_true(dir.exists(file.path(output_dir, "logs")))
  expect_true(file.exists(file.path(output_dir, "logs", "manifest.json")))
  expect_true(length(list.files(file.path(output_dir, "logs"), pattern = "\\.json$")) > 0)
  
  html_content <- readLines(file.path(output_dir, "index.html"))
  expect_true(any(grepl('script id="log_dir_context"', html_content, fixed = TRUE)))
  expect_true(any(grepl('"log_dir": "logs"', html_content, fixed = TRUE)))
  
  unlink(output_dir, recursive = TRUE)
})

test_that("vitals_bundle errors informatively", {
  skip("")
  
  output_dir <- file.path(withr::local_tempdir(), "test-inspect-bundle-empty")
  if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
  
  empty_log_dir <- file.path(withr::local_tempdir(), "empty-logs")
  if (!dir.exists(empty_log_dir)) dir.create(empty_log_dir)
  
  expect_snapshot(
  error = TRUE,
    vitals_bundle(log_dir = empty_log_dir, output_dir = output_dir)
  )
  
  non_existent_dir <- file.path(withr::local_tempdir(), "non-existent-dir")
  if (dir.exists(non_existent_dir)) unlink(non_existent_dir, recursive = TRUE)
  
  expect_snapshot(
    error = TRUE,
    vitals_bundle(log_dir = non_existent_dir, output_dir = output_dir)
  )
  
  unlink(output_dir, recursive = TRUE)
  unlink(empty_log_dir, recursive = TRUE)
})
