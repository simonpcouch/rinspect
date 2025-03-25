test_that("inspect_log_dir works", {
  current_dir <- inspect_log_dir()
  withr::defer(inspect_log_dir_set(current_dir))

  # returns NA if not set
  withr::local_envvar(INSPECT_LOG_DIR = NULL)
  expect_identical(inspect_log_dir(), NA_character_)

  # returns value if set
  withr::local_envvar(INSPECT_LOG_DIR = "path/to/logs")
  expect_equal(inspect_log_dir(), "path/to/logs")
})

test_that("inspect_log_dir_set works", {
  current_dir <- inspect_log_dir()
  withr::defer(inspect_log_dir_set(current_dir))

  # returns previous value invisibly
  withr::local_envvar(INSPECT_LOG_DIR = NULL)
  expect_equal(inspect_log_dir_set("abc123"), NA_character_)

  withr::local_envvar(INSPECT_LOG_DIR = "abc123")
  expect_equal(inspect_log_dir_set("def456"), "abc123")

  # sets new value for future calls
  withr::local_envvar(INSPECT_LOG_DIR = NULL)
  inspect_log_dir_set("abc123")
  expect_equal(inspect_log_dir(), "abc123")

  inspect_log_dir_set("def456")
  expect_equal(inspect_log_dir(), "def456")
})

test_that("inspect_view() works with read + written eval file", {
  log_dir <- tempdir()
  if (!dir.exists(log_dir)) dir.create(log_dir)
  withr::defer(unlink(log_dir, recursive = TRUE))

  # file generated from Python Inspect
  eval_log <- example_inspect_log()
  eval_log_write(x = eval_log, dir = log_dir)

  expect_condition(inspect_view(log_dir), class = "rinspect_viewer_start")
})
