test_that("Task R6 class works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(INSPECT_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) { })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_claude()),
    scorer = model_graded_qa()
  )
  
  expect_true(R6::is.R6(tsk))
  expect_true(inherits(tsk, "Task"))
  expect_snapshot(tsk)
  
  expect_equal(nrow(tsk$samples), nrow(simple_addition))
  expect_named(tsk$samples, c("input", "target", "id"))
  
  tsk$eval()
  expect_snapshot(tsk)
  
  expect_named(
    tsk$samples,
    c("input", "target", "id", "result", "solver_chat", "score", "scorer", "scorer_chat", "metadata"),
    ignore.order = TRUE
  )

  expect_equal(tsk, .last_task)
})

test_that("Task with epochs works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(INSPECT_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) { })
  local_mocked_bindings(interactive = function(...) FALSE)
  
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_claude()),
    scorer = model_graded_qa()
  )
  
  tsk$eval(epochs = 2)
  
  expect_equal(nrow(tsk$samples), nrow(simple_addition) * 2)
  expect_named(
    tsk$samples,
    c("input", "target", "id", "epoch", "result", "solver_chat", "score", "scorer", "scorer_chat", "metadata"),
    ignore.order = TRUE
  )
})

test_that("check_dataset works", {
  expect_snapshot(
    Task$new(
      dataset = data.frame(input = 1),
      solver = function() {},
      scorer = function() {}
    ),
    error = TRUE
  )
  expect_snapshot(
    Task$new(
      dataset = data.frame(target = 1),
      solver = function() {},
      scorer = function() {}
    ),
    error = TRUE
  )
  expect_snapshot(
    Task$new(
      dataset = data.frame(x = 1),
      solver = function() {},
      scorer = function() {}
    ),
    error = TRUE
  )
  
  d <- data.frame(input = "hey", target = "there")
  expect_equal(d, check_dataset(d))
})

test_that("join_epochs() works", {
  task_data <- data.frame(something = "here", id = 1:3)
  expect_equal(join_epochs(task_data, 1), task_data)

  joined <- join_epochs(task_data, 2)
  expect_equal(nrow(joined), nrow(task_data) * 2)
  expect_equal(joined$epoch, rep(1:2, 3))
  expect_equal(joined$id, rep(1:3, each = 2))
})

test_that("set_id_column works", {
  # no existing `id`
  df <- tibble::tibble(input = c("a", "b"), target = c("c", "d"))
  result <- set_id_column(df)
  
  expect_equal(nrow(result), 2)
  expect_true("id" %in% names(result))
  expect_equal(result$id, 1:2)

  # existing `id``
  df <- tibble::tibble(input = c("a", "b"), target = c("c", "d"), id = c(5, 10))
  result <- set_id_column(df)
  
  expect_equal(nrow(result), 2)
  expect_true("id" %in% names(result))
  expect_equal(result$id, c(5, 10))
})

test_that("Task preserves existing id column", {
  withr::local_envvar(list(INSPECT_LOG_DIR = withr::local_tempdir()))
  local_mocked_bindings(interactive = function(...) FALSE)
  
  d <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5"),
    id = c(10, 20)
  )
  
  tsk <- Task$new(
    dataset = d,
    solver = function() {},
    scorer = function() {}
  )
  
  expect_equal(tsk$samples$id, c(10, 20))
})

test_that("Task errors informatively with duplicate ids", {
  withr::local_envvar(list(INSPECT_LOG_DIR = withr::local_tempdir()))
  
  d <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5"),
    id = c(10, 10)
  )
  
  expect_snapshot(
    Task$new(
      dataset = d,
      solver = function() {},
      scorer = function() {}
    ),
    error = TRUE
  )
})

test_that("standard errors are clustered by default when `epochs > 1`", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(INSPECT_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) { })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_claude()),
    scorer = model_graded_qa()
  )
  tsk$eval(epochs = 2)
  
  expect_true("cluster" %in% names(tsk$metrics$standard_error$arguments))
})
