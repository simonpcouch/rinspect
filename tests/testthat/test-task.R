test_that("Task R6 class works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(INSPECT_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) { })
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
  
  task_data <- tsk$data()
  expect_equal(nrow(task_data), nrow(simple_addition))
  expect_named(task_data, c("input", "target", "id"))
  
  tsk$eval()
  expect_snapshot(tsk)
  
  task_data <- tsk$data()
  expect_named(
    task_data,
    c("input", "target", "id", "output", "solver", "score", "scorer", "metadata")
  )
})

test_that("Task with epochs works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(INSPECT_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) { })
  
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
  
  task_data <- tsk$data()
  expect_equal(nrow(task_data), nrow(simple_addition) * 2)
  expect_named(
    task_data,
    c("input", "target", "id", "epoch", "output", "solver", "score", "scorer", "metadata")
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
