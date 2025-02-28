test_that("basic task_create -> task_solve -> task_score works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  withr::local_options(cli.default_handler = function(...) { })
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- task_create(dataset = simple_addition)
  tsk

  expect_s3_class(tsk, "task")
  expect_snapshot(tsk)

  tsk <- task_solve(tsk, solver = chat_claude())
  tsk

  expect_s3_class(tsk, "task")
  expect_named(tsk, c("input", "target", "id", "output", "solver"))

  tsk <- task_score(tsk, scorer = model_graded_qa())
  tsk

  expect_s3_class(tsk, "task")
  expect_named(
    tsk,
    c("input", "target", "id", "output", "solver", "score", "scorer", "metadata")
  )
})

test_that("check_dataset works", {
  expect_snapshot(error = TRUE, task_create(data.frame(input = 1)))
  expect_snapshot(error = TRUE, task_create(data.frame(target = 1)))
  expect_snapshot(error = TRUE, task_create(data.frame(x = 1)))
  d <- data.frame(input = "hey", target = "there")
  expect_equal(d, check_dataset(d))
})

test_that("task_solve(epochs) works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  withr::local_options(cli.default_handler = function(...) { })
  
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- task_create(dataset = simple_addition)
  tsk <- task_solve(tsk, solver = chat_claude(), epochs = 2)
  tsk <- task_score(tsk, scorer = model_graded_qa())

  expect_s3_class(tsk, "task")
  expect_named(
    tsk,
    c("input", "target", "id", "epoch", "output", "solver", "score", "scorer", "metadata")
  )
  expect_equal(nrow(tsk), nrow(simple_addition) * 2)
})

test_that("join_epochs() works", {
  task <- data.frame(something = "here", id = 1:3)
  expect_equal(join_epochs(task, 1), task)

  joined <- join_epochs(task, 2)
  expect_equal(nrow(joined), nrow(task) * 2)
  expect_equal(joined$epoch, rep(1:3, 2))
})
