test_that("Task R6 class works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  withr::local_options(cli.default_handler = function(...) { })
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  # Create a new Task
  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_claude()),
    scorer = model_graded_qa()
  )
  
  # Test that the task was initialized correctly
  expect_s3_class(tsk, "R6")
  expect_true(R6::is.R6(tsk))
  expect_true(inherits(tsk, "Task"))
  
  # Initial data should match input dataset
  task_data <- tsk$data()
  expect_equal(nrow(task_data), nrow(simple_addition))
  expect_named(task_data, c("input", "target", "id"))
  
  # Evaluate the task
  tsk$eval()
  
  # After evaluation, should have output and score
  task_data <- tsk$data()
  expect_named(
    task_data,
    c("input", "target", "id", "output", "solver", "score", "scorer", "metadata")
  )
})

test_that("Task with epochs works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  withr::local_options(cli.default_handler = function(...) { })
  
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  # Create a new Task
  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_claude()),
    scorer = model_graded_qa()
  )
  
  # Evaluate with epochs = 2
  tsk$eval(epochs = 2)
  
  # Should have doubled the number of rows
  task_data <- tsk$data()
  expect_equal(nrow(task_data), nrow(simple_addition) * 2)
  expect_named(
    task_data,
    c("input", "target", "id", "epoch", "output", "solver", "score", "scorer", "metadata")
  )
})

test_that("check_dataset works", {
  expect_error(
    Task$new(
      dataset = data.frame(input = 1),
      solver = function() {},
      scorer = function() {}
    ),
    "is missing required column"
  )
  expect_error(
    Task$new(
      dataset = data.frame(target = 1),
      solver = function() {},
      scorer = function() {}
    ),
    "is missing required column"
  )
  expect_error(
    Task$new(
      dataset = data.frame(x = 1),
      solver = function() {},
      scorer = function() {}
    ),
    "is missing required column"
  )
  
  d <- data.frame(input = "hey", target = "there")
  expect_equal(d, check_dataset(d))
})

test_that("join_epochs() works", {
  task <- data.frame(something = "here", id = 1:3)
  expect_equal(join_epochs(task, 1), task)

  joined <- join_epochs(task, 2)
  expect_equal(nrow(joined), nrow(task) * 2)
  expect_equal(joined$epoch, rep(1:3, 2))
})

test_that("Task print method works", {
  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )
  
  # Mock solver and scorer functions
  mock_solver <- function(inputs, ...) {
    list(
      outputs = rep("test", length(inputs)),
      solvers = lapply(seq_along(inputs), function(i) "solver")
    )
  }
  
  mock_scorer <- function(task, ...) {
    list(
      scores = rep(1, nrow(task)),
      scorer = "scorer",
      metadata = list()
    )
  }

  # Create a new Task
  tsk <- Task$new(
    dataset = simple_addition,
    solver = mock_solver,
    scorer = mock_scorer
  )
  
  # Test print output before evaluation
  expect_output(print(tsk), "An evaluation task")
  expect_output(print(tsk), "Status: Not evaluated")
  
  # Evaluate without triggering API calls
  tsk$.__enclos_env__$private$tbl$output <- c("4", "5")
  tsk$.__enclos_env__$private$tbl$solver <- list("solver1", "solver2")
  tsk$.__enclos_env__$private$tbl$score <- c(1, 1)
  tsk$.__enclos_env__$private$tbl$scorer <- "scorer"
  tsk$.__enclos_env__$private$tbl$metadata <- list()
  
  # Test print output after evaluation
  expect_output(print(tsk), "Status: Evaluated")
  expect_output(print(tsk), "Average score: 1.00")
})
