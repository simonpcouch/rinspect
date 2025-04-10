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
    solver = generate(chat_anthropic(model = "claude-3-7-sonnet-latest")),
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
    c("input", "target", "id", "result", "solver_chat", "score", "scorer", "scorer_chat", "scorer_metadata"),
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
    solver = generate(chat_anthropic(model = "claude-3-7-sonnet-latest")),
    scorer = model_graded_qa()
  )
  
  tsk$eval(epochs = 2)
  
  expect_equal(nrow(tsk$samples), nrow(simple_addition) * 2)
  expect_named(
    tsk$samples,
    c("input", "target", "id", "epoch", "result", "solver_chat", "score", "scorer", "scorer_chat", "scorer_metadata"),
    ignore.order = TRUE
  )
})

test_that("Task respects `$new(epochs)`", {
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
    solver = generate(chat_anthropic(model = "claude-3-7-sonnet-latest")),
    scorer = model_graded_qa(),
    epochs = 2
  )
  
  tsk$eval()
  
  expect_equal(nrow(tsk$samples), nrow(simple_addition) * 2)
})

test_that("`$eval(epochs)` takes precedence over `$new(epochs)`", {
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
    solver = generate(chat_anthropic(model = "claude-3-7-sonnet-latest")),
    scorer = model_graded_qa(),
    epochs = 2
  )
  
  tsk$eval(epochs = 1)
  
  expect_equal(nrow(tsk$samples), nrow(simple_addition))
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

test_that("set_solver works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(INSPECT_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) { })
  local_mocked_bindings(interactive = function(...) FALSE)
  
  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    result = c("4", "5"),
    target = c("4", "5")
  )
  
  tsk <- Task$new(
    dataset = simple_addition,
    solver = function() {},
    scorer = function() {}
  )
  
  new_solver <- function(inputs) {
    list(
      result = c("4", "5"),
      solver_chat = list(ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest"), ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest"))
    )
  }
  tsk$set_solver(new_solver)
  tsk$solve()
  
  expect_equal(tsk$samples$result, c("4", "5"))
  expect_false("solver_metadata" %in% names(tsk$samples))

  # set a new solver that includes metadata
  new_solver <- function(inputs) {
    list(
      result = c("4", "5"),
      solver_chat = list(ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest"), ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest")),
      solver_metadata = c("boop!", "bop!")
    )
  }
  expect_snapshot(.res <- tsk$set_solver(new_solver))
  expect_false(
    any(c("solver_chat", "solver_metadata") %in% names(tsk$samples))
  )
  tsk$solve()

  expect_true("solver_metadata" %in% names(tsk$samples))
})

test_that("set_solver works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(INSPECT_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) { })
  local_mocked_bindings(interactive = function(...) FALSE)
  
  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    result = c("4", "5"),
    target = c("4", "5")
  )
  
  tsk <- Task$new(
    dataset = simple_addition,
    solver = function() {},
    scorer = function() {}
  )
  
  new_solver <- function(inputs) {
    list(
      result = c("4", "5"),
      solver_chat = list(ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest"), ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest"))
    )
  }
  tsk$set_solver(new_solver)
  tsk$solve()
  
  expect_equal(tsk$samples$result, c("4", "5"))
  expect_false("solver_metadata" %in% names(tsk$samples))

  # set a new solver that includes metadata
  new_solver <- function(inputs) {
    list(
      result = c("4", "5"),
      solver_chat = list(ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest"), ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest")),
      solver_metadata = c("boop!", "bop!")
    )
  }
  expect_snapshot(.res <- tsk$set_solver(new_solver))
  expect_false(
    any(c("solver_chat", "solver_metadata") %in% names(tsk$samples))
  )
  tsk$solve()

  expect_true("solver_metadata" %in% names(tsk$samples))
})

test_that("set_scorer works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(INSPECT_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) { })
  local_mocked_bindings(interactive = function(...) FALSE)
  
  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    result = c("4", "5"),
    target = c("4", "5")
  )
  
  solver <- function(inputs) {
    list(
      result = c("4", "5"),
      solver_chat = list(ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest"), ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest"))
    )
  }

  tsk <- Task$new(
    dataset = simple_addition,
    solver = solver,
    scorer = function() {}
  )
  
  tsk$solve()

  # first, return only the score
  scorer_minimal <- function(samples) {
    list(score = c(1, 1))
  }
  tsk$set_scorer(scorer_minimal)
  tsk$score()
  
  expect_equal(tsk$samples$score, c(1, 1))
  expect_false(any(c("scorer_chat", "scorer_metadata") %in% names(tsk$samples)))

  # return scorer chats
  scorer_chat <- function(samples) {
    list(
      score = c(1, 1),
      scorer_chat = list(ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest"), ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest"))
    )
  }
  expect_snapshot(.res <- tsk$set_scorer(scorer_chat))
  expect_true(all(is.na(tsk$samples$score)))
  tsk$score()
  expect_true("scorer_chat" %in% names(tsk$samples))

  # return metadata, too
  scorer_metadata <- function(samples) {
    list(
      score = c(1, 1),
      scorer_chat = list(ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest"), ellmer::chat_anthropic(model = "claude-3-7-sonnet-latest")),
      scorer_metadata = c("beep", "bop")
    )
  }
  expect_snapshot(.res <- tsk$set_scorer(scorer_metadata))
  expect_true(all(is.na(tsk$samples$score)))
  expect_false(any(c("scorer_chat", "scorer_metadata") %in% names(tsk$samples)))
  tsk$score()
  expect_true(all(c("scorer_chat", "scorer_metadata") %in% names(tsk$samples)))
})

test_that("task ids are deterministic", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))

  tsk_1 <- 
    Task$new(
      dataset = are,
      solver = generate(),
      scorer = model_graded_qa()
    )
  
  tsk_2 <- 
    Task$new(
      dataset = are,
      solver = generate(),
      scorer = model_graded_qa()
    )
  
  tsk_id_1 <- tsk_1$.__enclos_env__$private$task_id
  tsk_id_2 <- tsk_2$.__enclos_env__$private$task_id

  expect_equal(tsk_id_1, tsk_id_2)
  expect_equal(nchar(tsk_id_1), 22)
})

test_that("Task completeness is tracked and preserved", {
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
  
  mock_scorer <- function(samples) {
    list(
      score = c(1),
      metadata = list(NULL)
    )
  }
  
  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_anthropic(model = "claude-3-7-sonnet-latest")),
    scorer = mock_scorer
  )
  
  expect_false(tsk$.__enclos_env__$private$solved)
  expect_false(tsk$.__enclos_env__$private$scored)
  
  tsk$solve()
  expect_true(tsk$.__enclos_env__$private$solved)
  
  tsk$score()
  expect_true(tsk$.__enclos_env__$private$scored)
  
  expect_snapshot(.res <- tsk$set_solver(generate(chat_anthropic(model = "claude-3-7-sonnet-latest"))))
  expect_false(tsk$.__enclos_env__$private$solved)
  
  tsk$solve()
  expect_true(tsk$.__enclos_env__$private$solved)
  
  expect_snapshot(.res <- tsk$set_scorer(mock_scorer))
  expect_false(tsk$.__enclos_env__$private$scored)
  
  tsk$solve()
  tsk$score()
  
  tsk_clone <- tsk$clone()
  original_results <- tsk$samples$result
  original_scores <- tsk$samples$score
  
  tsk_clone$eval()
  expect_equal(nrow(tsk_clone$samples), nrow(simple_addition))
  
  expect_equal(tsk$samples$result, original_results)
  expect_equal(tsk$samples$score, original_scores)
  
  # test re-evaluation with epochs
  tsk_epochs <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_anthropic(model = "claude-3-7-sonnet-latest")),
    scorer = mock_scorer
  )
  
  tsk_epochs$eval(epochs = 2)
  expect_equal(nrow(tsk_epochs$samples), nrow(simple_addition) * 2)
  expect_true("epoch" %in% names(tsk_epochs$samples))
  
  tsk_epochs$eval(epochs = 3)
  expect_equal(nrow(tsk_epochs$samples), nrow(simple_addition) * 3)
  expect_true("epoch" %in% names(tsk_epochs$samples))
})

test_that("Task errors informatively with bad solver output", {
  withr::local_envvar(list(INSPECT_LOG_DIR = withr::local_tempdir()))
  withr::local_options(cli.default_handler = function(...) { })
  local_mocked_bindings(interactive = function(...) FALSE)
  
  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )
  
  bad_solver_missing_fields <- function(inputs) {
    list(
      wrong_name = c("4", "5")
      # missing solver_chat
    )
  }
  
  tsk <- Task$new(
    dataset = simple_addition,
    solver = bad_solver_missing_fields,
    scorer = function() {}
  )
  
  expect_snapshot(tsk$solve(), error = TRUE)
})

test_that("Task detects non-Chat objects in solver_chat", {
  withr::local_envvar(list(INSPECT_LOG_DIR = withr::local_tempdir()))
  withr::local_options(cli.default_handler = function(...) { })
  local_mocked_bindings(interactive = function(...) FALSE)
  
  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )
  
  bad_solver_wrong_type <- function(inputs) {
    list(
      result = c("4", "5"),
      solver_chat = list("not a Chat object", "also not a Chat object")
    )
  }
  
  tsk <- Task$new(
    dataset = simple_addition,
    solver = bad_solver_wrong_type,
    scorer = function() {}
  )
  
  expect_snapshot(tsk$solve(), error = TRUE)
})

test_that("Task errors informatively with bad scorer output", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  withr::local_envvar(list(INSPECT_LOG_DIR = withr::local_tempdir()))
  withr::local_options(cli.default_handler = function(...) { })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)
  
  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )
  
  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_anthropic(model = "claude-3-7-sonnet-latest")),
    scorer = function(samples) {
      list(wrong_name = c("4", "5"))
    }
  )
  
  expect_snapshot(tsk$eval(), error = TRUE)
})

test_that("Task detects non-Chat objects in scorer_chat", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  withr::local_envvar(list(INSPECT_LOG_DIR = withr::local_tempdir()))
  withr::local_options(cli.default_handler = function(...) { })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)
  
  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_anthropic(model = "claude-3-7-sonnet-latest")),
    scorer = function(samples) {
      list(
        score = c("4", "5"),
        scorer_chat = list("not a Chat object", "also not a Chat object")
      )
    }
  )
  
  expect_snapshot(tsk$eval(), error = TRUE)
})
