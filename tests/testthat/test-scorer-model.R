test_that("model_graded_qa works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  skip_on_cran()
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
  
  tsk$eval()

  # returns scores and a complete scorer chat
  expect_true(all(tsk$samples$score %in% c(0, .5, 1)))
  expect_s3_class(tsk$samples$solver_chat[[1]], "Chat")
  expect_length(tsk$samples$solver_chat[[1]]$get_turns(), 2)
  expect_s3_class(tsk$samples$scorer_chat[[1]], "Chat")
  expect_length(tsk$samples$scorer_chat[[1]]$get_turns(), 2)

  # by default, scorer detects last model used to solve
  expect_equal(
    tsk$samples$solver_chat[[1]]$get_model(),
    tsk$samples$scorer_chat[[1]]$get_model()
  )
})

test_that("model_graded_fact works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  skip_on_cran()
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(INSPECT_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) { })
  local_mocked_bindings(interactive = function(...) FALSE)

  library(ellmer)

  r_history <- tibble::tibble(
    input = c("Who created R?", "In what year was version 1.0.0 of R released?"),
    target = c("Ross Ihaka and Robert Gentleman.", "2000.")
  )

  tsk <- Task$new(
    dataset = r_history, 
    solver = generate(chat_claude()), 
    scorer = model_graded_fact()
  )
  
  tsk$eval()

  # returns scores and a complete scorer chat
  expect_true(all(tsk$samples$score %in% c(0, .5, 1)))
  expect_s3_class(tsk$samples$solver_chat[[1]], "Chat")
  expect_length(tsk$samples$solver_chat[[1]]$get_turns(), 2)
  expect_s3_class(tsk$samples$scorer_chat[[1]], "Chat")
  expect_length(tsk$samples$scorer_chat[[1]]$get_turns(), 2)

  # by default, scorer detects last model used to solve
  expect_equal(
    tsk$samples$solver_chat[[1]]$get_model(),
    tsk$samples$scorer_chat[[1]]$get_model()
  )
})
