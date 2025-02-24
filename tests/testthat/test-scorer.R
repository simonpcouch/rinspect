test_that("model_graded_qa works", {
  skip_if(identical("ANTHROPIC_API_KEY", ""))
  skip_on_cran()
  withr::local_options(cli.default_handler = function(...) { })

  library(ellmer)
  
  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )
  
  tsk <- task_create(dataset = simple_addition)
  tsk <- task_solve(tsk, solver = chat_claude())
  tsk <- task_score(tsk, scorer = model_graded_qa())

  # returns scores and a complete solver chat
  expect_true(all(tsk$score %in% c(0, .5, 1)))
  expect_s3_class(tsk$solver[[1]], "Chat")
  expect_length(tsk$solver[[1]]$get_turns(), 2)
})

test_that("model_graded_fact works", {
  skip_if(identical("ANTHROPIC_API_KEY", ""))
  skip_on_cran()
  withr::local_options(cli.default_handler = function(...) { })

  library(ellmer)
  
  r_history <- tibble::tibble(
    input = c("Who created R?", "In what year was version 1.0.0 of R released?"),
    target = c("Ross Ihaka and Robert Gentleman.", "2000.")
  )
  
  tsk <- task_create(dataset = r_history)
  tsk <- task_solve(tsk, solver = chat_claude())
  tsk <- task_score(tsk, scorer = model_graded_fact())

  # returns scores and a complete solver chat
  expect_true(all(tsk$score %in% c(0, .5, 1)))
  expect_s3_class(tsk$solver[[1]], "Chat")
  expect_length(tsk$solver[[1]]$get_turns(), 2)
})
