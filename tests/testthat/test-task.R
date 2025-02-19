test_that("basic task_create -> task_solve -> task_score works", {
  skip_if(identical("ANTHROPIC_API_KEY", ""))
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
    c("input", "target", "id", "output", "solver", "score", "scorer")
  )
})
