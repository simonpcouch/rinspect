test_that("basic task_new -> task_evaluate works", {
  skip_if(identical("ANTHROPIC_API_KEY", ""))
  withr::local_envvar(INSPECT_LOG_DIR = test_path("logs"))
  library(ellmer)

  dataset <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )
  
  tsk <- task_new(
    name = "simple_addition",
    dataset = dataset,
    solver = generate(chat = chat_claude()),
    scorer = model_graded_qa(chat = chat_claude())
  )
  
  eval_log_file <- task_evaluate(tsk)
  withr::defer(unlink(eval_log_file))
  expect_true(file.exists(eval_log_file))
  
  # INSPECT_LOG_DIR is respected
  expect_match(eval_log_file, Sys.getenv("INSPECT_LOG_DIR"), fixed = TRUE)
})
