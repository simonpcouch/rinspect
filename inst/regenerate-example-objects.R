# Runs the code to regenerate example eval objects used in the package.
regenerate_example_objects <- function() {
  devtools::load_all()

  cli::cli_progress_step("Regenerating example task")
  regenerate_example_task()

  cli::cli_progress_step("Regenerating `are` on 1 epoch")
  regenerate_are_1e()

  cli::cli_progress_done()
}

# example_task -- a minimal, 2-row toy eval ---------------------------
regenerate_example_task <- function() {
  library(ellmer)
  library(tibble)

  simple_addition <- tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )
    
  tsk <- Task$new(
    dataset = simple_addition, 
    solver = generate(chat_claude(model = "claude-3-7-sonnet-latest")), 
    scorer = model_graded_qa()
  )
  
  tsk$eval()
  save(tsk, file = "inst/test-objects/example-task.rda")
}

# An R Eval on 1 epoch, via the intro vignette ------------------------
regenerate_are_1e <- function() {
  withr::local_envvar(RINSPECT_SHOULD_EVAL = "true")
  rmarkdown::render('vignettes/rinspect.Rmd')
}

regenerate_example_objects()
