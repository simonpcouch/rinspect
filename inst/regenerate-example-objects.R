# Runs the code to regenerate example eval objects used in the package.
regenerate_example_objects <- function() {
  devtools::load_all()

  cli::cli_progress_step("Regenerating example task")
  regenerate_example_task()

  cli::cli_progress_step("Regenerating example solver")
  regenerate_example_solver()

  cli::cli_progress_step("Regenerating `are` on 1 epoch")
  regenerate_are_1e()

  cli::cli_progress_step("Regenerating `are` with custom solvers")
  regenerate_are_custom_solvers()

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

  ch <- chat_anthropic(model = "claude-3-7-sonnet-latest")
  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(ch$set_system_prompt("Respond in the format 'a+b=c'.")),
    scorer = model_graded_qa(scorer_chat = ch)
  )

  tsk$eval()
  tsk <- scrub_providers(tsk)
  save(tsk, file = "inst/test/example-task.rda")
}

regenerate_example_solver <- function() {
  library(ellmer)

  solver <- chat_anthropic(
    "Respond in the format 'a+b=c'.",
    model = "claude-3-7-sonnet-latest"
  )
  solver$chat("What's 2+2?")

  solver <- scrub_provider(solver)
  save(solver, file = "inst/test/solver.rda")
}

# An R Eval on 1 epoch, via the intro vignette ------------------------
regenerate_are_1e <- function() {
  json_files <- list.files(
    "vignettes/vignettes/data/logs",
    pattern = "\\.json$",
    full.names = TRUE
  )
  if (length(json_files) > 0) {
    file.remove(json_files)
  }

  withr::local_envvar(VITALS_SHOULD_EVAL = "true")
  rmarkdown::render('vignettes/vitals.Rmd')
}

# An R Eval on 1 epoch, via the intro vignette ------------------------
regenerate_are_custom_solvers <- function() {
  json_files <- list.files(
    "vignettes/articles/data/solvers",
    pattern = "\\.json$",
    full.names = TRUE
  )
  if (length(json_files) > 0) {
    file.remove(json_files)
  }

  withr::local_envvar(VITALS_SHOULD_EVAL = "true")
  rmarkdown::render('vignettes/articles/solvers.Rmd')
}

regenerate_example_objects()
