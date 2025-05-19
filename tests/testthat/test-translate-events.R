test_that("translate_turns works", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  example_sample <- example_task()$get_samples()[1, , drop = FALSE]
  chat_translated <- translate_to_events(example_sample)

  inspect_log <- example_inspect_log()
  inspect_log_first_events <- inspect_log$samples[[1]]$events

  expect_contains(
    names(inspect_log_first_events[[1]]),
    names(chat_translated[[1]])
  )

  expect_contains(
    names(inspect_log_first_events[[2]]),
    names(chat_translated[[2]])
  )
})
