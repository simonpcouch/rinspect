test_that("translate_to_model_usage works with example turns", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)

  ellmer_usage <- translate_to_model_usage(example_ellmer_solver())

  inspect_usage <- example_inspect_log()[["samples"]][[1]][["model_usage"]]

  expect_type(ellmer_usage, "list")
  expect_match(names(ellmer_usage), "claude")
  expect_equal(length(ellmer_usage[[1]]), length(inspect_usage[[1]]))
})

test_that("translate_to_output works with example turns", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)

  ellmer_output <- translate_to_output(example_ellmer_solver())

  inspect_output <- example_inspect_log()[["samples"]][[1]][["output"]]

  expect_equal(names(ellmer_output), names(inspect_output))
  expect_equal(length(ellmer_output$usage), length(ellmer_output$usage))
})
