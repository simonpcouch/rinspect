test_that("translate_to_messages works with example turns", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)

  ellmer_messages <- translate_to_messages(example_ellmer_solver())

  inspect_messages <- example_inspect_log()[["samples"]][[1]][["messages"]]

  zap_id <- function(l) {
    l[["id"]] <- NULL
    l
  }
  expect_equal(
    purrr::map(ellmer_messages, zap_id),
    purrr::map(inspect_messages, zap_id)
  )
})
