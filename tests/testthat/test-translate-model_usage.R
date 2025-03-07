test_that("translate_to_model_usage works with example turns", {
  ellmer_turns <- example_ellmer_solver()$get_turns()
  ellmer_usage <- translate_to_model_usage(ellmer_turns)

  inspect_usage <- example_inspect_log()[["samples"]][[1]][["model_usage"]]
  
  expect_type(ellmer_usage, "list")
  expect_match(names(ellmer_usage), "claude")
  expect_equal(length(ellmer_usage[[1]]), length(inspect_usage[[1]]))
})
