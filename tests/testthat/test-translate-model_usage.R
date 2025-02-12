test_that("translate_model_usage works with example turns", {
  turns <- example_ellmer_solver()$get_turns()
  usage <- translate_model_usage(turns)
  
  expect_type(usage, "list")
  expect_match(names(usage), "claude")
  expect_length(usage[[1]], 5)
})
