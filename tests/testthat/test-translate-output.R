test_that("translate_to_output works with example turns", {
  ellmer_turns <- example_ellmer_solver()$get_turns()
  ellmer_output <- translate_to_output(turns)

  inspect_output <- example_inspect_log()[["samples"]][[1]][["output"]]
  
  expect_equal(names(ellmer_output), names(inspect_output))
  expect_equal(length(ellmer_output$usage), length(ellmer_output$usage))
})
