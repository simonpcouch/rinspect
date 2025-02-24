test_that("translate_to_messages works with example turns", {
  ellmer_turns <- example_ellmer_solver()$get_turns()
  ellmer_messages <- translate_to_messages(ellmer_turns)

  inspect_messages <- example_inspect_log()[["samples"]][[1]][["messages"]]

  expect_equal(ellmer_messages, inspect_messages)
})
