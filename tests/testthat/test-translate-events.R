test_that("translate_turns works", {
  chat <- example_ellmer_solver()
  example_sample <- example_task()[1, , drop = FALSE]
  chat_translated <- translate_to_events(chat, example_sample)

  inspect_log <- example_inspect_log()
  inspect_log_first_events <- inspect_log$samples[[1]]$events

  expect_contains(
    names(inspect_log_first_events[[1]]),
    names(chat_translated[[1]])
  )

  # currently the translated log doesn't contain some elements of the real
  # log--subset the correct one out for now
  expect_contains(
    names(inspect_log_first_events[[3]]),
    names(chat_translated[[2]])
  )
})
