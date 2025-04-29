test_that("translate_turns works", {
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
