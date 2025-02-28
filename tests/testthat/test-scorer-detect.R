test_that("detect_includes works", {
  tsk <- example_task(scored = FALSE)
  tsk <- task_score(tsk, scorer = detect_includes())

  expect_equal(tsk$score, c(1, 1))
  expect_equal(tsk$scorer, c("includes", "includes"))
  # TODO: include and test for `metadata` slots (here and in model-based)

  # respects case sensitivity
  tsk <- example_task(scored = FALSE)
  tsk$output[1] <- "The answer is C"
  tsk$target[1] <- "c"
  tsk$output[2] <- "The answer is D"
  tsk$target[2] <- "d"

  tsk <- task_score(tsk, scorer = detect_includes(case_sensitive = FALSE))
  expect_equal(tsk$score, c(1, 1))

  tsk <- task_score(tsk, scorer = detect_includes(case_sensitive = TRUE))
  expect_equal(tsk$score, c(0, 0))
})

test_that("detect_match works", {
  tsk <- example_task(scored = FALSE)
  tsk <- task_score(tsk, scorer = detect_match())

  expect_equal(tsk$score, c(1, 1))
  expect_equal(tsk$scorer, c("match", "match"))

  # respects case sensitivity
  tsk <- example_task(scored = FALSE)
  tsk$output[1] <- "The answer is C"
  tsk$target[1] <- "c"
  tsk$output[2] <- "The answer is D"
  tsk$target[2] <- "d"

  tsk <- task_score(tsk, scorer = detect_match(case_sensitive = FALSE))
  expect_equal(tsk$score, c(1, 1))

  tsk <- task_score(tsk, scorer = detect_match(case_sensitive = TRUE))
  expect_equal(tsk$score, c(0, 0))
})

test_that("detect_pattern works", {
  tsk <- example_task(scored = FALSE)
  tsk <- task_score(tsk, scorer = detect_pattern("(\\d+)\\s*=\\s*(\\d+)"))

  expect_equal(tsk$score, c(1, 1))
  expect_equal(tsk$scorer, c("pattern", "pattern"))

  # respects case sensitivity
  tsk <- example_task(scored = FALSE)
  tsk$output[1] <- "The answer contains C"
  tsk$target[1] <- "c"
  tsk$output[2] <- "The answer contains D"
  tsk$target[2] <- "d"

  tsk <- task_score(
    tsk,
    scorer = detect_pattern("contains\\s+([A-Za-z])", case_sensitive = FALSE)
  )
  expect_equal(tsk$score, c(1, 1))

  tsk <- task_score(
    tsk,
    scorer = detect_pattern("contains\\s+([A-Za-z])", case_sensitive = TRUE)
  )
  expect_equal(tsk$score, c(0, 0))

  # respects `all`
  tsk <- example_task(scored = FALSE)
  tsk$output[1] <- "Found colors red and blue"
  tsk$target[1] <- "red"
  tsk$output[2] <- "Found colors green and yellow"
  tsk$target[2] <- "green"

  tsk <- task_score(
    tsk,
    scorer = detect_pattern("colors\\s+(\\w+)\\s+and\\s+(\\w+)", all = FALSE)
  )
  expect_equal(tsk$score, c(1, 1))

  tsk <- task_score(
    tsk,
    scorer = detect_pattern("colors\\s+(\\w+)\\s+and\\s+(\\w+)", all = TRUE)
  )
  expect_equal(tsk$score, c(0, 0))
})

test_that("detect_exact works", {
  tsk <- example_task(scored = FALSE)

  # "rig" the results so that we can test for one exact match
  tsk$output[1] <- tsk$target[1]
  tsk <- task_score(tsk, scorer = detect_exact())

  expect_equal(tsk$score, c(1, 0))
  expect_equal(tsk$scorer, c("exact", "exact"))

  # respects case sensitivity
  tsk <- example_task(scored = FALSE)
  tsk$output[1] <- "ANSWER: C"
  tsk$target[1] <- "ANSWER: c"
  tsk$output[2] <- "ANSWER: d"
  tsk$target[2] <- "ANSWER: d"

  tsk <- task_score(tsk, scorer = detect_exact(case_sensitive = FALSE))
  expect_equal(tsk$score, c(1, 1))

  tsk <- task_score(tsk, scorer = detect_exact(case_sensitive = TRUE))
  expect_equal(tsk$score, c(0, 1))
})

test_that("detect_answer works", {
  tsk <- example_task(scored = FALSE)
  tsk$output <- paste("ANSWER:", tsk$target)
  tsk <- task_score(tsk, scorer = detect_answer())

  expect_equal(tsk$score, c(1, 1))
  expect_equal(tsk$scorer, c("answer", "answer"))

  # whitespace is trimmed by default
  tsk <- example_task(scored = FALSE)
  tsk$output <- paste("ANSWER: ", tsk$target)
  tsk <- task_score(tsk, scorer = detect_answer())

  expect_equal(tsk$score, c(1, 1))
  expect_equal(tsk$scorer, c("answer", "answer"))

  # respects format argument
  tsk <- example_task(scored = FALSE)
  tsk$output <- c(
    "The solution is:\nANSWER: The Industrial Revolution",
    "ANSWER: C\nExplanation follows..."
  )
  tsk$target <- c(
    "The Industrial Revolution",
    "C"
  )

  tsk <- task_score(tsk, scorer = detect_answer(format = "line"))
  expect_equal(tsk$score, c(1, 0))
  expect_equal(tsk$scorer, c("answer", "answer"))

  # letter is the same word in the second example
  tsk <- task_score(tsk, scorer = detect_answer(format = "word"))
  expect_equal(tsk$score, c(0, 1))

  tsk <- task_score(tsk, scorer = detect_answer(format = "letter"))
  expect_equal(tsk$score, c(0, 1))
})
