test_that("detect_includes works", {
  tsk <- example_task(scored = FALSE)
  tsk$set_scorer(detect_includes())
  tsk$score()

  expect_equal(tsk$samples$score, c(1, 1))
  # TODO: include and test for `metadata` slots (here and in model-based)

  simple_df <- tibble::tibble(
    input = c("Question 1", "Question 2"),
    result = c("The answer is C", "The answer is D"),
    target = c("c", "d")
  )

  tsk_insensitive <- Task$new(
    dataset = simple_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_insensitive$set_scorer(detect_includes(case_sensitive = FALSE))
  tsk_insensitive$score()

  expect_equal(tsk_insensitive$samples$score, c(1, 1))

  tsk_sensitive <- Task$new(
    dataset = simple_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_sensitive$set_scorer(detect_includes(case_sensitive = TRUE))
  tsk_sensitive$score()
  expect_equal(tsk_sensitive$samples$score, c(0, 0))
})

test_that("detect_match works", {
  tsk <- example_task(scored = FALSE)
  tsk$set_scorer(detect_match())
  tsk$score()

  expect_equal(tsk$samples$score, c(1, 1))

  simple_df <- tibble::tibble(
    input = c("Question 1", "Question 2"),
    result = c("The answer is C", "The answer is D"),
    target = c("c", "d")
  )

  tsk_insensitive <- Task$new(
    dataset = simple_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_insensitive$set_scorer(detect_match(case_sensitive = FALSE))
  tsk_insensitive$score()
  expect_equal(tsk_insensitive$samples$score, c(1, 1))

  tsk_sensitive <- Task$new(
    dataset = simple_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_sensitive$set_scorer(detect_match(case_sensitive = TRUE))
  tsk_sensitive$score()
  expect_equal(tsk_sensitive$samples$score, c(0, 0))
})

test_that("detect_pattern works", {
  skip_if(getRversion() > "4.4.3")
  tsk <- example_task(scored = FALSE)
  tsk$set_scorer(detect_pattern("(\\d+)\\s*=\\s*(\\d+)"))
  tsk$score()

  expect_equal(tsk$samples$score, c(1, 1))

  case_df <- tibble::tibble(
    input = c("Question 1", "Question 2"),
    result = c("The answer contains C", "The answer contains D"),
    target = c("c", "d")
  )

  tsk_insensitive <- Task$new(
    dataset = case_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_insensitive$set_scorer(detect_pattern(
    "contains\\s+([A-Za-z])",
    case_sensitive = FALSE
  ))
  tsk_insensitive$score()
  expect_equal(tsk_insensitive$samples$score, c(1, 1))

  tsk_sensitive <- Task$new(
    dataset = case_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_sensitive$set_scorer(detect_pattern(
    "contains\\s+([A-Za-z])",
    case_sensitive = TRUE
  ))
  tsk_sensitive$score()
  expect_equal(tsk_sensitive$samples$score, c(0, 0))

  all_df <- tibble::tibble(
    input = c("Question 1", "Question 2"),
    result = c(
      "Found colors red and blue",
      "Found colors green and yellow"
    ),
    target = c("red", "green")
  )

  tsk_all_false <- Task$new(
    dataset = all_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_all_false$set_scorer(detect_pattern(
    "colors\\s+(\\w+)\\s+and\\s+(\\w+)",
    all = FALSE
  ))
  tsk_all_false$score()
  expect_equal(tsk_all_false$samples$score, c(1, 1))

  tsk_all_true <- Task$new(
    dataset = all_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_all_true$set_scorer(detect_pattern(
    "colors\\s+(\\w+)\\s+and\\s+(\\w+)",
    all = TRUE
  ))
  tsk_all_true$score()
  expect_equal(tsk_all_true$samples$score, c(0, 0))
})

test_that("detect_exact works", {
  ex_task <- example_task(scored = FALSE)
  exact_df <- tibble::tibble(
    input = ex_task$samples$input,
    result = c(ex_task$samples$target[1], "non-matching output"),
    target = ex_task$samples$target
  )

  tsk <- Task$new(
    dataset = exact_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk$set_scorer(detect_exact())
  tsk$score()

  expect_equal(tsk$samples$score, c(1, 0))

  case_df <- tibble::tibble(
    input = c("Question 1", "Question 2"),
    result = c("ANSWER: C", "ANSWER: d"),
    target = c("ANSWER: c", "ANSWER: d")
  )

  tsk_insensitive <- Task$new(
    dataset = case_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_insensitive$set_scorer(detect_exact(case_sensitive = FALSE))
  tsk_insensitive$score()
  expect_equal(tsk_insensitive$samples$score, c(1, 1))

  tsk_sensitive <- Task$new(
    dataset = case_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_sensitive$set_scorer(detect_exact(case_sensitive = TRUE))
  tsk_sensitive$score()
  expect_equal(tsk_sensitive$samples$score, c(0, 1))
})

test_that("detect_answer works", {
  ex_task <- example_task(scored = FALSE)
  answer_df <- tibble::tibble(
    input = ex_task$samples$input,
    result = paste("ANSWER:", ex_task$samples$target),
    target = ex_task$samples$target
  )

  tsk <- Task$new(
    dataset = answer_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk$set_scorer(detect_answer())
  tsk$score()

  expect_equal(tsk$samples$score, c(1, 1))

  whitespace_df <- tibble::tibble(
    input = ex_task$samples$input,
    result = paste("ANSWER: ", ex_task$samples$target),
    target = ex_task$samples$target
  )

  tsk_whitespace <- Task$new(
    dataset = whitespace_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_whitespace$set_scorer(detect_answer())
  tsk_whitespace$score()
  expect_equal(tsk_whitespace$samples$score, c(1, 1))

  format_df <- tibble::tibble(
    input = c("Question 1", "Question 2"),
    result = c(
      "The solution is:\nANSWER: The Industrial Revolution",
      "ANSWER: C\nExplanation follows..."
    ),
    target = c(
      "The Industrial Revolution",
      "C"
    )
  )

  tsk_line <- Task$new(
    dataset = format_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_line$set_scorer(detect_answer(format = "line"))
  tsk_line$score()
  expect_equal(tsk_line$samples$score, c(1, 0))

  tsk_word <- Task$new(
    dataset = format_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_word$set_scorer(detect_answer(format = "word"))
  tsk_word$score()
  expect_equal(tsk_word$samples$score, c(0, 1))

  tsk_letter <- Task$new(
    dataset = format_df,
    solver = function() {},
    scorer = function() {}
  )
  tsk_letter$set_scorer(detect_answer(format = "letter"))
  tsk_letter$score()
  expect_equal(tsk_letter$samples$score, c(0, 1))
})
