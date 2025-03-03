test_that("are is an appropriate task dataset", {
  tsk <- Task$new(
    are,
    "The R Eval",
    solver = function(...) list(...),
    scorer = function(...) list(...)
  )
  expect_true(R6::is.R6(tsk))
})
