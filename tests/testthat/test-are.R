test_that("are is an appropriate task dataset", {
  withr::local_envvar(VITALS_LOG_DIR = withr::local_tempdir())
  tsk <- Task$new(
    dataset = are,
    name = "The R Eval",
    solver = function(...) list(...),
    scorer = function(...) list(...)
  )
  expect_true(R6::is.R6(tsk))
})
