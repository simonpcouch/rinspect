test_that("are is an appropriate task dataset", {
  tsk <- task_create(are, "The R Eval")
  expect_s3_class(tsk, "task")
})
