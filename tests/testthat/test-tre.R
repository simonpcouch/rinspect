test_that("tre is an appropriate task dataset", {
  tsk <- task_create(tre, "The R Eval")
  expect_s3_class(tsk, "task")
})
