test_that("vitals_bind works", {
  load(here::here("vignettes/data/are_task.rda"))
  load(here::here("vignettes/data/are_task_openai.rda"))
  
  res <- vitals_bind(are_task, are_task_openai)

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("task", "id", "score", "metadata"))
  expect_equal(nrow(res), nrow(are_task$samples) * 2)
})
