test_that("inspect_compare works with basic task comparisons", {
  skip_on_cran()
  
  load(here::here("vignettes/data/are_task.rda"))
  load(here::here("vignettes/data/are_task_openai.rda"))
  
  result <- inspect_compare(are_task, are_task_openai)
  
  expect_type(result, "list")
  expect_named(
    result, 
    c("mean_x", "mean_y", "mean_diff", "paired_se", "conf_int")
  )
  
  expect_type(result$mean_x, "double")
  expect_type(result$mean_y, "double")
  expect_type(result$mean_diff, "double")
  expect_type(result$paired_se, "double")
  expect_length(result$conf_int, 2)
  
  result_90 <- inspect_compare(are_task, are_task_openai, conf_level = 0.90)
  expect_true(abs(result_90$conf_int[2] - result_90$conf_int[1]) < 
              abs(result$conf_int[2] - result$conf_int[1]))
})

test_that("inspect_compare errors informatively", {
  skip_on_cran()
  
  load(here::here("vignettes/data/are_task.rda"))
  load(here::here("vignettes/data/are_task_openai.rda"))
  
  # tasks have different task_ids
  modified_task <- are_task_openai$clone()
  modified_task$.__enclos_env__$private$task_id <- "different_task_id"
  
  expect_snapshot(
    inspect_compare(are_task, modified_task),
    error = TRUE
  )

  # tasks have different number of samples
  modified_task <- are_task_openai$clone()
  modified_task$samples <- modified_task$samples[1:(nrow(modified_task$samples)-1), ]
  
  expect_snapshot(
    inspect_compare(are_task, modified_task),
    error = TRUE
  )

  # clustered version gets called when there are epochs
  task1 <- are_task$clone()
  task2 <- are_task$clone()
  
  task1$samples$epoch <- rep(1:2, length.out = nrow(task1$samples))
  task2$samples$epoch <- rep(1:2, length.out = nrow(task2$samples))
  
  local_mocked_bindings(
    paired_comparison_clustered = function(x, y, conf_level) {
      "Got here!"
    }
  )
  
  result <- inspect_compare(task1, task2)
  expect_equal(result, "Got here!")

  # basic input validation
  expect_error(inspect_compare(are_task, list()))
  expect_error(inspect_compare(are_task, are_task, conf_level = 1.1))
  expect_error(inspect_compare(are_task, are_task, conf_level = -0.1))
})
