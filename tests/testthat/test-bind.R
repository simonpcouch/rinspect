test_that("vitals_bind works", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_openai(model = "gpt-4.1-nano")),
    scorer = model_graded_qa()
  )

  tsk_1 <- tsk$clone()$eval()
  tsk_2 <- tsk$clone()$eval()

  res <- vitals_bind(tsk_1, tsk_2)

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("task", "id", "score", "metadata"))
  expect_equal(nrow(res), nrow(tsk_1$get_samples()) * 2)
})
