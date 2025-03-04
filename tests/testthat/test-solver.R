test_that("generate works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  library(ellmer)

  res <- generate(chat_claude())
  expect_s3_class(res, "crate")

  chat_res <- res(list("hey", "hi", "hello"))

  expect_length(chat_res, 2)
  expect_length(chat_res[[1]], 3)
  expect_length(chat_res[[2]], 3)
  expect_type(chat_res[[1]][[1]], "character")
  expect_s3_class(chat_res[[2]][[1]], "Chat")
})
