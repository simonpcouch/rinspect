test_that("generate works", {
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  library(ellmer)

  res <- generate(chat_anthropic(model = "claude-3-7-sonnet-latest"))
  expect_contains(class(res), "function")

  chat_res <- res(list("hey", "hi", "hello"))

  expect_length(chat_res, 2)
  expect_length(chat_res[["result"]], 3)
  expect_length(chat_res[["solver_chat"]], 3)
  expect_type(chat_res[["result"]][[1]], "character")
  expect_s3_class(chat_res[["solver_chat"]][[1]], "Chat")
})

test_that("generate() allows NULL default model", {
  res <- generate()
  expect_contains(class(res), "function")
  expect_snapshot(res(), error = TRUE)
})
