test_that("check_inherits works", {
  # errors informatively with wrong class
  expect_snapshot(check_inherits(1, "list"), error = TRUE)
  expect_snapshot(check_inherits(1, "list", x_arg = "my_arg"), error = TRUE)

  # no condition otherwise
  expect_null(check_inherits(list(), "list"))
  expect_null(check_inherits(mtcars, c("data.frame", "list")))
})
