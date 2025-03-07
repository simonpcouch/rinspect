test_that("`logged()` works", {
  expect_equal(
    logged(mean)(x = 1:3),
    list(
      name = "mean",
      value = 2,
      arguments = list(x = "1:3")
    )
  )
})
