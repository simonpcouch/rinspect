
test_that("standard_error_clustered works", {
  scores <- c(1, 1, 1, 0, 0, 0)
  cluster <- c(0, 1, 1, 1, 0, 0)

  res_unclustered <- logged(standard_error)(scores)
  res_clustered <- logged(standard_error)(scores, cluster = cluster)

  expect_equal(res_unclustered$name, res_clustered$name)
  expect_false(identical(res_clustered$value, res_unclustered$value))
  expect_true("cluster" %in% names(res_clustered$arguments))
})
