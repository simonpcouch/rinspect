test_that("apply_metric works", {
  scores <- c(1, 1, 1, 0, 0, 0)
  expect_equal(
    apply_metric(scores, mean),
    list(name = "mean", value = .5, options = dots_list())
  )

  # respects ...
  scores_na <- c(1, 1, 1, 0, 0, 0, NA)
  expect_equal(
    apply_metric(scores_na, mean, na.rm = TRUE),
    list(name = "mean", value = .5, options = dots_list(na.rm = TRUE))
  )
})

test_that("standard_error_clustered works", {
  scores <- c(1, 1, 1, 0, 0, 0)
  cluster <- c(0, 1, 1, 1, 0, 0)

  res_unclustered <- apply_metric(scores, standard_error)
  res_clustered <- apply_metric(scores, standard_error, cluster = cluster)

  expect_equal(res_unclustered$name, res_clustered$name)
  expect_false(identical(res_clustered$value, res_unclustered$value))
  expect_named(res_clustered$options, "cluster")
})
