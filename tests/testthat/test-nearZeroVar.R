test_that("nearZeroVar works properly with foreach", {
  skip_on_cran()
  ## shouldn't trigger error
  r <- nearZeroVar(iris, foreach = T)

  ## should pick up x, y and z
  bad.iris <- cbind(
    iris,
    x = rep(-1, nrow(iris)),
    y = rep(0, nrow(iris)),
    z = rep(1, nrow(iris))
  )
  r1 <- nearZeroVar(bad.iris)
  r2 <- nearZeroVar(bad.iris, foreach = T)
  expect_equal(r1, r2)

  r1 <- nearZeroVar(bad.iris, names = T)
  r2 <- nearZeroVar(bad.iris, names = T, foreach = T)
  expect_equal(r1, r2)

  r1 <- nearZeroVar(bad.iris, saveMetrics = T)
  r2 <- nearZeroVar(bad.iris, saveMetrics = T, foreach = T)
  expect_equal(r1, r2)
})

test_that("checkResamples finds predictors that degenerate within a resample", {
  # in the first fold x3 is constant for class "a" (5, 5) even though it
  # varies overall, so only that column is flagged
  x <- data.frame(
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8),
    x2 = c(2, 4, 6, 8, 1, 3, 5, 7),
    x3 = c(5, 5, 7, 8, 1, 2, 3, 4)
  )
  y <- factor(c("a", "a", "b", "b", "a", "a", "b", "b"))
  index <- list(Fold1 = 1:4, Fold2 = 5:8)

  expect_identical(checkResamples(index, x, y), 3L)
})

test_that("checkResamples validates its outcome", {
  x <- data.frame(x1 = 1:4)
  expect_snapshot(checkResamples(list(1:4), x, 1:4), error = TRUE)
  one_level <- factor(rep("a", 4))
  expect_snapshot(checkResamples(list(1:4), x, one_level), error = TRUE)
})
