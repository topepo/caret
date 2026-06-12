context("Test nearZeroVar")

test_that("nearZeroVar works properly with foreach", {
  ## shouldn't trigger error
  r <- nearZeroVar(iris, foreach = T)

  ## should pick up x, y and z
  bad.iris <- cbind(iris,
                    x = rep(-1, nrow(iris)),
                    y = rep(0, nrow(iris)),
                    z = rep(1, nrow(iris)))
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
