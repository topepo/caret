# Test data sets live in helper-mnLogLoss.R

test_that("Multiclass logloss returns expected values", {
  skip_on_cran()
  result1 <- mnLogLoss(mnll_dat, mnll_classes)

  test_dat2 <- mnll_dat
  test_dat2$A[1] <- NA
  result2 <- mnLogLoss(test_dat2, mnll_classes)

  test_dat3 <- mnll_dat[, rev(1:5)]
  result3 <- mnLogLoss(test_dat3, mnll_classes)

  expect_equal(result1, c(logLoss = 0.424458), tolerance = 0.000001)
  expect_equal(result2, c(logLoss = 0.5093496), tolerance = 0.000001)
  expect_equal(result3, c(logLoss = 0.424458), tolerance = 0.000001)
})

# Issue #637
test_that("Twoclass logloss returns expected values", {
  skip_on_cran()
  result1 <- mnLogLoss(mnll_dat_b, mnll_classes_b)

  test_dat2 <- mnll_dat_b
  test_dat2$A[1] <- NA
  result2 <- mnLogLoss(test_dat2, mnll_classes_b)

  test_dat3 <- mnll_dat_b[, rev(1:4)]
  result3 <- mnLogLoss(test_dat3, mnll_classes_b)

  expect_equal(result1, c(logLoss = 0.244998), tolerance = 0.00001)
  expect_equal(result2, c(logLoss = 0.306248), tolerance = 0.000001)
  expect_equal(result3, c(logLoss = 0.244998), tolerance = 0.00001)
})
