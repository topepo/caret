context('mnLogLoss')

classes <- LETTERS[1:3]

test_dat1 <- data.frame(obs  = c("A", "A", "A", "B", "B", "C"),
                        pred = c("A", "A", "A", "B", "B", "C"),
                        A = c(1, .80, .51, .1, .2, .3),
                        B = c(0, .05, .29, .8, .6, .3),
                        C = c(0, .15, .20, .1, .2, .4),
                        stringsAsFactors = TRUE)

test_that("Multiclass logloss returns expected values", {
  result1 <- mnLogLoss(test_dat1, classes)

  test_dat2 <- test_dat1
  test_dat2$A[1] <- NA
  result2 <- mnLogLoss(test_dat2, classes)

  test_dat3 <- test_dat1
  test_dat3 <- test_dat3[, rev(1:5)]
  result3 <- mnLogLoss(test_dat3, classes)

  expect_equal(result1, c(logLoss = 0.424458), tolerance = .000001)
  expect_equal(result2, c(logLoss = 0.5093496), tolerance = .000001)
  expect_equal(result3, c(logLoss = 0.424458), tolerance = .000001)

})

# Issue #637

classes.b <- c("A", "B")

test_dat1.b <- data.frame(obs  = c("A", "A", "A", "B", "B"),
                          pred = c("A", "A", "A", "B", "B"),
                          A = c(1, .80, .51, .1, .2),
                          B = c(0, .20, .49, .9, .8),
                          stringsAsFactors = TRUE)

test_that("Twoclass logloss returns expected values", {
  result1 <- mnLogLoss(test_dat1.b, classes.b)

  test_dat2.b <- test_dat1.b
  test_dat2.b$A[1] <- NA
  result2 <- mnLogLoss(test_dat2.b, classes.b)

  test_dat3.b <- test_dat1.b
  test_dat3.b <- test_dat3.b[, rev(1:4)]
  result3 <- mnLogLoss(test_dat3.b, classes.b)

  expect_equal(result1, c(logLoss = 0.244998), tolerance = .000001)
  expect_equal(result2, c(logLoss = 0.306248), tolerance = .000001)
  expect_equal(result3, c(logLoss = 0.244998), tolerance = .000001)

})
