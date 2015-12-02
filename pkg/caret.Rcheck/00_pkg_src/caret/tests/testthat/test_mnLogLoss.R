context('mnLogLoss')

eps <- 1e-15

classes <- LETTERS[1:3]

test_dat1 <- data.frame(obs  = c("A", "A", "A", "B", "B", "C"),
                        pred = c("A", "A", "A", "B", "B", "C"),
                        A = c(1, .80, .51, .1, .2, .3),
                        B = c(0, .05, .29, .8, .6, .3),
                        C = c(0, .15, .20, .1, .2, .4))

expected1 <- log(1-eps) + log(.8) + log(.51) + log(.8) + log(.6) + log(.4)
expected1 <- c(logLoss = -expected1/nrow(test_dat1))
result1 <- mnLogLoss(test_dat1, lev = classes)

test_dat2 <- test_dat1
test_dat2$A[1] <- NA

expected2 <-  log(.8) + log(.51) + log(.8) + log(.6) + log(.4)
expected2 <- c(logLoss = -expected2/sum(complete.cases(test_dat2)))
result2 <- mnLogLoss(test_dat2, lev = classes)

test_dat3 <- test_dat1
test_dat3 <- test_dat3[, rev(1:5)]
expected3 <- expected1
result3 <- mnLogLoss(test_dat3, lev = classes[c(2, 3, 1)])

expect_equal(result1, expected1)
expect_equal(result2, expected2)
expect_equal(result3, expected3)


