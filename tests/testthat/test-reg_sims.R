# Tests for the regression simulation functions (R/reg_sims.R): SLC14_1,
# SLC14_2, LPH07_1, LPH07_2 and the shared make_noise helper. Each one draws
# random data, so the tests check the structure of the result (dimensions,
# column names, types) rather than the values.

test_that("SLC14_1 simulates 20 predictors and an outcome", {
  set.seed(6733)
  dat <- SLC14_1(30)
  expect_s3_class(dat, "data.frame")
  expect_shape(dat, dim = c(30L, 21L))
  expect_named(dat, c(sprintf("Var%02d", 1:20), "y"))
  expect_type(dat$y, "double")
})

test_that("SLC14_2 simulates 200 predictors and an outcome", {
  set.seed(2408)
  dat <- SLC14_2(20)
  expect_shape(dat, dim = c(20L, 201L))
  # 200 predictors are zero-padded so they sort correctly
  expect_identical(names(dat)[1], "Var001")
  expect_identical(names(dat)[201], "y")
})

test_that("LPH07_1 simulates binary predictors", {
  set.seed(8830)
  dat <- LPH07_1(40)
  expect_shape(dat, dim = c(40L, 11L))
  expect_named(dat, c(sprintf("Var%02d", 1:10), "y"))
  # the predictors are Bernoulli draws
  expect_in(unlist(dat[, 1:10]), c(0, 1))
})

test_that("LPH07_1 can return a class outcome or factor predictors", {
  set.seed(1194)
  cls <- LPH07_1(40, class = TRUE)
  expect_named(cls, c(sprintf("Var%02d", 1:10), "Class"))
  expect_s3_class(cls$Class, "factor")
  expect_in(levels(cls$Class), c("Class1", "Class2"))
  # the numeric outcome is dropped in favour of the class
  expect_false("y" %in% names(cls))

  set.seed(1194)
  fac <- LPH07_1(40, factors = TRUE)
  expect_s3_class(fac$Var01, "factor")
  expect_in(levels(fac$Var01), c("val0", "val1"))
  # the outcome stays numeric
  expect_type(fac$y, "double")
})

test_that("LPH07_2 simulates 20 predictors and an outcome", {
  set.seed(5271)
  dat <- LPH07_2(25)
  expect_shape(dat, dim = c(25L, 21L))
  expect_named(dat, c(sprintf("Var%02d", 1:20), "y"))
  expect_type(dat$y, "double")
})

test_that("the simulations can add uncorrelated noise predictors", {
  set.seed(3862)
  dat <- SLC14_1(30, noiseVars = 3)
  expect_named(dat, c(sprintf("Var%02d", 1:20), paste0("Noise", 1:3), "y"))

  set.seed(3862)
  dat2 <- LPH07_2(30, noiseVars = 2)
  expect_contains(names(dat2), c("Noise1", "Noise2"))
})

test_that("the simulations can add correlated predictors", {
  skip_if_not_installed("MASS")

  # the default AR1 structure gives a Toeplitz correlation matrix
  set.seed(9411)
  dat <- SLC14_2(60, corrVars = 4, corrValue = 0.8)
  expect_contains(names(dat), paste0("Corr", 1:4))
  cors <- cor(dat[, paste0("Corr", 1:4)])
  # neighbouring columns are more correlated than distant ones
  expect_gt(cors["Corr1", "Corr2"], cors["Corr1", "Corr4"])

  # the exchangeable structure correlates every pair equally
  set.seed(9411)
  dat2 <- SLC14_1(
    60,
    corrVars = 3,
    corrType = "exch",
    corrValue = 0.7
  )
  expect_contains(names(dat2), paste0("Corr", 1:3))
})

test_that("the simulations can add both noise and correlated predictors", {
  skip_if_not_installed("MASS")

  set.seed(7079)
  dat <- LPH07_2(30, noiseVars = 2, corrVars = 2, corrValue = 0.5)
  expect_contains(names(dat), c("Noise1", "Noise2", "Corr1", "Corr2"))
})

test_that("make_noise builds binary noise for the LPH07_1 simulation", {
  skip_if_not_installed("MASS")

  set.seed(2564)
  # LPH07_1 asks for binary noise so it matches its binary predictors
  dat <- LPH07_1(40, noiseVars = 2, corrVars = 2, corrValue = 0.5)
  noise <- dat[, c("Noise1", "Noise2", "Corr1", "Corr2")]
  expect_in(unlist(noise), c(0, 1))
})

test_that("make_noise handles each combination of noise and correlation", {
  skip_if_not_installed("MASS")

  set.seed(4839)
  # noise only
  only_noise <- caret:::make_noise(20, noiseVars = 3)
  expect_named(only_noise, paste0("Noise", 1:3))

  # correlated only
  only_corr <- caret:::make_noise(20, corrVars = 2, corrValue = 0.5)
  expect_named(only_corr, paste0("Corr", 1:2))

  # both
  both <- caret:::make_noise(20, noiseVars = 2, corrVars = 2, corrValue = 0.5)
  expect_named(both, c("Noise1", "Noise2", "Corr1", "Corr2"))
})
