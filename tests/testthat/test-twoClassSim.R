# Tests for twoClassSim (R/twoClassSim.R), which simulates a two-class
# problem. The data is random, so the tests check structure and the effect of
# each option rather than particular values.

test_that("twoClassSim simulates two classes and the documented predictors", {
  set.seed(4972)
  dat <- twoClassSim(50)
  expect_s3_class(dat, "data.frame")
  expect_identical(nrow(dat), 50L)
  # two informative predictors, a non-linear block, then the linear ones
  expect_contains(
    names(dat),
    c("TwoFactor1", "TwoFactor2", "Nonlinear1", "Linear01", "Class")
  )
  expect_s3_class(dat$Class, "factor")
  expect_identical(levels(dat$Class), c("Class1", "Class2"))
})

test_that("twoClassSim can add noise and correlated predictors", {
  skip_if_not_installed("MASS")

  set.seed(8140)
  dat <- twoClassSim(60, noiseVars = 2, corrVars = 3, corrValue = 0.7)
  expect_contains(names(dat), c("Noise1", "Noise2", "Corr1", "Corr2", "Corr3"))

  # the exchangeable structure correlates every pair equally, unlike AR1
  set.seed(8140)
  exch <- twoClassSim(
    60,
    corrVars = 3,
    corrType = "exch",
    corrValue = 0.7
  )
  expect_contains(names(exch), paste0("Corr", 1:3))
})

test_that("twoClassSim can drop the linear predictors", {
  set.seed(5501)
  dat <- twoClassSim(40, linearVars = 0)
  expect_all_false(grepl("^Linear", names(dat)))
})

test_that("twoClassSim can simulate an ordinal outcome", {
  set.seed(1738)
  dat <- twoClassSim(80, ordinal = TRUE)
  expect_s3_class(dat$Class, "ordered")
  expect_identical(levels(dat$Class), c("low", "med", "high"))
})

test_that("twoClassSim can mislabel a fraction of the outcomes", {
  # mislabelling flips the class probability for a random subset, so the two
  # data sets disagree even from the same seed
  set.seed(2019)
  clean <- twoClassSim(200)
  set.seed(2019)
  noisy <- twoClassSim(200, mislabel = 0.4)

  expect_identical(levels(noisy$Class), c("Class1", "Class2"))
  expect_false(identical(clean$Class, noisy$Class))
})
