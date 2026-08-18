test_that('bad class levels', {
  skip_on_cran()
  set.seed(5131)
  dat <- twoClassSim(100)
  dat$Class <- factor(ifelse(dat$Class == "Class1", "1", "0"))
  foo <- function(train_dat) {
    train(
      Class ~ .,
      data = train_dat,
      method = "rpart",
      metric = "ROC",
      trControl = trainControl(
        classProbs = TRUE,
        summaryFunction = twoClassSummary
      )
    )
  }
  expect_snapshot(foo(dat), error = TRUE)
})

test_that('no class probs with ROC', {
  skip_on_cran()
  set.seed(4729)
  dat <- twoClassSim(100)
  foo <- function(train_dat) {
    train(
      Class ~ .,
      data = train_dat,
      method = "rpart",
      metric = "ROC",
      trControl = trainControl(summaryFunction = twoClassSummary)
    )
  }
  expect_snapshot(foo(dat), error = TRUE)
})

test_that('numeric y and classification', {
  skip_on_cran()
  set.seed(5099)
  dat <- twoClassSim(100)
  dat$Class <- ifelse(dat$Class == "Class1", 1, 0)
  foo <- function(train_dat) {
    train(Class ~ ., data = train_dat, method = "rpart")
  }
  # the snapshot captures the two-valued-outcome warning; the follow-on
  # missing-performance warning from the same fit is silenced
  suppressWarnings(expect_snapshot_warning(foo(dat)))
})

test_that('3+ classes and twoClassSummary', {
  skip_on_cran()
  foo <- function() {
    data(oil)
    train(
      x = fattyAcids,
      y = oilType,
      method = "rpart",
      metric = "ROC",
      trControl = trainControl(
        classProbs = TRUE,
        summaryFunction = twoClassSummary
      )
    )
  }
  expect_snapshot(foo(), error = TRUE)
})

# ------------------------------------------------------------------------------
# input validation
#
# These are all cheap: train() rejects them before any model is fit. The
# custom-model helper lives in helper-fake-models.R and the data in
# helper-engine-data.R.

test_that("train needs column names and a usable outcome", {
  x <- matrix(rnorm(30), ncol = 3)
  y <- rnorm(10)
  expect_snapshot(train(x, y, method = "lm"), error = TRUE)

  colnames(x) <- paste0("v", 1:3)
  # a character outcome is neither a factor nor numeric
  expect_snapshot(train(x, letters[1:10], method = "lm"), error = TRUE)
})

test_that("train checks a custom method list for its required parts", {
  dat <- engine_regression(20)
  incomplete <- make_custom_model()
  incomplete$prob <- NULL
  incomplete$predict <- NULL
  expect_snapshot(
    train(dat[, 1:3], dat$y, method = incomplete),
    error = TRUE
  )
})

test_that("train rejects an unknown model name", {
  dat <- engine_regression(20)
  expect_snapshot(
    train(dat[, 1:3], dat$y, method = "nosuchmodel99"),
    error = TRUE
  )
})

test_that("train rejects a model that cannot handle the outcome type", {
  dat <- engine_regression(20)
  # lda is classification only
  expect_snapshot(train(dat[, 1:3], dat$y, method = "lda"), error = TRUE)
})

test_that("train rejects unknown preProcess methods", {
  dat <- engine_regression(20)
  expect_snapshot(
    train(dat[, 1:3], dat$y, method = "lm", preProcess = "bogus"),
    error = TRUE,
    transform = mask_na_label
  )
})

test_that("train rejects an outcome level with no data", {
  dat <- engine_three_class()
  y <- factor(dat$Species, levels = c(levels(dat$Species), "empty"))
  expect_snapshot(
    train(dat[, 1:4], y, method = "lda"),
    error = TRUE
  )
})

test_that("train rejects metrics that do not match the outcome", {
  cls <- engine_three_class()
  expect_snapshot(
    train(cls[, 1:4], cls$Species, method = "lda", metric = "RMSE"),
    error = TRUE
  )

  reg <- engine_regression(20)
  expect_snapshot(
    train(reg[, 1:3], reg$y, method = "lm", metric = "Accuracy"),
    error = TRUE
  )
})

test_that("train refuses sampling for a regression outcome", {
  reg <- engine_regression(20)
  expect_snapshot(
    train(
      reg[, 1:3],
      reg$y,
      method = "lm",
      trControl = trainControl(sampling = "down")
    ),
    error = TRUE
  )
})

test_that("train refuses out-of-bag estimates for models without them", {
  reg <- engine_regression(20)
  expect_snapshot(
    train(
      reg[, 1:3],
      reg$y,
      method = "lm",
      trControl = trainControl(method = "oob")
    ),
    error = TRUE
  )
})

test_that("train warns about a two-valued numeric outcome", {
  dat <- engine_regression(20)
  dat$y <- rep(c(0, 1), 10)
  # the follow-on warning about missing performance values is incidental
  suppressWarnings(
    expect_snapshot_warning(
      train(
        dat[, 1:3],
        dat$y,
        method = "lm",
        trControl = trainControl(method = "cv", number = 3)
      )
    )
  )
})

test_that("train drops class probabilities it cannot produce", {
  dat <- engine_three_class()
  # `prob` has to be present for the method list to be accepted at all, so it
  # is left in place but is not a function; the request for class probabilities
  # is then dropped with a warning
  no_prob <- make_custom_model()
  no_prob$prob <- NA
  expect_snapshot_warning(
    fit <- train(
      dat[, 1:4],
      dat$Species,
      method = no_prob,
      tuneLength = 2,
      trControl = trainControl(
        method = "cv",
        number = 3,
        classProbs = TRUE
      )
    )
  )
  expect_false(fit$control$classProbs)
})

test_that("train drops class probabilities for a regression outcome", {
  dat <- engine_regression(20)
  expect_snapshot_warning(
    fit <- train(
      dat[, 1:3],
      dat$y,
      method = "lm",
      trControl = trainControl(
        method = "cv",
        number = 3,
        classProbs = TRUE
      )
    )
  )
  expect_false(fit$control$classProbs)
})
