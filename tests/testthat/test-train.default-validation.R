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

# ------------------------------------------------------------------------------
# the outcome and the predictors

test_that("train needs a numeric or factor outcome", {
  reg <- engine_regression(20)
  # A character outcome is turned into a factor before this check, so it is
  # other types that reach it; the message names the class it was given.
  expect_snapshot(
    train(reg[, 1:3], rep(c(TRUE, FALSE), 10), method = "lda"),
    error = TRUE
  )
  expect_snapshot(
    train(reg[, 1:3], as.Date("2020-01-01") + 1:20, method = "lda"),
    error = TRUE
  )
})

test_that("train wants a character matrix for the string kernels", {
  skip_on_cran()
  skip_if_not_installed("kernlab")

  strings <- c("abc", "bcd", "cde", "def")
  y <- factor(rep(c("one", "two"), 2))

  # `x` needs column names before the shape is looked at, which is why a bare
  # character vector never reaches the string-kernel check
  expect_snapshot(train(strings, y, method = "svmSpectrumString"), error = TRUE)

  # a numeric matrix and a data frame are both refused by name
  numeric_x <- matrix(1:8, ncol = 2, dimnames = list(NULL, c("a", "b")))
  expect_snapshot(
    train(numeric_x, y, method = "svmSpectrumString"),
    error = TRUE
  )
  expect_snapshot(
    train(data.frame(a = strings), y, method = "svmSpectrumString"),
    error = TRUE
  )
})

test_that("train converts a data.table to a data frame", {
  skip_on_cran()

  # A stand-in for the class rather than the package: train() only asks
  # `inherits(x, "data.table")` before coercing, and data.table is not a caret
  # dependency (declaring one just for this test is not worth it).
  reg <- engine_regression(30)
  fake_dt <- reg[, 1:3]
  class(fake_dt) <- c("data.table", "data.frame")

  set.seed(2811)
  fit <- train(
    fake_dt,
    reg$y,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_s3_class(fit, "train")
  # the coercion is what drops the extra class, so this is the evidence it ran
  expect_identical(class(fit$trainingData), "data.frame")
})

# ------------------------------------------------------------------------------
# the tuning grid

test_that("train checks the columns of a supplied tuning grid", {
  cls <- engine_three_class()

  # a column the model does not have
  expect_snapshot(
    train(
      Species ~ .,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(bogus = 5)
    ),
    error = TRUE
  )
  # the right column plus one the model does not have
  expect_snapshot(
    train(
      Species ~ .,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(k = 5, bogus = 1)
    ),
    error = TRUE
  )
})

test_that("train checks the savePredictions option", {
  reg <- engine_regression(20)
  expect_snapshot(
    train(
      reg[, 1:3],
      reg$y,
      method = "lm",
      trControl = trainControl(method = "cv", savePredictions = "some")
    ),
    error = TRUE
  )
})

test_that("train accepts the old dot-prefixed grid names", {
  skip_on_cran()

  cls <- engine_three_class()
  set.seed(6612)
  fit <- train(
    Species ~ .,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(.k = c(3, 5)),
    trControl = trainControl(method = "cv", number = 3)
  )
  # the dots are stripped, so the results are named as the model names them
  expect_named(fit$results)
  expect_in("k", names(fit$results))
})

test_that("train needs more than one candidate for adaptive resampling", {
  cls <- engine_three_class()
  expect_snapshot(
    train(
      Species ~ .,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(k = 5),
      trControl = trainControl(
        method = "adaptive_cv",
        number = 4,
        adaptive = list(min = 2, alpha = 0.05, method = "gls", complete = TRUE)
      )
    ),
    error = TRUE
  )
})

test_that("train checks what a model's loop function returns", {
  reg <- engine_regression(30)
  bad_loop <- make_custom_model()
  bad_loop$loop <- function(grid) list(nonsense = grid)

  expect_snapshot(
    train(reg[, 1:3], reg$y, method = bad_loop, tuneLength = 2),
    error = TRUE
  )
})

test_that("train ignores a loop that produces no sub-models", {
  skip_on_cran()

  reg <- engine_regression(30)
  empty_loop <- make_custom_model()
  empty_loop$loop <- function(grid) {
    list(
      loop = grid,
      submodels = rep(list(grid[0, , drop = FALSE]), nrow(grid))
    )
  }

  set.seed(4290)
  # the custom model predicts a constant, so R-squared is undefined and the
  # workflow says so; the sub-model handling is what is under test
  fit <- suppressWarnings(train(
    reg[, 1:3],
    reg$y,
    method = empty_loop,
    tuneLength = 2,
    trControl = trainControl(method = "cv", number = 2)
  ))
  # the empty sub-model frames are dropped, so every candidate is fit on its own
  expect_identical(nrow(fit$results), 2L)
})

# ------------------------------------------------------------------------------
# metrics and pre-processing

test_that("train falls back when the metric is not in the results", {
  skip_on_cran()

  reg <- engine_regression(30)
  # RMSE is a valid regression metric, but this summary function does not
  # compute it, so the first thing it does compute is used instead
  expect_snapshot_warning(
    fit <- train(
      reg[, 1:3],
      reg$y,
      method = "lm",
      metric = "RMSE",
      trControl = trainControl(
        method = "cv",
        number = 3,
        summaryFunction = function(data, lev = NULL, model = NULL) {
          c(MedianError = median(abs(data$obs - data$pred)))
        }
      )
    )
  )
  expect_identical(fit$metric, "MedianError")
})

test_that("train builds the default grid from the pre-processed predictors", {
  skip_on_cran()
  skip_if_not_installed("RANN")

  reg <- engine_regression(40)
  reg$x1[c(3, 9)] <- NA

  # the grid function sees the imputed, reduced predictors rather than the raw
  # ones, so the pre-processing has to run first
  set.seed(3517)
  fit <- train(
    reg[, 1:3],
    reg$y,
    method = "knn",
    tuneLength = 2,
    preProcess = c("knnImpute", "pca"),
    trControl = trainControl(
      method = "cv",
      number = 3,
      preProcOptions = list(k = 3, thresh = 0.9)
    )
  )
  expect_identical(nrow(fit$results), 2L)
})

test_that("train truncates a random search to the requested length", {
  skip_on_cran()
  skip_if_not_installed("kernlab")

  cls <- engine_two_class(60)
  set.seed(9182)
  fit <- train(
    Class ~ .,
    data = cls,
    method = "svmRadial",
    tuneLength = 2,
    trControl = trainControl(method = "cv", number = 3, search = "random")
  )
  # a random search proposes tuneLength combinations, no more
  expect_lte(nrow(fit$results), 2L)
})
