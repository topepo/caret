# This is an extremely basic test that would catch serious kinds of errors
# such as the bagEarth() not returning the right kind of object, that one of
# the functions (bagEarth, format, predict) crash during normal usage, or that
# bagEarth cannot model a simplistic kind of linear equation.
test_that('bagEarth simple regression', {
  skip_on_cran()
  data <- data.frame(X = 1:100)
  data$Y <- data$X * 2
  data$training <- data$X %% 2
  fit <- bagEarth(Y ~ X, data = data[1 == data$training, ], B = 3)
  expect_type(format(fit, cat = FALSE), "character")
  expect_s3_class(fit, "bagEarth")
  data$pred <- predict(fit, newdata = data)
  data$resid <- with(data, Y - pred)
  mae <- mean(abs(data$resid))
  expect_equal(mae, 0)
})

test_that('bagEarth simple classification', {
  skip_on_cran()
  data <- twoClassSim(n = 1000)
  fit <- bagEarth(Class ~ ., data = data, B = 3, glm = list(family = binomial))
  expect_type(format(fit, cat = FALSE), "character")
  expect_s3_class(fit, "bagEarth")

  pred_response <- predict(fit, newdata = data)
  expect_s3_class(pred_response, "factor")
  expect_length(pred_response, nrow(data))

  pred_class <- predict(fit, newdata = data, type = "class")
  expect_s3_class(pred_class, "factor")
  expect_length(pred_class, 1000)

  pred_prob <- predict(fit, newdata = data, type = "prob")
  expect_s3_class(pred_prob, "data.frame")
  expect_equal(ncol(pred_prob), 2)
  expect_equal(nrow(pred_prob), 1000)
  expect_true(0 <= min(pred_prob))
  expect_true(max(pred_prob) <= 1)
})

# ------------------------------------------------------------------------------
# constructors

test_that("bagEarth coerces its inputs and can drop the stored predictors", {
  skip_on_cran()
  skip_if_not_installed("earth")

  # a one-column matrix or data frame outcome is reduced to its column
  set.seed(2314)
  fit <- bagEarth(as.matrix(trees[, -3]), as.matrix(trees[, 3]), B = 2)
  expect_s3_class(fit, "bagEarth")

  set.seed(2314)
  from_df <- bagEarth(trees[, -3], trees[, "Volume", drop = FALSE], B = 2)
  expect_s3_class(from_df, "bagEarth")

  # keepX = FALSE leaves the training predictors out of the object
  set.seed(2314)
  lean <- bagEarth(trees[, -3], trees[, 3], B = 2, keepX = FALSE)
  expect_null(lean$x)
})

test_that("bagEarth requires a binomial glm for a factor outcome", {
  skip_on_cran()
  skip_if_not_installed("earth")

  set.seed(5566)
  dat <- twoClassSim(60)
  expect_snapshot(bagEarth(dat[, 1:5], dat$Class, B = 2), error = TRUE)
})

test_that("bagEarth fits a classification model through earth's glm", {
  skip_on_cran()
  skip_if_not_installed("earth")

  set.seed(7418)
  dat <- twoClassSim(60)
  fit <- suppressWarnings(
    bagEarth(dat[, 1:5], dat$Class, B = 2, glm = list(family = binomial))
  )
  expect_identical(fit$levels, levels(dat$Class))

  # both classification prediction types work
  cls <- suppressWarnings(predict(fit, dat[, 1:5], type = "class"))
  expect_s3_class(cls, "factor")
  probs <- suppressWarnings(predict(fit, dat[, 1:5], type = "prob"))
  expect_named(probs, levels(dat$Class))
})

test_that("predict.bagEarth validates the type and can use the training data", {
  skip_on_cran()
  skip_if_not_installed("earth")

  set.seed(4192)
  fit <- bagEarth(trees[, -3], trees[, 3], B = 3)

  expect_snapshot(predict(fit, trees[, -3], type = "nope"), error = TRUE)

  # with no newdata the out-of-bag predictions of the stored data are summarised
  oob <- predict(fit)
  expect_length(oob, nrow(trees))
})

# ------------------------------------------------------------------------------
# print and summary

test_that("print.bagEarth describes the ensemble", {
  skip_on_cran()
  skip_if_not_installed("earth")

  set.seed(9655)
  fit <- bagEarth(trees[, -3], trees[, 3], B = 3)
  # the call is deparsed into the output, so only the data shape and B vary
  expect_snapshot(print(fit))
})

test_that("summary.bagEarth reports the term and variable counts", {
  skip_on_cran()
  skip_if_not_installed("earth")

  set.seed(9655)
  fit <- bagEarth(trees[, -3], trees[, 3], B = 3)
  smry <- summary(fit)
  expect_s3_class(smry, "summary.bagEarth")
  expect_named(smry, c("modelInfo", "oobStat", "bagEarthCall"))
  expect_identical(colnames(smry$modelInfo), c("Num Terms", "Num Variables"))
  # one row of model information per bootstrap sample
  expect_identical(nrow(smry$modelInfo), 3L)

  # the printed statistics are resampled floats, so mask the numbers
  expect_snapshot(print(smry), transform = mask_decimals)
})

test_that("bagEarth.formula needs a formula", {
  skip_on_cran()
  skip_if_not_installed("earth")
  expect_snapshot(caret:::bagEarth.formula(iris[, 1:4]), error = TRUE)
})

test_that("bagEarth fits from a formula and formats its terms", {
  skip_on_cran()
  skip_if_not_installed("earth")

  set.seed(3078)
  fit <- bagEarth(Volume ~ ., data = trees, B = 2)
  expect_s3_class(fit, "bagEarth")

  # format.bagEarth writes out the pooled model expression; cat = FALSE
  # returns it instead of printing, and the coefficients are fitted floats
  expect_type(format(fit, cat = FALSE), "character")
  expect_snapshot(format(fit), transform = mask_decimals)
})

test_that("predict.bagEarth averages out-of-bag predictions without stored x", {
  skip_on_cran()
  skip_if_not_installed("earth")

  # keepX = FALSE means there is no training data to fall back on, so the
  # out-of-bag predictions of each bootstrap fit are pooled instead
  set.seed(5721)
  fit <- bagEarth(trees[, -3], trees[, 3], B = 5, keepX = FALSE)
  expect_null(fit$x)
  oob <- predict(fit)
  expect_true(length(oob) > 0)
})
