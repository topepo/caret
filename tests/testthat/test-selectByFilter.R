# Tests for filter-based feature selection (sbf). The pure helpers (sbfControl,
# anovaScores) and the standalone nullModel are unit-tested directly; the sbf()
# workflow and its methods are exercised by small integration fits at the bottom.

# ------------------------------------------------------------------------------
# sbfControl

test_that("sbfControl fills in sensible defaults", {
  ctrl <- sbfControl()
  expect_identical(ctrl$method, "boot")
  # with no functions supplied it defaults to caretSBF
  expect_identical(ctrl$functions, caretSBF)
})

# ------------------------------------------------------------------------------
# univariate filter scores

test_that("anovaScores returns a small p-value for an informative predictor", {
  set.seed(1)
  grp <- factor(rep(c("a", "b"), each = 20))
  signal <- c(rnorm(20, 0), rnorm(20, 5))
  expect_lt(caret:::anovaScores(signal, grp), 0.05)
})

test_that("anovaScores rejects factor predictors", {
  expect_snapshot(
    caret:::anovaScores(factor(c("a", "b")), factor(c("a", "b"))),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------
# nullModel (a baseline that ignores the predictors)

test_that("nullModel predicts the majority class for a factor outcome", {
  nm <- caret:::nullModel(y = factor(c("a", "a", "b")))
  expect_s3_class(nm, "nullModel")
  expect_identical(nm$value, "a")
  expect_identical(nm$levels, c("a", "b"))

  # class predictions: the majority class, one per new row
  preds <- predict(nm, newdata = data.frame(x = 1:4))
  expect_identical(as.character(preds), rep("a", 4))
  # probability predictions: the class proportions
  probs <- predict(nm, type = "prob")
  expect_identical(colnames(probs), c("a", "b"))
})

test_that("nullModel predicts the mean for a numeric outcome", {
  nm <- caret:::nullModel(y = c(1, 2, 3, 4))
  expect_null(nm$levels)
  # mean(1:4) is exactly 2.5, so this round-trips bit-for-bit
  expect_identical(nm$value, 2.5)
  expect_identical(predict(nm, newdata = data.frame(x = 1:3)), rep(2.5, 3))
  # class/prob predictions make no sense for regression
  expect_snapshot(predict(nm, type = "prob"), error = TRUE)
})

test_that("print.nullModel labels the model type correctly", {
  # both prints are deterministic (fixed inputs, exact predicted values)
  expect_snapshot(print(caret:::nullModel(y = factor(c("a", "a", "b")))))
  expect_snapshot(print(caret:::nullModel(y = c(1, 2, 3))))
})

# ------------------------------------------------------------------------------
# sbf() workflow + methods

test_that("sbf runs and its methods behave (default interface)", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  set.seed(1)
  dat <- twoClassSim(150)
  set.seed(1)
  sf <- sbf(
    dat[, 1:8],
    dat$Class,
    sbfControl = sbfControl(functions = ldaSBF, method = "cv", number = 3)
  )

  expect_s3_class(sf, "sbf")
  expect_identical(predictors(sf), sf$optVariables)
  expect_identical(nrow(predict(sf, dat[, 1:8])), nrow(dat))
  expect_snapshot(print(sf))
})

test_that("sbf works with the formula and recipe interfaces", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  set.seed(1)
  full <- twoClassSim(150)
  dat <- full[, c(names(full)[1:8], "Class")]

  set.seed(1)
  sf_form <- sbf(
    Class ~ .,
    data = dat,
    sbfControl = sbfControl(functions = ldaSBF, method = "cv", number = 3)
  )
  expect_s3_class(sf_form, "sbf")

  set.seed(1)
  rec <- recipes::recipe(Class ~ ., data = dat)
  sf_rec <- sbf(
    rec,
    data = dat,
    sbfControl = sbfControl(functions = ldaSBF, method = "cv", number = 3)
  )
  expect_s3_class(sf_rec, "sbf")
})

# ------------------------------------------------------------------------------
# resampling methods and interfaces

test_that("sbf runs with each resampling method", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  cls <- engine_two_class(80)
  x <- cls[, names(cls) != "Class"]

  for (m in c("boot", "boot632", "LOOCV")) {
    dat <- if (m == "LOOCV") x[1:20, ] else x
    y <- if (m == "LOOCV") cls$Class[1:20] else cls$Class
    set.seed(3390)
    sf <- suppressWarnings(sbf(
      dat,
      y,
      sbfControl = sbfControl(functions = ldaSBF, method = m, number = 3)
    ))
    expect_s3_class(sf, "sbf")
    expect_identical(sf$control$method, m)
    expect_all_false(is.na(sf$results$Accuracy))
  }
})

test_that("sbf runs a recipe through leave-one-out resampling", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  cls <- engine_two_class(24)
  rec <- recipes::recipe(Class ~ ., data = cls)

  set.seed(6011)
  sf <- suppressWarnings(sbf(
    rec,
    data = cls,
    sbfControl = sbfControl(functions = ldaSBF, method = "LOOCV")
  ))
  expect_s3_class(sf, "sbf")
  expect_identical(sf$control$method, "LOOCV")
  # the predictions are pooled into a single estimate rather than kept per row
  expect_identical(nrow(sf$results), 1L)
  expect_all_false(is.na(sf$results$Accuracy))
})

test_that("sbf can score all the predictors at once", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  cls <- engine_two_class(80)
  x <- cls[, names(cls) != "Class"]

  # a multivariate score function is handed the whole predictor set
  multi <- ldaSBF
  multi$score <- function(x, y) {
    vapply(x, function(col) anovaScores(col, y), double(1))
  }

  set.seed(1155)
  sf <- suppressWarnings(sbf(
    x,
    cls$Class,
    sbfControl = sbfControl(
      functions = multi,
      method = "cv",
      number = 3,
      multivariate = TRUE
    )
  ))
  expect_s3_class(sf, "sbf")

  # and it has to return one score per predictor
  wrong <- multi
  wrong$score <- function(x, y) 1
  expect_error(
    sbf(
      x,
      cls$Class,
      sbfControl = sbfControl(
        functions = wrong,
        method = "cv",
        number = 3,
        multivariate = TRUE
      )
    ),
    "should return a vector with"
  )
})

test_that("sbf checks the seeds it is given and can make its own", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  cls <- engine_two_class(60)
  x <- cls[, names(cls) != "Class"]
  folds <- createFolds(cls$Class, k = 3, returnTrain = TRUE)

  # one seed per resample plus one for the final fit
  set.seed(9081)
  sf <- suppressWarnings(sbf(
    x,
    cls$Class,
    sbfControl = sbfControl(
      functions = ldaSBF,
      method = "cv",
      index = folds,
      seeds = 1:4
    )
  ))
  expect_length(sf$control$seeds, 4)

  expect_snapshot(
    sbf(
      x,
      cls$Class,
      sbfControl = sbfControl(
        functions = ldaSBF,
        method = "cv",
        index = folds,
        seeds = 1:2
      )
    ),
    error = TRUE
  )
})

test_that("sbf times its predictions when asked", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  cls <- engine_two_class(60)
  set.seed(5528)
  sf <- suppressWarnings(sbf(
    cls[, names(cls) != "Class"],
    cls$Class,
    sbfControl = sbfControl(
      functions = ldaSBF,
      method = "cv",
      number = 3,
      timingSamps = 5
    )
  ))
  expect_in("prediction", names(sf$times))
})

test_that("sbf uses a performance-var role from the recipe", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  cls <- engine_two_class(60)
  cls$extra <- seq_len(nrow(cls))
  rec <- recipes::recipe(Class ~ ., data = cls)
  rec <- recipes::update_role(rec, extra, new_role = "performance var")

  saw_extra <- ldaSBF
  saw_extra$summary <- function(data, lev = NULL, model = NULL) {
    c(
      Accuracy = mean(data$obs == data$pred),
      HasExtra = as.numeric("extra" %in% names(data))
    )
  }

  set.seed(2263)
  sf <- suppressWarnings(sbf(
    rec,
    data = cls,
    sbfControl = sbfControl(functions = saw_extra, method = "cv", number = 3)
  ))
  expect_all_equal(sf$results$HasExtra, 1)
  expect_disjoint(sf$optVariables, "extra")
})

# ------------------------------------------------------------------------------
# methods on a fitted object

test_that("predict.sbf prepares new data for a formula fit", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  cls <- engine_two_class(60)
  set.seed(7521)
  sf <- suppressWarnings(sbf(
    Class ~ .,
    data = cls,
    sbfControl = sbfControl(functions = ldaSBF, method = "cv", number = 3)
  ))
  # the formula interface remembers its terms and applies them to new data
  expect_identical(nrow(predict(sf, cls)), nrow(cls))
})

test_that("varImp.sbf reports the retained variables' scores", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  cls <- engine_two_class(60)
  set.seed(4198)
  sf <- suppressWarnings(sbf(
    cls[, names(cls) != "Class"],
    cls$Class,
    sbfControl = sbfControl(functions = ldaSBF, method = "cv", number = 3)
  ))
  vi <- varImp(sf)
  expect_s3_class(vi, "data.frame")
  expect_setequal(rownames(vi), sf$optVariables)
})

test_that("the sbf resampling plots draw and refuse leave-one-out results", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  cls <- engine_two_class(60)
  set.seed(2823)
  sf <- suppressWarnings(sbf(
    cls[, names(cls) != "Class"],
    cls$Class,
    sbfControl = sbfControl(
      functions = ldaSBF,
      method = "cv",
      number = 3,
      returnResamp = "all"
    )
  ))

  for (f in list(densityplot, histogram)) {
    drawn <- f(sf)
    expect_s3_class(drawn, "trellis")
    draw_trellis(drawn)
  }

  # a single held-out row per resample leaves no distribution to plot
  small <- engine_two_class(20)
  set.seed(6031)
  loo <- suppressWarnings(sbf(
    small[, names(small) != "Class"],
    small$Class,
    sbfControl = sbfControl(functions = ldaSBF, method = "LOOCV")
  ))
  expect_snapshot(densityplot(loo), error = TRUE)
  expect_snapshot(histogram(loo), error = TRUE)
})

test_that("gamScores scores a predictor with a smooth term", {
  skip_on_cran()
  skip_if_not_installed("gam")

  cls <- engine_two_class(60)
  # gamScores scores one predictor at a time, as the other score functions do,
  # and returns the p-value of the smooth term
  score <- vapply(
    c("TwoFactor1", "TwoFactor2"),
    function(v) gamScores(cls[[v]], cls$Class),
    double(1)
  )
  expect_length(score, 2)
  expect_all_true(score >= 0 & score <= 1)
})
