# Tests for yardstickSummary() and is_minimized() in R/yardstick.R.

make_class_data <- function(n = 40, classes = c("a", "b")) {
  set.seed(5094)
  dat <- data.frame(
    obs = factor(sample(classes, n, replace = TRUE), levels = classes),
    pred = factor(sample(classes, n, replace = TRUE), levels = classes)
  )
  probs <- matrix(runif(n * length(classes)), ncol = length(classes))
  probs <- probs / rowSums(probs)
  colnames(probs) <- classes
  cbind(dat, probs)
}

# --- yardstickSummary ---------------------------------------------------------

test_that("yardstickSummary computes regression metrics", {
  set.seed(7731)
  dat <- data.frame(obs = rnorm(30))
  dat$pred <- dat$obs + rnorm(30, sd = 0.3)
  summ <- yardstickSummary(yardstick::metric_set(
    yardstick::rmse,
    yardstick::ccc
  ))
  res <- summ(dat)
  expect_named(res, c("rmse", "ccc"))
  expect_equal(res[["rmse"]], yardstick::rmse_vec(dat$obs, dat$pred))
  expect_equal(res[["ccc"]], yardstick::ccc_vec(dat$obs, dat$pred))
})

test_that("yardstickSummary uses the first level as the event", {
  dat <- make_class_data()
  summ <- yardstickSummary(yardstick::metric_set(
    yardstick::mcc,
    yardstick::roc_auc,
    yardstick::brier_class
  ))
  res <- summ(dat, lev = c("a", "b"))
  expect_named(res, c("mcc", "roc_auc", "brier_class"))
  expect_equal(res[["roc_auc"]], yardstick::roc_auc_vec(dat$obs, dat$a))
  expect_equal(res[["mcc"]], yardstick::mcc_vec(dat$obs, dat$pred))
})

test_that("yardstickSummary handles more than two classes", {
  classes <- c("a", "b", "c")
  dat <- make_class_data(classes = classes)
  summ <- yardstickSummary(yardstick::metric_set(
    yardstick::accuracy,
    yardstick::mn_log_loss
  ))
  res <- summ(dat, lev = classes)
  expect_equal(
    res[["mn_log_loss"]],
    yardstick::mn_log_loss_vec(dat$obs, as.matrix(dat[, classes]))
  )
  expect_equal(unname(res["mn_log_loss"]), unname(mnLogLoss(dat, classes)))
})

test_that("yardstickSummary does not need probabilities for class metrics", {
  dat <- make_class_data()[, c("obs", "pred")]
  summ <- yardstickSummary(yardstick::metric_set(yardstick::kap))
  expect_named(summ(dat, lev = c("a", "b")), "kap")
})

test_that("yardstickSummary uses case weights", {
  set.seed(1406)
  dat <- data.frame(obs = rnorm(30), weights = runif(30))
  dat$pred <- dat$obs + rnorm(30)
  summ <- yardstickSummary(yardstick::metric_set(yardstick::mae))
  expect_equal(
    summ(dat)[["mae"]],
    yardstick::mae_vec(dat$obs, dat$pred, case_weights = dat$weights)
  )
})

test_that("yardstickSummary stores the metric directions", {
  summ <- yardstickSummary(yardstick::metric_set(
    yardstick::rmse,
    yardstick::rsq
  ))
  expect_identical(
    attr(summ, "metric_directions"),
    c(rmse = "minimize", rsq = "maximize")
  )
})

test_that("yardstickSummary gives informative errors", {
  expect_snapshot(error = TRUE, yardstickSummary(yardstick::rmse))

  dat <- make_class_data()
  reg_summ <- yardstickSummary(yardstick::metric_set(yardstick::rmse))
  expect_snapshot(error = TRUE, reg_summ(dat, lev = c("a", "b")))

  cls_summ <- yardstickSummary(yardstick::metric_set(yardstick::roc_auc))
  expect_snapshot(error = TRUE, cls_summ(dat[, c("obs", "pred")], c("a", "b")))

  num_dat <- data.frame(obs = 1:5, pred = 1:5)
  expect_snapshot(error = TRUE, cls_summ(num_dat))
})

# --- is_minimized ---------------------------------------------------------------

test_that("is_minimized knows caret's metric names", {
  expect_identical(
    is_minimized(c("RMSE", "MAE", "logLoss", "Rsquared", "Accuracy", "ROC")),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_identical(
    is_minimized(c("Mean_F1", "prAUC", "Detection_Rate", "Spec")),
    c(FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("every caret metric name maps to a yardstick metric", {
  directions <- vapply(caret_yardstick_names, yardstick_direction, character(1))
  expect_all_true(directions %in% c("maximize", "minimize"))
})

test_that("is_minimized uses yardstick metric directions", {
  expect_identical(
    is_minimized(c("brier_class", "huber_loss", "mcc", "roc_auc")),
    c(TRUE, TRUE, FALSE, FALSE)
  )
  expect_false(is_minimized("not_a_metric"))
})

test_that("is_minimized prefers directions stored by yardstickSummary", {
  summ <- function(data, lev = NULL, model = NULL) NULL
  attr(summ, "metric_directions") <- c(custom = "minimize")
  expect_true(is_minimized("custom", summ))
  expect_false(is_minimized("custom"))
})

test_that("is_minimized warns about metrics that are optimal at zero", {
  expect_snapshot_warning(is_minimized("msd"))
})

# --- train() ------------------------------------------------------------------

test_that("train() minimizes yardstick loss metrics by default", {
  skip_on_cran()
  set.seed(2290)
  dat <- SLC14_1(100)
  ctrl <- trainControl(
    method = "cv",
    number = 3,
    summaryFunction = yardstickSummary(yardstick::metric_set(
      yardstick::rmse,
      yardstick::huber_loss
    ))
  )
  fit <- train(
    y ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = c(1, 5, 15)),
    metric = "huber_loss",
    trControl = ctrl
  )
  expect_false(fit$maximize)
  expect_named(
    fit$results,
    c("k", "rmse", "huber_loss", "rmseSD", "huber_lossSD")
  )
  expect_identical(
    fit$bestTune$k,
    fit$results$k[which.min(fit$results$huber_loss)]
  )
})

test_that("train() can use yardstick probability metrics", {
  skip_on_cran()
  set.seed(9147)
  dat <- twoClassSim(100)
  ctrl <- trainControl(
    method = "cv",
    number = 3,
    classProbs = TRUE,
    summaryFunction = yardstickSummary(yardstick::metric_set(
      yardstick::mcc,
      yardstick::brier_class
    ))
  )
  fit <- train(
    Class ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 9, 21)),
    metric = "brier_class",
    trControl = ctrl
  )
  expect_false(fit$maximize)
  expect_identical(
    fit$bestTune$k,
    fit$results$k[which.min(fit$results$brier_class)]
  )
})
