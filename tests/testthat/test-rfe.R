# Tests for recursive feature elimination. The pure helpers (size/variable
# pickers, rfeControl, repair_rank) are unit-tested directly; the rfe() workflow
# and its methods are exercised by small integration fits at the bottom.

# ------------------------------------------------------------------------------
# size / variable pickers

test_that("pickSizeBest returns the smallest size at the best metric", {
  perf_max <- data.frame(
    Variables = c(1, 2, 3, 4),
    Accuracy = c(0.8, 0.9, 0.88, 0.9)
  )
  # 0.9 is the best; the smaller size (2) wins the tie
  expect_identical(
    caret:::pickSizeBest(perf_max, "Accuracy", maximize = TRUE),
    2
  )

  perf_min <- data.frame(Variables = c(1, 2, 3, 4), RMSE = c(3, 1, 1, 2))
  expect_identical(caret:::pickSizeBest(perf_min, "RMSE", maximize = FALSE), 2)
})

test_that("pickSizeTolerance picks the smallest size within tolerance", {
  perf <- data.frame(
    Variables = c(1, 2, 3, 4),
    Accuracy = c(0.8, 0.9, 0.88, 0.91)
  )
  # within 1.5% of the best (0.91): sizes 2 and 4; the smaller one wins
  expect_identical(
    caret:::pickSizeTolerance(perf, "Accuracy", tol = 1.5, maximize = TRUE),
    2
  )

  perf_min <- data.frame(Variables = c(1, 2, 3, 4), RMSE = c(3, 1, 1, 2))
  expect_identical(
    caret:::pickSizeTolerance(perf_min, "RMSE", tol = 10, maximize = FALSE),
    2
  )
})

test_that("pickVars returns the top-ranked variables by mean importance", {
  imp <- data.frame(
    var = c("a", "b", "c", "a", "b", "c"),
    Overall = c(10, 5, 1, 10, 5, 1),
    stringsAsFactors = FALSE
  )
  expect_identical(caret:::pickVars(imp, size = 2), c("a", "b"))
})

test_that("repair_rank fills in variables that are missing from the ranking", {
  imp <- data.frame(
    var = c("a", "b"),
    Overall = c(1, 2),
    stringsAsFactors = FALSE
  )
  out <- caret:::repair_rank(imp, nms = c("a", "b", "c"))
  expect_identical(nrow(out), 3L)
  expect_identical(out$var[3], "c")
  expect_identical(out$Overall[3], NA_real_)
})

# ------------------------------------------------------------------------------
# rfeControl

test_that("rfeControl fills in sensible defaults", {
  ctrl <- rfeControl()
  expect_identical(ctrl$method, "boot")
  expect_identical(ctrl$number, 25)
  # with no functions supplied it defaults to caretFuncs
  expect_identical(ctrl$functions, caretFuncs)
  # cv switches the default resample count to 10
  expect_identical(rfeControl(method = "cv")$number, 10)
})

# ------------------------------------------------------------------------------
# rfe() workflow + methods

test_that("rfe runs and its methods behave (default interface)", {
  skip_on_cran()

  set.seed(1)
  dat <- twoClassSim(150)
  set.seed(1)
  rf <- rfe(
    dat[, 1:8],
    dat$Class,
    sizes = c(2, 4, 6),
    rfeControl = rfeControl(functions = lrFuncs, method = "cv", number = 3)
  )

  expect_s3_class(rf, "rfe")
  # the chosen variables match the reported optimal size
  expect_length(rf$optVariables, rf$optsize)
  expect_identical(predictors(rf), rf$optVariables)
  # predict returns one row per new observation
  expect_identical(nrow(predict(rf, dat[, 1:8])), nrow(dat))
  expect_s3_class(varImp(rf), "data.frame")
  expect_snapshot(print(rf))

  # update() refits the final model at a chosen size
  up <- suppressWarnings(update(rf, x = dat[, 1:8], y = dat$Class, size = 4))
  expect_s3_class(up, "rfe")
})

test_that("rfe works with a recipe", {
  skip_on_cran()

  set.seed(1)
  full <- twoClassSim(150)
  dat <- full[, c(names(full)[1:8], "Class")]
  rec <- recipes::recipe(Class ~ ., data = dat)

  set.seed(1)
  rf <- rfe(
    rec,
    data = dat,
    sizes = c(2, 4),
    rfeControl = rfeControl(functions = lrFuncs, method = "cv", number = 3)
  )

  expect_s3_class(rf, "rfe")
  expect_length(rf$optVariables, rf$optsize)
})

test_that("rfe works with the formula interface", {
  skip_on_cran()

  set.seed(1)
  full <- twoClassSim(150)
  dat <- full[, c(names(full)[1:8], "Class")]
  set.seed(1)
  rf <- rfe(
    Class ~ .,
    data = dat,
    rfeControl = rfeControl(functions = lrFuncs, method = "cv", number = 3)
  )

  expect_s3_class(rf, "rfe")
  expect_in(rf$optVariables, colnames(dat))
})

# ------------------------------------------------------------------------------
# rfeIter

test_that("rfeIter validates its arguments", {
  x <- as.matrix(engine_regression(20)[, 1:3])
  y <- engine_regression(20)$y

  unnamed <- x
  colnames(unnamed) <- NULL
  expect_snapshot(
    rfeIter(unnamed, y, testX = x, testY = y, sizes = 2),
    error = TRUE
  )
  # the held-out data is what each subset is scored on
  expect_snapshot(
    rfeIter(x, y, testX = NULL, testY = y, sizes = 2),
    error = TRUE
  )
  expect_snapshot(
    rfeIter(x, y, testX = x, testY = y, sizes = NULL),
    error = TRUE
  )
})

test_that("rfeIter ranks and scores one subset sequence", {
  skip_on_cran()

  reg <- engine_regression(60)
  train_rows <- 1:40
  ctrl <- rfeControl(functions = lmFuncs, method = "cv", number = 3)

  set.seed(3388)
  out <- rfeIter(
    x = reg[train_rows, 1:3],
    y = reg$y[train_rows],
    testX = reg[-train_rows, 1:3],
    testY = reg$y[-train_rows],
    sizes = c(1, 2),
    rfeControl = ctrl
  )
  expect_named(out, c("finalVariables", "pred"))
  # one entry per size, plus the full set
  expect_length(out$finalVariables, 3)
  # the predictions cover every held-out row at each size
  expect_identical(nrow(out$pred), 3L * length(reg$y[-train_rows]))
  expect_contains(names(out$pred), c("pred", "obs", "Variables"))
})

test_that("rfeIter can keep the ranking from the full model", {
  skip_on_cran()

  reg <- engine_regression(60)
  train_rows <- 1:40
  # rerank = TRUE re-ranks the variables after each elimination
  ctrl <- rfeControl(
    functions = lmFuncs,
    method = "cv",
    number = 3,
    rerank = TRUE
  )

  set.seed(9741)
  out <- rfeIter(
    x = reg[train_rows, 1:3],
    y = reg$y[train_rows],
    testX = reg[-train_rows, 1:3],
    testY = reg$y[-train_rows],
    sizes = c(1, 2),
    rfeControl = ctrl
  )
  expect_length(out$finalVariables, 3)
})

# ------------------------------------------------------------------------------
# the recipe interface

test_that("rfe drives a recipe through several resampling methods", {
  skip_on_cran()

  reg <- engine_regression(60)
  rec <- recipes::recipe(y ~ ., data = reg)
  rec <- recipes::step_normalize(rec, recipes::all_predictors())

  for (m in c("cv", "boot", "boot632")) {
    set.seed(1148)
    rf <- rfe(
      rec,
      data = reg,
      sizes = c(1, 2),
      rfeControl = rfeControl(functions = lmFuncs, method = m, number = 3)
    )
    expect_s3_class(rf, "rfe")
    expect_identical(rf$control$method, m)
    expect_in(rf$optsize, c(1, 2, 3))
    expect_all_false(is.na(rf$results$RMSE))
  }
})

test_that("rfe drives a recipe through leave-one-out resampling", {
  skip_on_cran()

  small <- engine_regression(16)
  rec <- recipes::recipe(y ~ ., data = small)

  set.seed(5017)
  rf <- rfe(
    rec,
    data = small,
    sizes = c(1, 2),
    rfeControl = rfeControl(functions = lmFuncs, method = "LOOCV")
  )
  expect_identical(rf$control$method, "LOOCV")
  expect_all_false(is.na(rf$results$RMSE))
  # one pooled estimate per subset size, the full set included
  expect_identical(nrow(rf$results), 3L)
})

test_that("rfe refuses a case-weight role", {
  skip_on_cran()

  reg <- engine_regression(40)
  reg$wt <- rep(c(1, 2), length.out = nrow(reg))
  rec <- recipes::recipe(y ~ ., data = reg)
  rec <- recipes::update_role(rec, wt, new_role = "case weight")

  # unlike train(), rfe() has nowhere to put case weights and says so
  expect_snapshot(
    rfe(
      rec,
      data = reg,
      sizes = c(1, 2),
      rfeControl = rfeControl(functions = lmFuncs, method = "cv", number = 3)
    ),
    error = TRUE
  )
})

test_that("rfe passes performance variables to the summary function", {
  skip_on_cran()

  reg <- engine_regression(60)
  reg$extra <- seq_len(nrow(reg))
  rec <- recipes::recipe(y ~ ., data = reg)
  rec <- recipes::update_role(rec, extra, new_role = "performance var")

  # the summary function sees the extra column alongside the predictions
  perf_funcs <- lmFuncs
  perf_funcs$summary <- function(data, lev = NULL, model = NULL) {
    c(
      RMSE = sqrt(mean((data$obs - data$pred)^2)),
      HasExtra = as.numeric("extra" %in% names(data))
    )
  }

  set.seed(2263)
  rf <- rfe(
    rec,
    data = reg,
    sizes = c(1, 2),
    metric = "RMSE",
    maximize = FALSE,
    rfeControl = rfeControl(functions = perf_funcs, method = "cv", number = 3)
  )
  expect_all_equal(rf$results$HasExtra, 1)
  # the extra column is not a candidate predictor
  expect_disjoint(rf$optVariables, "extra")
})

test_that("rfe classifies from a recipe with class probabilities", {
  skip_on_cran()

  # logistic regression separates on a small sample of this simulation, so use
  # enough rows for glm to converge quietly
  cls <- engine_two_class(200)
  rec <- recipes::recipe(Class ~ ., data = cls)

  # the built-in functions summarise with defaultSummary, so ROC needs both a
  # summary function that computes it and the probabilities to compute it from
  roc_funcs <- lrFuncs
  roc_funcs$summary <- twoClassSummary

  set.seed(7739)
  rf <- rfe(
    rec,
    data = cls,
    sizes = c(2, 4),
    metric = "ROC",
    rfeControl = rfeControl(
      functions = roc_funcs,
      method = "cv",
      number = 3,
      returnResamp = "all"
    )
  )
  expect_contains(names(rf$results), "ROC")
  expect_all_false(is.na(rf$results$ROC))
})

# ------------------------------------------------------------------------------
# plots and predictions

test_that("plot.rfe draws the performance profile", {
  skip_on_cran()

  reg <- engine_regression(60)
  set.seed(5744)
  rf <- rfe(
    reg[, 1:3],
    reg$y,
    sizes = c(1, 2),
    rfeControl = rfeControl(functions = lmFuncs, method = "cv", number = 3)
  )

  drawn <- plot(rf)
  expect_s3_class(drawn, "trellis")
  # the chosen size is marked, which the panel function draws
  draw_trellis(drawn)

  # another metric can be asked for
  draw_trellis(plot(rf, metric = "Rsquared"))
})

test_that("the rfe resampling plots need saved resamples", {
  skip_on_cran()

  reg <- engine_regression(60)
  set.seed(2823)
  rf <- rfe(
    reg[, 1:3],
    reg$y,
    sizes = c(1, 2),
    rfeControl = rfeControl(
      functions = lmFuncs,
      method = "cv",
      number = 3,
      returnResamp = "all"
    )
  )

  # each lattice method draws the distribution of the resampled metric
  for (f in list(xyplot, stripplot, densityplot, histogram)) {
    drawn <- f(rf)
    expect_s3_class(drawn, "trellis")
    draw_trellis(drawn)
  }

  # `data` belongs to the object, so supplying it is ignored with a warning
  expect_snapshot_warning(xyplot(rf, data = reg))
})

test_that("the rfe resampling plots refuse leave-one-out results", {
  skip_on_cran()

  small <- engine_regression(16)
  set.seed(6031)
  rf <- rfe(
    small[, 1:3],
    small$y,
    sizes = c(1, 2),
    rfeControl = rfeControl(functions = lmFuncs, method = "LOOCV")
  )

  # a single held-out row per resample leaves nothing to plot a distribution of
  expect_snapshot(xyplot(rf), error = TRUE)
  expect_snapshot(stripplot(rf), error = TRUE)
  expect_snapshot(densityplot(rf), error = TRUE)
  expect_snapshot(histogram(rf), error = TRUE)
})

test_that("predict.rfe returns classes and probabilities", {
  skip_on_cran()

  cls <- engine_two_class(200)
  set.seed(4198)
  rf <- rfe(
    cls[, names(cls) != "Class"],
    cls$Class,
    sizes = c(2, 4),
    rfeControl = rfeControl(functions = lrFuncs, method = "cv", number = 3)
  )

  out <- predict(rf, cls[, names(cls) != "Class"])
  # the built-in logistic functions return the class and its probabilities
  expect_identical(nrow(out), nrow(cls))
  expect_contains(names(out), "pred")
})

test_that("predict.rfe works from the formula interface", {
  skip_on_cran()

  reg <- engine_regression(60)
  set.seed(7521)
  rf <- rfe(
    y ~ .,
    data = reg,
    sizes = c(1, 2),
    rfeControl = rfeControl(functions = lmFuncs, method = "cv", number = 3)
  )

  # the formula interface remembers its terms, so new data is prepared the same
  # way it was for fitting
  expect_length(predict(rf, reg), nrow(reg))
  expect_s3_class(rf, "rfe.formula")
})

# ------------------------------------------------------------------------------
# seeds, timing and progress

test_that("rfe checks the seeds it is given", {
  skip_on_cran()

  reg <- engine_regression(40)
  folds <- createFolds(reg$y, k = 3, returnTrain = TRUE)

  # one integer vector per resample, each as long as the number of subset sizes
  # plus the full set, and a single integer for the final fit
  good <- c(lapply(1:3, function(i) 1:3), list(1L))
  set.seed(7712)
  fit <- rfe(
    reg[, 1:3],
    reg$y,
    sizes = c(1, 2),
    rfeControl = rfeControl(
      functions = lmFuncs,
      method = "cv",
      index = folds,
      seeds = good
    )
  )
  expect_s3_class(fit, "rfe")

  # too few vectors, and vectors that are too short, are both refused
  expect_snapshot(
    rfe(
      reg[, 1:3],
      reg$y,
      sizes = c(1, 2),
      rfeControl = rfeControl(
        functions = lmFuncs,
        method = "cv",
        index = folds,
        seeds = good[1:2]
      )
    ),
    error = TRUE
  )
  expect_snapshot(
    rfe(
      reg[, 1:3],
      reg$y,
      sizes = c(1, 2),
      rfeControl = rfeControl(
        functions = lmFuncs,
        method = "cv",
        index = folds,
        seeds = c(lapply(1:3, function(i) 1L), list(1L))
      )
    ),
    error = TRUE
  )
})

test_that("rfe times its predictions when asked", {
  skip_on_cran()

  reg <- engine_regression(40)
  set.seed(5528)
  fit <- rfe(
    reg[, 1:3],
    reg$y,
    sizes = c(1, 2),
    rfeControl = rfeControl(
      functions = lmFuncs,
      method = "cv",
      number = 3,
      timingSamps = 5
    )
  )
  # the prediction time is recorded alongside the fitting time
  expect_in("prediction", names(fit$times))
  expect_s3_class(fit$times$prediction, "proc_time")
})

test_that("rfe reports its progress", {
  skip_on_cran()

  reg <- engine_regression(40)
  set.seed(3390)
  # the fitting, importance and elimination steps each announce themselves
  expect_snapshot(
    fit <- rfe(
      reg[, 1:3],
      reg$y,
      sizes = c(1, 2),
      rfeControl = rfeControl(
        functions = lmFuncs,
        method = "cv",
        number = 2,
        verbose = TRUE
      )
    )
  )
  expect_s3_class(fit, "rfe")
})

test_that("rfe reports its progress for a recipe", {
  skip_on_cran()

  reg <- engine_regression(40)
  rec <- recipes::recipe(y ~ ., data = reg)
  set.seed(3390)
  expect_snapshot(
    fit <- rfe(
      rec,
      data = reg,
      sizes = c(1, 2),
      rfeControl = rfeControl(
        functions = lmFuncs,
        method = "cv",
        number = 2,
        verbose = TRUE
      )
    )
  )
  expect_s3_class(fit, "rfe")
})

# ------------------------------------------------------------------------------
# what a recipe leaves to select from

test_that("rfe needs at least two predictors after the recipe", {
  skip_on_cran()

  reg <- engine_regression(40)
  # the recipe reduces the predictors to one, which leaves nothing to eliminate
  rec <- recipes::recipe(y ~ ., data = reg)
  rec <- recipes::step_rm(rec, x2, x3)

  # Not snapshotted: the error is raised inside the resampling loop, and foreach
  # re-raises it with the whole loop body as the call, which covr rewrites when
  # it instruments the package.
  expect_error(
    rfe(
      rec,
      data = reg,
      sizes = 1,
      rfeControl = rfeControl(functions = lmFuncs, method = "cv", number = 3)
    ),
    "less than two predictors remaining"
  )
})

test_that("rfe checks the sizes against what the recipe produces", {
  skip_on_cran()

  reg <- engine_regression(40)
  rec <- recipes::recipe(y ~ ., data = reg)
  rec <- recipes::step_rm(rec, x3)

  # Asking only for sizes larger than the two remaining predictors leaves the
  # search with nothing to do. The recipe also warns that it will truncate the
  # subset sizes, which is captured here. As above, the error is not snapshotted
  # because it carries the resampling loop's body as its call.
  expect_error(
    suppressWarnings(
      rfe(
        rec,
        data = reg,
        sizes = c(5, 6),
        rfeControl = rfeControl(functions = lmFuncs, method = "cv", number = 3)
      )
    ),
    "values are inconsistent with this"
  )
})

test_that("predict.rfe needs the variables it selected", {
  skip_on_cran()

  reg <- engine_regression(40)
  set.seed(8830)
  fit <- rfe(
    reg[, 1:3],
    reg$y,
    sizes = c(1, 2),
    rfeControl = rfeControl(functions = lmFuncs, method = "cv", number = 3)
  )

  # drop whichever variables the search settled on, so the message names them
  # however the selection turned out
  without <- reg[, setdiff(names(reg), fit$optVariables), drop = FALSE]
  expect_snapshot(predict(fit, without), error = TRUE)
})

test_that("predict.rfe prepares new data with the recipe", {
  skip_on_cran()

  reg <- engine_regression(40)
  rec <- recipes::recipe(y ~ ., data = reg)
  rec <- recipes::step_normalize(rec, recipes::all_predictors())

  set.seed(1160)
  fit <- rfe(
    rec,
    data = reg,
    sizes = c(1, 2),
    rfeControl = rfeControl(functions = lmFuncs, method = "cv", number = 3)
  )
  # the recipe is applied to the new data before the model sees it
  expect_length(predict(fit, reg), nrow(reg))
})

test_that("rfe applies the .632 correction with a recipe", {
  skip_on_cran()

  reg <- engine_regression(60)
  rec <- recipes::recipe(y ~ ., data = reg)

  # boot632 scores an extra "apparent" pass on the whole training set and
  # weights it against the bootstrap estimates
  set.seed(1148)
  fit <- rfe(
    rec,
    data = reg,
    sizes = c(1, 2),
    rfeControl = rfeControl(functions = lmFuncs, method = "boot632", number = 3)
  )
  expect_identical(fit$control$method, "boot632")
  expect_all_false(is.na(fit$results$RMSE))
  # the apparent pass is kept alongside the bootstrap resamples
  expect_contains(unique(fit$resample$Resample), "AllData")

  # the recipe and x/y interfaces agree, which is what went wrong before: the
  # apparent pass had no rows to score
  set.seed(1148)
  xy <- rfe(
    reg[, 1:3],
    reg$y,
    sizes = c(1, 2),
    rfeControl = rfeControl(functions = lmFuncs, method = "boot632", number = 3)
  )
  expect_equal(fit$results$RMSE, xy$results$RMSE)
})

# ------------------------------------------------------------------------------
# importance values that do not line up with the subset size

test_that("rfe repairs a ranking that is missing variables", {
  skip_on_cran()

  reg <- engine_regression(60)
  # a rank function that drops a variable, as a sparse model would
  short_rank <- lmFuncs
  short_rank$rank <- function(object, x, y) {
    out <- lmFuncs$rank(object, x, y)
    out[-1, , drop = FALSE]
  }

  set.seed(4471)
  expect_snapshot(
    fit <- rfe(
      reg[, 1:3],
      reg$y,
      sizes = c(1, 2),
      rfeControl = rfeControl(functions = short_rank, method = "cv", number = 3)
    )
  )
  # the missing variable is filled in at the bottom of the ranking
  expect_s3_class(fit, "rfe")
  expect_setequal(fit$variables$var, names(reg)[1:3])
})

test_that("rfe generates its own seeds when asked to", {
  skip_on_cran()

  reg <- engine_regression(40)
  # `seeds = NULL` (rather than the default NA) means "make them up"
  set.seed(6693)
  fit <- rfe(
    reg[, 1:3],
    reg$y,
    sizes = c(1, 2),
    rfeControl = rfeControl(
      functions = lmFuncs,
      method = "cv",
      number = 3,
      seeds = NULL
    )
  )
  # one integer vector per resample, plus one for the final fit
  expect_length(fit$control$seeds, 4)
  expect_length(fit$control$seeds[[4]], 1)
})

test_that("rfe reports a model that fails in a resample", {
  skip_on_cran()

  reg <- engine_regression(60)
  # the fit fails for whichever resample holds out the sentinel row
  sentinel <- engine_sentinel_data(60, classification = FALSE)
  failing <- lmFuncs
  failing$fit <- function(x, y, first, last, ...) {
    if (!any(unlist(x, use.names = FALSE) >= 999)) {
      stop("fit failed on purpose", call. = FALSE)
    }
    lm(.outcome ~ ., data = cbind(x, .outcome = y))
  }

  # Not snapshotted: the failure surfaces from inside the resampling loop, so
  # the error carries the loop body as its call and covr rewrites that.
  set.seed(2262)
  expect_error(
    rfe(
      sentinel[, 1:3],
      sentinel$y,
      sizes = c(1, 2),
      rfeControl = rfeControl(functions = failing, method = "cv", number = 3)
    ),
    "fit failed on purpose"
  )
})

test_that("rfe times its predictions from a recipe", {
  skip_on_cran()

  reg <- engine_regression(40)
  rec <- recipes::recipe(y ~ ., data = reg)

  set.seed(5528)
  fit <- rfe(
    rec,
    data = reg,
    sizes = c(1, 2),
    rfeControl = rfeControl(
      functions = lmFuncs,
      method = "cv",
      number = 3,
      timingSamps = 5
    )
  )
  expect_in("prediction", names(fit$times))
})

# ------------------------------------------------------------------------------
# the recipe interface's remaining options

test_that("rfe falls back when the metric is not computed", {
  skip_on_cran()

  reg <- engine_regression(40)
  # a summary function that computes something else entirely
  median_funcs <- lmFuncs
  median_funcs$summary <- function(data, lev = NULL, model = NULL) {
    c(MedianError = median(abs(data$obs - data$pred)))
  }
  ctrl <- rfeControl(functions = median_funcs, method = "cv", number = 3)

  set.seed(2242)
  expect_snapshot_warning(
    xy <- rfe(reg[, 1:3], reg$y, sizes = c(1, 2), rfeControl = ctrl)
  )
  expect_identical(xy$metric, "MedianError")

  # and the same for a recipe
  set.seed(2242)
  expect_snapshot_warning(
    rc <- rfe(
      recipes::recipe(y ~ ., data = reg),
      data = reg,
      sizes = c(1, 2),
      rfeControl = ctrl
    )
  )
  expect_identical(rc$metric, "MedianError")
})

test_that("rfe checks the seeds given for a recipe fit", {
  skip_on_cran()

  reg <- engine_regression(40)
  rec <- recipes::recipe(y ~ ., data = reg)
  folds <- createFolds(reg$y, k = 3, returnTrain = TRUE)

  # one integer vector per resample, each as long as the sizes plus the full
  # set, and a single integer for the final fit
  good <- c(lapply(1:3, function(i) 1:3), list(1L))
  set.seed(7712)
  fit <- rfe(
    rec,
    data = reg,
    sizes = c(1, 2),
    rfeControl = rfeControl(
      functions = lmFuncs,
      method = "cv",
      index = folds,
      seeds = good
    )
  )
  expect_s3_class(fit, "rfe")

  expect_snapshot(
    rfe(
      rec,
      data = reg,
      sizes = c(1, 2),
      rfeControl = rfeControl(
        functions = lmFuncs,
        method = "cv",
        index = folds,
        seeds = good[1:2]
      )
    ),
    error = TRUE
  )

  # and it can generate its own
  set.seed(6693)
  own <- rfe(
    rec,
    data = reg,
    sizes = c(1, 2),
    rfeControl = rfeControl(
      functions = lmFuncs,
      method = "cv",
      number = 3,
      seeds = NULL
    )
  )
  expect_length(own$control$seeds, 4)
})

test_that("rfe reports a recipe fit's progress and repairs its ranking", {
  skip_on_cran()

  reg <- engine_regression(60)
  rec <- recipes::recipe(y ~ ., data = reg)
  # a rank function that drops a variable, as a sparse model would
  short_rank <- lmFuncs
  short_rank$rank <- function(object, x, y) {
    out <- lmFuncs$rank(object, x, y)
    out[-1, , drop = FALSE]
  }

  set.seed(4471)
  expect_snapshot(
    fit <- rfe(
      rec,
      data = reg,
      sizes = c(1, 2),
      rfeControl = rfeControl(
        functions = short_rank,
        method = "cv",
        number = 2,
        verbose = TRUE
      )
    )
  )
  expect_setequal(fit$variables$var, names(reg)[1:3])
})

test_that("rfe uses a performance-var role with leave-one-out resampling", {
  skip_on_cran()

  small <- engine_regression(16)
  small$extra <- seq_len(nrow(small))
  rec <- recipes::recipe(y ~ ., data = small)
  rec <- recipes::update_role(rec, extra, new_role = "performance var")

  perf_funcs <- lmFuncs
  perf_funcs$summary <- function(data, lev = NULL, model = NULL) {
    c(
      RMSE = sqrt(mean((data$obs - data$pred)^2)),
      HasExtra = as.numeric("extra" %in% names(data))
    )
  }

  set.seed(5017)
  fit <- rfe(
    rec,
    data = small,
    sizes = c(1, 2),
    metric = "RMSE",
    maximize = FALSE,
    rfeControl = rfeControl(functions = perf_funcs, method = "LOOCV")
  )
  expect_all_equal(fit$results$HasExtra, 1)
  expect_disjoint(fit$optVariables, "extra")
})

test_that("rfe checks the sizes a recipe leaves for leave-one-out", {
  skip_on_cran()

  small <- engine_regression(16)
  one_left <- recipes::step_rm(
    recipes::recipe(y ~ ., data = small),
    x2,
    x3
  )
  two_left <- recipes::step_rm(recipes::recipe(y ~ ., data = small), x3)

  # the errors come from inside the resampling loop, so they carry the loop body
  # as their call and are matched rather than snapshotted
  expect_error(
    rfe(
      one_left,
      data = small,
      sizes = 1,
      rfeControl = rfeControl(functions = lmFuncs, method = "LOOCV")
    ),
    "less than two predictors remaining"
  )
  expect_error(
    suppressWarnings(rfe(
      two_left,
      data = small,
      sizes = c(5, 6),
      rfeControl = rfeControl(functions = lmFuncs, method = "LOOCV")
    )),
    "values are inconsistent with this"
  )
})
