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
  expect_true(is.na(out$Overall[3]))
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
  expect_output(print(rf), "Outer resampling")

  # update() refits the final model at a chosen size
  up <- suppressWarnings(update(rf, x = dat[, 1:8], y = dat$Class, size = 4))
  expect_s3_class(up, "rfe")
})

test_that("rfe works with a recipe", {
  skip_on_cran()
  skip_if_not_installed("recipes")

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
  expect_true(all(rf$optVariables %in% colnames(dat)))
})
