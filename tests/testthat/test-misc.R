test_that("R2 and RMSE are calculating correctly", {
  skip_on_cran()
  pred <- runif(25)
  obs <- runif(25)

  expect_equal(R2(pred, obs), cor(obs, pred)^2)
  expect_equal(RMSE(pred, obs), sqrt(mean((pred - obs)^2)))
})


test_that("auc calculation is > .5 when Xs provide prediction", {
  skip_on_cran()
  skip_if_not_installed("MLmetrics")
  skip_if_not_installed("earth")
  skip_if_not_installed("mda")

  suppressPackageStartupMessages(library(earth))

  trCntlListMulti <-
    trainControl(
      method = "cv",
      number = 3,
      verboseIter = FALSE,
      classProbs = TRUE,
      summaryFunction = multiClassSummary
    )

  set.seed(3453)
  knnFit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    trControl = trCntlListMulti
  )

  expect_true(all(knnFit$resample$AUC > 0.5))

  set.seed(7686)
  tr_dat <- twoClassSim(200)
  te_dat <- tr_dat
  tr_dat$Class = factor(tr_dat$Class, levels = rev(levels(te_dat$Class)))

  modle <- train(
    Class ~ .,
    data = te_dat,
    method = "fda",
    tuneLength = 10,
    metric = "ROC",
    trControl = trainControl(
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    )
  )

  expect_true(all(modle$resample$AUC > 0.5))
})

# ------------------------------------------------------------------------------
# Small internal helpers (pure functions, no model fitting)

test_that("MAE returns the mean absolute error", {
  # |1-1| + |2-2| + |3-5| = 2, over 3 points. Computed metric, so expect_equal
  # (the result isn't bit-identical to the literal 2/3).
  expect_equal(MAE(c(1, 2, 3), c(1, 2, 5)), 2 / 3)
})

test_that("R2 supports the traditional formula", {
  # perfect predictions -> R^2 of 1 (computed metric -> tolerant comparison)
  expect_equal(R2(c(1, 2, 3), c(1, 2, 3), formula = "traditional"), 1)
})

test_that("well_numbered builds zero-padded, sortable names", {
  expect_identical(
    caret:::well_numbered("Model", 3),
    c("Model1", "Model2", "Model3")
  )
  # padding kicks in once the width grows, so names still sort correctly
  padded <- caret:::well_numbered("Model", 10)
  expect_identical(padded[c(1, 10)], c("Model01", "Model10"))
})

test_that("prettySeq labels resamples", {
  expect_identical(
    caret:::prettySeq(1:3),
    c("Resample1", "Resample2", "Resample3")
  )
  expect_identical(caret:::prettySeq(1:10)[10], "Resample10")
})

test_that("flatTable flattens a confusion table into named cells", {
  ft <- caret:::flatTable(factor(c("a", "b", "a")), factor(c("a", "b", "b")))
  expect_identical(names(ft), paste0(".cell", 1:4))
  # column-major counts: [a,a]=1, [b,a]=0, [a,b]=1, [b,b]=1
  expect_identical(unname(ft), c(1L, 0L, 1L, 1L))
})

test_that("splitIndicies splits evenly when it can", {
  # 6 items into 3 groups -> two of each, in order
  expect_identical(caret:::splitIndicies(6, 3), c(1L, 1L, 2L, 2L, 3L, 3L))
  # with a remainder, every index is still a valid group label
  si <- caret:::splitIndicies(7, 3)
  expect_length(si, 7)
  expect_true(all(si %in% 1:3))
})

test_that("repList makes repeated copies, optionally indexed", {
  out <- caret:::repList(list(z = 1), times = 3, addIndex = TRUE)
  expect_length(out, 3)
  expect_identical(out[[2]]$z, 1)
  expect_identical(out[[2]]$.index, 2L)
})

test_that("useMathSymbols swaps in a plotmath R-squared", {
  expect_identical(caret:::useMathSymbols("Rsquared"), expression(R^2))
  # anything else is returned unchanged
  expect_identical(caret:::useMathSymbols("Accuracy"), "Accuracy")
})

test_that("model2method maps object classes to train methods", {
  expect_identical(caret:::model2method("randomForest"), "rf")
  expect_identical(caret:::model2method("rvm"), "rvmRadial")
  expect_identical(caret:::model2method("ksvm"), "svmRadial")
  expect_identical(caret:::model2method("lssvm"), "lssvmRadial")
  expect_identical(caret:::model2method("gausspr"), "gaussprRadial")
  expect_identical(caret:::model2method("NaiveBayes"), "nb")
  # both bagging classes map to treebag
  expect_identical(caret:::model2method("classbagg"), "treebag")
  expect_identical(caret:::model2method("regbagg"), "treebag")
  expect_identical(caret:::model2method("plsda"), "pls")
  expect_identical(caret:::model2method("pamrtrained"), "pam")
  # an unknown class passes through unchanged
  expect_identical(caret:::model2method("zzz"), "zzz")
})

test_that("get_model_type distinguishes regression from classification", {
  expect_identical(caret:::get_model_type(1:5), "Regression")
  expect_identical(
    caret:::get_model_type(factor(letters[1:3])),
    "Classification"
  )
})

test_that("get_range is NA for factors and a widened range otherwise", {
  expect_identical(caret:::get_range(factor(letters)), NA)
  # get_range delegates to extendrange for numerics, so it matches exactly
  expect_identical(caret:::get_range(c(0, 10)), extendrange(c(0, 10)))
})

test_that("outcome_conversion coerces to a factor with the given levels", {
  out <- caret:::outcome_conversion(c("a", "b"), lv = c("a", "b", "c"))
  expect_s3_class(out, "factor")
  expect_identical(levels(out), c("a", "b", "c"))
  # an ordered set of levels yields an ordered factor
  ord <- caret:::outcome_conversion(
    c("a", "b"),
    lv = structure(c("a", "b", "c"), ordered = TRUE)
  )
  expect_s3_class(ord, "ordered")
  # numeric input is left alone
  expect_identical(
    caret:::outcome_conversion(c(1.5, 2.5), lv = NULL),
    c(1.5, 2.5)
  )
})

test_that("subset_x keeps two dimensions when subsetting rows", {
  m <- matrix(1:12, nrow = 4)
  expect_identical(dim(caret:::subset_x(m, c(1, 3))), c(2L, 3L))
  df <- data.frame(a = 1:4, b = letters[1:4])
  expect_identical(nrow(caret:::subset_x(df, c(2, 4))), 2L)
})

test_that("var_seq builds a default grid of predictor counts", {
  expect_identical(
    caret:::var_seq(10, classification = FALSE, len = 3),
    c(2, 6, 10)
  )
  # length-one grids: floor(sqrt(p)) for classification, floor(p/3) otherwise
  expect_identical(caret:::var_seq(16, classification = TRUE, len = 1), 4)
  expect_identical(caret:::var_seq(9, classification = FALSE, len = 1), 3)
  # a tiny p collapses to duplicate counts, which are dropped with a note
  expect_snapshot(vs <- caret:::var_seq(2, classification = FALSE, len = 3))
  expect_identical(vs, 2)
})

test_that("check_dims requires x rows to match the outcome length", {
  m <- matrix(1:12, nrow = 4)
  expect_null(caret:::check_dims(m, 1:4))
  expect_snapshot(caret:::check_dims(m, 1:3), error = TRUE)
  expect_snapshot(caret:::check_dims(matrix(1:3, nrow = 1), 1), error = TRUE)
})
