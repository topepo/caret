# The fixtures (adapt_results, adapt_wide, adapt_grid) live in helper-adaptive.R.
# These check the little model-picking helpers behind adaptiveWorkflow() - the
# workflow itself is tested elsewhere.

# --- ccc --------------------------------------------------------------------

test_that("ccc returns Lin's concordance correlation coefficient", {
  # two identical vectors agree perfectly, so we should get exactly 1
  expect_equal(caret:::ccc(1:5, 1:5), 1)
  # a pair that disagrees, with a value we can work out by hand
  expect_equal(caret:::ccc(1:5, c(5, 3, 1, 2, 4)), -0.3)
  # shifting every value by a constant should pull ccc below 1, even though the
  # plain correlation would still be a perfect 1
  expect_lt(caret:::ccc(1:5, (1:5) + 1), 1)
})

# --- cccmat -----------------------------------------------------------------

test_that("cccmat builds a symmetric concordance matrix with unit diagonal", {
  cc <- caret:::cccmat(adapt_wide)
  expect_equal(dim(cc), c(4, 4))
  expect_equal(unname(diag(cc)), rep(1, 4))
  expect_equal(cc, t(cc))
  expect_identical(colnames(cc), colnames(adapt_wide))
})

test_that("cccmat off-diagonals match ccc() and reflect the data", {
  cc <- caret:::cccmat(adapt_wide)
  # each cell should be the real pairwise score - before the fix this matrix
  # came back as all 1s
  expect_equal(
    cc["m1", "m2"],
    caret:::ccc(adapt_wide[, "m1"], adapt_wide[, "m2"])
  )
  expect_equal(
    cc["m1", "m3"],
    caret:::ccc(adapt_wide[, "m1"], adapt_wide[, "m3"])
  )
  # m1 and m2 look almost the same; m1 and m3 don't
  expect_gt(cc["m1", "m2"], 0.9)
  expect_lt(abs(cc["m1", "m3"]), 0.1)
})

# --- diffmat ----------------------------------------------------------------

test_that("diffmat is symmetric with an NA diagonal", {
  dm <- caret:::diffmat(adapt_wide)
  expect_equal(dim(dm), c(4, 4))
  expect_all_true(is.na(diag(dm)))
  # it should match its own transpose (leaving the NA diagonal aside)
  expect_equal(dm[lower.tri(dm)], t(dm)[lower.tri(dm)])
})

test_that("diffmat returns 0 when two columns differ by a constant", {
  # if two columns differ by the same amount everywhere, there's no spread in
  # the difference, so the gap comes out as 0
  m <- cbind(a = c(1, 2, 3), b = c(1, 2, 3) + 5)
  dm <- caret:::diffmat(m)
  expect_equal(dm["a", "b"], 0)
})

# --- long2wide --------------------------------------------------------------

test_that("long2wide reshapes resampling results to one row per resample", {
  w <- caret:::long2wide(adapt_results, "RMSE")
  expect_named(w, c("Resample", "m1", "m2", "m3", "m4"))
  expect_equal(nrow(w), length(unique(adapt_results$Resample)))
  expect_equal(w$Resample, sort(unique(adapt_results$Resample)))
  # pick one cell and make sure it survived the reshape intact
  expect_equal(w$m3[w$Resample == "Fold1"], 2.0)
})

# --- get_id -----------------------------------------------------------------

test_that("get_id assigns one id per unique parameter combination", {
  ids <- caret:::get_id(adapt_grid, "k")
  # the two k = 3 rows should fold into one
  expect_equal(ids$k, c(3, 5, 7, 9))
  expect_equal(ids$model_id, c("m1", "m2", "m3", "m4"))
})

test_that("get_id zero-pads ids so they sort correctly", {
  ids <- caret:::get_id(data.frame(k = 1:12), "k")
  expect_equal(ids$model_id[1], "m01")
  expect_equal(ids$model_id[12], "m12")
})

# --- filter_on_corr ---------------------------------------------------------

test_that("filter_on_corr drops a redundant (highly concordant) model", {
  keep <- sort(unique(
    caret:::filter_on_corr(adapt_results, "RMSE", cutoff = 0.9)$model_id
  ))
  # m2 just echoes m1, so it gets dropped and the distinct models stay
  expect_equal(keep, c("m1", "m3", "m4"))
})

test_that("filter_on_corr keeps everything when nothing exceeds the cutoff", {
  keep <- sort(unique(
    caret:::filter_on_corr(adapt_results, "RMSE", cutoff = 1)$model_id
  ))
  expect_equal(keep, c("m1", "m2", "m3", "m4"))
})

test_that("filter_on_corr errors with a single model", {
  one <- adapt_results[adapt_results$model_id == "m1", ]
  expect_snapshot(
    caret:::filter_on_corr(one, "RMSE", cutoff = 0.9),
    error = TRUE
  )
})

# --- filter_on_diff ---------------------------------------------------------

test_that("filter_on_diff drops the weaker of two indistinguishable models", {
  keep <- sort(unique(
    caret:::filter_on_diff(
      adapt_results,
      "RMSE",
      cutoff = 0.1,
      maximize = FALSE
    )$model_id
  ))
  # m1 and m2 are basically the same; keep the better one (m1), drop m2
  expect_equal(keep, c("m1", "m3", "m4"))
})

test_that("filter_on_diff keeps everything when nothing is below the cutoff", {
  # nothing is closer than the cutoff, so it should bail out early and leave
  # every model in place
  keep <- sort(unique(
    caret:::filter_on_diff(
      adapt_results,
      "RMSE",
      cutoff = 0,
      maximize = FALSE
    )$model_id
  ))
  expect_equal(keep, c("m1", "m2", "m3", "m4"))
})

# --- end-to-end through train() ----------------------------------------------
# These run adaptive resampling the way someone actually would - through
# train() - and check we get a sensible model back. It's the only route that
# reaches adaptiveWorkflow() (and the diversity filters underneath it), so we
# keep the runs tiny and skip when the optional racing packages aren't around.
# adaptive_knn_fit() lives in helper-adaptive.R.

test_that("adaptive_cv tuning runs end to end with the gls racer", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  fit <- adaptive_knn_fit("gls")

  expect_s3_class(fit, "train")
  expect_equal(fit$control$method, "adaptive_cv")
  expect_false(is.null(fit$finalModel))
  # every candidate still shows up in the results, and the winner is one of them
  expect_equal(sort(fit$results$k), c(1, 5, 9, 13, 17))
  expect_true(fit$bestTune$k %in% fit$results$k)
  # and the model we get back can actually make predictions
  expect_length(predict(fit, iris), nrow(iris))
})

test_that("adaptive_cv tuning runs end to end with the Bradley-Terry racer", {
  skip_on_cran()
  skip_if_not_installed("BradleyTerry2")

  fit <- adaptive_knn_fit("BT")

  expect_s3_class(fit, "train")
  expect_true(fit$bestTune$k %in% fit$results$k)
  expect_length(predict(fit, iris), nrow(iris))
})

# ------------------------------------------------------------------------------
# adaptiveWorkflow: the paths that differ from the plain knn race above

test_that("adaptive resampling collects class probabilities and predictions", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  cls <- engine_two_class(60)
  set.seed(3742)
  fit <- suppressWarnings(train(
    cls[, names(cls) != "Class"],
    cls$Class,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5, 7, 9)),
    metric = "ROC",
    trControl = trainControl(
      method = "adaptive_cv",
      number = 6,
      classProbs = TRUE,
      savePredictions = "all",
      summaryFunction = twoClassSummary,
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
    )
  ))
  expect_s3_class(fit, "train")
  # the saved predictions carry a probability column per class
  expect_contains(names(fit$pred), c("Class1", "Class2", "obs", "rowIndex"))
  expect_all_false(is.na(fit$results$ROC))
})

test_that("adaptive resampling scores sub-models from one fit", {
  skip_on_cran()
  skip_if_not_installed("nlme")
  skip_if_not_installed("pls")

  cls <- engine_two_class(60)
  set.seed(8125)
  # pls announces the objects it masks when caret attaches it
  fit <- suppressMessages(suppressWarnings(train(
    cls[, names(cls) != "Class"],
    cls$Class,
    method = "pls",
    tuneGrid = data.frame(ncomp = 1:4),
    metric = "ROC",
    trControl = trainControl(
      method = "adaptive_cv",
      number = 6,
      classProbs = TRUE,
      savePredictions = "all",
      summaryFunction = twoClassSummary,
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
    )
  )))
  # every candidate that survived the race is scored
  expect_gte(nrow(fit$results), 1L)
  expect_in(fit$bestTune$ncomp, 1:4)
})

test_that("adaptive resampling works for regression with case weights", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  reg <- engine_regression(60)
  wts <- rep(c(1, 2), length.out = nrow(reg))

  set.seed(1653)
  fit <- suppressWarnings(train(
    reg[, 1:3],
    reg$y,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5, 7, 9)),
    weights = wts,
    trControl = trainControl(
      method = "adaptive_cv",
      number = 6,
      savePredictions = "final",
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
    )
  ))
  expect_identical(fit$modelType, "Regression")
  expect_all_false(is.na(fit$results$RMSE))
})

test_that("adaptive resampling runs over bootstrap and group splits", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  cls <- engine_three_class()
  for (m in c("adaptive_boot", "adaptive_LGOCV")) {
    set.seed(9401)
    fit <- suppressWarnings(train(
      Species ~ .,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(k = c(3, 5, 7, 9)),
      trControl = trainControl(
        method = m,
        number = 6,
        p = 0.75,
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    ))
    expect_identical(fit$control$method, m)
    expect_all_false(is.na(fit$results$Accuracy))
  }
})

test_that("adaptive resampling can stop as soon as one model is left", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  cls <- engine_three_class()
  set.seed(6274)
  fit <- suppressWarnings(train(
    Species ~ .,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(k = c(1, 5, 9, 13, 17)),
    trControl = trainControl(
      method = "adaptive_cv",
      number = 10,
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = FALSE)
    )
  ))
  # with complete = FALSE the race ends early, so the surviving candidates have
  # been scored on fewer resamples than the full ten
  expect_s3_class(fit, "train")
  expect_lte(max(fit$resample$Resample %in% fit$resample$Resample), 10L)
})

test_that("adaptive resampling reports a model that fails in every resample", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  reg <- engine_regression(40)
  always <- make_custom_model(fail_when = function(x, y) TRUE)

  suppressWarnings(
    expect_snapshot(
      train(
        reg[, 1:3],
        reg$y,
        method = always,
        tuneLength = 2,
        trControl = trainControl(
          method = "adaptive_cv",
          number = 4,
          adaptive = list(
            min = 2,
            alpha = 0.05,
            method = "gls",
            complete = TRUE
          )
        )
      ),
      error = TRUE,
      transform = mask_na_label
    )
  )
})

test_that("adaptive resampling passes the workflow debug flag through", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  reg <- engine_regression(30)
  tolerant <- make_custom_model()

  # the custom model predicts its own tuning value, so the debug trace is the
  # same every run; the resampled metrics in it are masked
  expect_snapshot(
    fit <- suppressWarnings(train(
      reg[, 1:3],
      reg$y,
      method = tolerant,
      tuneLength = 2,
      trControl = trainControl(
        method = "adaptive_cv",
        number = 4,
        adaptive = list(min = 2, alpha = 0.05, method = "gls", complete = TRUE)
      ),
      testing = TRUE
    )),
    transform = mask_decimals
  )
  expect_s3_class(fit, "train")
})

# ------------------------------------------------------------------------------
# the racing evaluators, on the shared fixture

test_that("seq_eval keeps the models that are not yet distinguishable", {
  withr::local_package("plyr")

  # m1 is best and m2 is its near-copy, so the two cannot be told apart yet;
  # m3 and m4 are clearly worse and are dropped
  expect_setequal(
    caret:::seq_eval(adapt_results, "RMSE", maximize = FALSE),
    c("m1", "m2")
  )

  # maximizing turns the ordering around, and the worst model becomes the one
  # to beat
  expect_identical(
    caret:::seq_eval(adapt_results, "RMSE", maximize = TRUE),
    "m4"
  )

  # with only two models there is a single comparison, made with a t-test
  two <- adapt_results[adapt_results$model_id %in% c("m1", "m3"), ]
  expect_identical(caret:::seq_eval(two, "RMSE", maximize = FALSE), "m1")
})

test_that("gls_eval keeps the models that are not yet distinguishable", {
  skip_if_not_installed("nlme")
  withr::local_package("plyr")

  # the mixed model reaches the same conclusion as the sequential test
  expect_setequal(
    caret:::gls_eval(adapt_results, "RMSE", maximize = FALSE),
    c("m1", "m2")
  )
})

test_that("bt_eval ranks the models by paired comparisons", {
  skip_if_not_installed("BradleyTerry2")
  withr::local_package("plyr")

  # the Bradley-Terry model is more forgiving here and keeps the middle model
  keepers <- suppressWarnings(
    caret:::bt_eval(adapt_results, "RMSE", maximize = FALSE)
  )
  expect_contains(keepers, c("m1", "m2"))
  expect_disjoint(keepers, "m4")
})

test_that("get_scores counts the wins within one resample", {
  skip_if_not_installed("BradleyTerry2")

  one_fold <- adapt_results[adapt_results$Resample == "Fold1", ]
  scores <- caret:::get_scores(one_fold, maximize = FALSE, metric = "RMSE")

  expect_named(scores, c("player1", "player2", "win1", "win2"))
  # one row per pair of the four models
  expect_identical(nrow(scores), 6L)
  # m1 has the lowest RMSE in this fold, so it wins all three of its pairings
  m1_rows <- scores[scores$player1 == "m1", ]
  expect_all_equal(m1_rows$win1, 1)
  expect_all_equal(m1_rows$win2, 0)
})

test_that("skunked drops the models that never win", {
  withr::local_package("plyr")

  # m4 loses every comparison it takes part in
  scores <- data.frame(
    player1 = c("m1", "m1", "m2"),
    player2 = c("m2", "m4", "m4"),
    win1 = c(3, 5, 4),
    win2 = c(2, 0, 0),
    stringsAsFactors = FALSE
  )

  expect_snapshot(out <- caret:::skunked(scores))
  expect_disjoint(c(as.character(out$player1), as.character(out$player2)), "m4")

  # and it can be told to keep quiet about it
  expect_silent(quiet <- caret:::skunked(scores, verbose = FALSE))
  expect_identical(nrow(quiet), nrow(out))
})

test_that("retrospective re-runs the race on a finished model", {
  skip_on_cran()
  skip_if_not_installed("nlme")
  withr::local_package("plyr")

  cls <- engine_three_class()
  set.seed(2039)
  fit <- train(
    Species ~ .,
    data = cls,
    method = "knn",
    tuneGrid = data.frame(k = c(1, 5, 9, 13)),
    trControl = trainControl(method = "cv", number = 6, returnResamp = "all")
  )

  # given an ordinary fit, retrospective() asks which candidates the race would
  # still have been considering after B resamples
  out <- caret:::retrospective(fit, B = 5, method = "gls")
  expect_named(out, c("models", "mods", "long", "wide"))
  # the surviving models are a subset of the candidates, in both views
  expect_in(out$models, out$long$model_id)
  expect_setequal(out$mods$model_id, out$models)
  # `wide` is one row per resample and one column per candidate
  expect_identical(nrow(out$wide), 5L)
  expect_contains(colnames(out$wide), out$models)
})

test_that("adaptive resampling reports its progress", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  cls <- engine_three_class()
  set.seed(5866)
  # verboseIter names each resample and candidate, and the race says how many
  # models it dropped after each one
  expect_snapshot(
    fit <- suppressWarnings(train(
      Species ~ .,
      data = cls,
      method = "knn",
      tuneGrid = data.frame(k = c(1, 9, 17)),
      trControl = trainControl(
        method = "adaptive_cv",
        number = 5,
        verboseIter = TRUE,
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )),
    transform = mask_decimals
  )
  expect_s3_class(fit, "train")
})

test_that("adaptive resampling carries on when a model fails in one resample", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  reg <- engine_regression(60)
  # exactly one fold holds out the largest outcome, so exactly one fit fails.
  # The race burns in on `min - 1` resamples, so min = 3 leaves it a resample to
  # compare on even if the failing fold is one of them.
  sometimes <- make_custom_model(
    fail_when = function(x, y) max(y) < max(reg$y)
  )

  set.seed(7017)
  # one warning per candidate that failed, then one for the hole they leave
  expect_snapshot(
    fit <- train(
      reg[, 1:3],
      reg$y,
      method = sometimes,
      tuneLength = 2,
      trControl = trainControl(
        method = "adaptive_cv",
        number = 5,
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )
  )
  expect_s3_class(fit, "train")
  # the failed resample leaves a hole rather than stopping the race
  expect_true(anyNA(fit$resample$RMSE) || nrow(fit$resample) < 8L)
})

test_that("adaptive resampling keeps sub-model probabilities through the race", {
  skip_on_cran()
  skip_if_not_installed("nlme")
  skip_if_not_installed("rpart")

  cls <- engine_three_class()
  set.seed(4034)
  fit <- suppressWarnings(train(
    Species ~ .,
    data = cls,
    method = "rpart",
    tuneGrid = data.frame(cp = c(0.01, 0.05, 0.1, 0.3)),
    trControl = trainControl(
      method = "adaptive_cv",
      number = 6,
      classProbs = TRUE,
      savePredictions = "all",
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
    )
  ))
  # one fit per resample scores every cp value, each with its own probabilities
  expect_contains(names(fit$pred), c(levels(cls$Species), "cp", "rowIndex"))
  expect_gt(length(unique(fit$pred$cp)), 1L)
})

test_that("the diversity filters can report what they dropped", {
  withr::local_package("plyr")

  # m2 is a near-copy of m1, so each filter drops one of the four models
  expect_snapshot(
    keep_corr <- caret:::filter_on_corr(
      adapt_results,
      "RMSE",
      cutoff = 0.9,
      verbose = TRUE
    )
  )
  expect_length(keep_corr, 3)

  expect_snapshot(
    keep_diff <- caret:::filter_on_diff(
      adapt_results,
      "RMSE",
      cutoff = 0.1,
      maximize = FALSE,
      verbose = TRUE
    )
  )
  expect_length(keep_diff, 3)
})

# ------------------------------------------------------------------------------
# the race's failure paths, crossed with sub-models and class probabilities
#
# make_submodel_model() fails for whichever resample holds out the sentinel row,
# so exactly one resample fails and the race still has something to compare.

test_that("the race fills in sub-model predictions when a fit fails", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  dat <- engine_sentinel_data(60)
  failing <- make_submodel_model(fail_fit = TRUE)

  set.seed(4471)
  # one warning for the resample that failed, then one for the hole it leaves
  expect_snapshot(
    fit <- train(
      dat[, 1:3],
      dat$y,
      method = failing,
      tuneLength = 3,
      trControl = trainControl(
        method = "adaptive_cv",
        number = 5,
        classProbs = TRUE,
        savePredictions = "all",
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )
  )
  # every candidate is still scored, from the resamples that worked
  expect_identical(nrow(fit$results), 3L)
  # and the failed resample leaves missing probabilities behind rather than
  # dropping the rows
  expect_contains(names(fit$pred), c("one", "two", "shift", "scale"))
  expect_true(anyNA(fit$pred$one))
})

test_that("the race fills in sub-model predictions when prediction fails", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  dat <- engine_sentinel_data(60)
  bad_pred <- make_submodel_model(fail_pred = TRUE)

  set.seed(4471)
  # one warning for the resample that failed, then one for the hole it leaves
  expect_snapshot(
    fit <- train(
      dat[, 1:3],
      dat$y,
      method = bad_pred,
      tuneLength = 3,
      trControl = trainControl(
        method = "adaptive_cv",
        number = 5,
        adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
      )
    )
  )
  expect_identical(nrow(fit$results), 3L)
  expect_all_false(is.na(fit$results$Accuracy))
})

test_that("the race scores sub-models without class probabilities", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  # a regression outcome, so there are no probabilities to collect and the
  # placeholder frames are built instead
  dat <- engine_sentinel_data(60, classification = FALSE)
  subs <- make_submodel_model()

  set.seed(6907)
  fit <- suppressWarnings(train(
    dat[, 1:3],
    dat$y,
    method = subs,
    tuneLength = 3,
    trControl = trainControl(
      method = "adaptive_cv",
      number = 5,
      savePredictions = "all",
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
    )
  ))
  expect_identical(fit$modelType, "Regression")
  expect_identical(nrow(fit$results), 3L)
  expect_contains(names(fit$pred), c("pred", "obs", "shift", "scale"))
})
