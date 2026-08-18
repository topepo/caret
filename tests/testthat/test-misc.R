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

  expect_all_true(knnFit$resample$AUC > 0.5)

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

  # twoClassSummary names its area-under-the-curve column ROC (the old check
  # against an absent AUC column passed vacuously)
  expect_all_true(modle$resample$ROC > 0.5)
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
  expect_named(ft, paste0(".cell", 1:4))
  # column-major counts: [a,a]=1, [b,a]=0, [a,b]=1, [b,b]=1
  expect_identical(unname(ft), c(1L, 0L, 1L, 1L))
})

test_that("splitIndicies splits evenly when it can", {
  # 6 items into 3 groups -> two of each, in order
  expect_identical(caret:::splitIndicies(6, 3), c(1L, 1L, 2L, 2L, 3L, 3L))
  # with a remainder, every index is still a valid group label
  si <- caret:::splitIndicies(7, 3)
  expect_length(si, 7)
  expect_in(si, 1:3)
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

test_that("cranRef builds a LaTeX link to a package's CRAN page", {
  expect_identical(
    caret:::cranRef("caret"),
    "{\\tt \\href{http://cran.r-project.org/web/packages/caret/index.html}{caret}}"
  )
})

test_that("scrubCall replaces over-long x/y/data arguments", {
  long <- paste(rep("a", 150), collapse = "")
  scrubbed <- caret:::scrubCall(bquote(train(x = .(long), y = 1)))
  expect_identical(as.character(scrubbed[["x"]]), "scrubbed")
  # short arguments are left alone
  short <- caret:::scrubCall(quote(train(x = predictors, y = 1)))
  expect_identical(as.character(short[["x"]]), "predictors")
})

test_that("get_labels maps a method to its human-readable label", {
  expect_identical(caret:::get_labels("knn"), "k-Nearest Neighbors")
})

test_that("Kim2009 simulates a two-class data frame", {
  set.seed(1)
  dat <- caret:::Kim2009(20)
  expect_identical(nrow(dat), 20L)
  expect_s3_class(dat$Class, "factor")
  expect_identical(levels(dat$Class), c("Class1", "Class2"))
})

test_that("subsemble_index returns model and holdout index lists", {
  set.seed(1)
  idx <- caret:::subsemble_index(factor(rep(c("a", "b"), each = 20)))
  expect_named(idx, c("model", "holdout"))
})

test_that("printCall prints the call under a 'Call:' header", {
  expect_snapshot(caret:::printCall(quote(train(y ~ x, data = dat))))
})

test_that("fail_warning warns about a failed model fit", {
  expect_snapshot(
    caret:::fail_warning(list(method = "knn"), "boom", iter = 1, verb = FALSE)
  )
})

test_that("check_na_conflict warns when imputation clashes with na.action", {
  expect_snapshot(
    caret:::check_na_conflict(
      quote(train(y ~ x, na.action = na.omit, preProcess = "knnImpute"))
    )
  )
})

# ------------------------------------------------------------------------------
# formula builders

test_that("gamFormula smooths the predictors with enough unique values", {
  dat <- data.frame(few = rep(1:2, 10), many = seq_len(20), constant = 1)

  # the constant column is dropped as near-zero variance, `many` has more than
  # `cut` unique values so it is smoothed, and `few` is left as it is
  form <- caret:::gamFormula(dat, y = "y")
  expect_s3_class(form, "formula")
  expect_identical(deparse1(form), "y ~ few + s(many)")

  # with a high enough cut-off nothing is smoothed
  expect_identical(
    deparse1(caret:::gamFormula(dat, cut = 25, y = "y")),
    "y ~ few + many"
  )
})

test_that("smootherFormula writes each smoother's arguments", {
  dat <- data.frame(few = rep(1:2, 10), many = seq_len(20), constant = 1)

  # the default outcome name is the one train() gives the outcome column
  expect_identical(
    deparse1(caret:::smootherFormula(dat)),
    ".outcome ~ few + s(many)"
  )
  # s() takes the degrees of freedom only when one was asked for
  expect_identical(
    deparse1(caret:::smootherFormula(dat, df = 3)),
    ".outcome ~ few + s(many, df = 3)"
  )
  expect_identical(
    deparse1(caret:::smootherFormula(dat, smoother = "lo")),
    ".outcome ~ few + lo(many, span = 0.5, degree = 1)"
  )
  expect_identical(
    deparse1(caret:::smootherFormula(dat, smoother = "rcs")),
    ".outcome ~ few + rcs(many)"
  )
})

test_that("depth2cp interpolates the complexity parameter", {
  cp_table <- cbind(nsplit = c(0, 1, 3), CP = c(0.5, 0.2, 0.01))

  out <- caret:::depth2cp(cp_table, c(0, 1, 2))
  # the first two depths are in the table, the third is interpolated
  expect_equal(out, c(0.5, 0.2, 0.105))

  # a depth beyond the deepest tree gets just under the smallest cp
  expect_equal(caret:::depth2cp(cp_table, 9), 0.01 * 0.99)
})

test_that("varSeq lists the predictors in each subset", {
  out <- caret:::varSeq(fake_regsubsets())
  expect_length(out, 3)
  # the intercept is dropped and the subsets grow
  expect_identical(out[[1]], "x1")
  expect_identical(out[[3]], c("x1", "x2", "x3"))
})

test_that("var_seq uses log2 steps for many predictors", {
  # below 500 predictors the sequence is evenly spaced
  expect_identical(caret:::var_seq(100, len = 3), c(2, 51, 100))
  # above it, the steps are powers of two
  expect_identical(caret:::var_seq(1000, len = 3), c(2, 44, 1000))
})

test_that("makeTable describes a model for the documentation tables", {
  info <- data.frame(model = "knn", parameter = "k", Package = "base")
  out <- caret:::makeTable(info)
  expect_named(out, c("method", "Package", "Parameters"))
  expect_identical(out$Parameters, "\\code{k}")
  expect_identical(out$Package, caret:::cranRef("base"))

  # models with nothing to tune carry the placeholder parameter name
  none <- data.frame(model = "lm", parameter = "parameter", Package = "stats")
  expect_identical(caret:::makeTable(none)$Parameters, "None")
})

test_that("get_labels can format labels for LaTeX", {
  out <- caret:::get_labels(c("svmPoly", "lda", "earth"), format = TRUE)
  # several models come back as a table
  expect_s3_class(out, "data.frame")
  expect_named(out, c("model", "label"))
  expect_identical(
    out$label,
    c("Support Vector Machines (Polynomial)", "LDA", "MARS")
  )

  # glmnet has no label of its own and is typeset instead
  expect_identical(
    caret:::get_labels(c("glmnet", "knn"), format = TRUE)$label[1],
    "\\textsf{glmnet}"
  )
  # a method that is not in the library keeps the name it was given
  expect_identical(caret:::get_labels("not_a_model"), "not_a_model")
})

test_that("flatTable fills in the cells when the table comes back empty", {
  # a failed prediction has no levels, so the table has no rows; the cell count
  # then has to come from the observed levels
  out <- caret:::flatTable(
    factor(character(0)),
    factor(character(0), levels = c("a", "b"))
  )
  expect_length(out, 4)
  expect_named(out, paste0(".cell", 1:4))
  expect_all_true(is.na(out))
})

test_that("requireNamespaceQuietStop asks for a missing package by name", {
  expect_null(caret:::requireNamespaceQuietStop("stats"))
  expect_snapshot(
    caret:::requireNamespaceQuietStop("notARealPackage"),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------
# the sampling argument

test_that("parse_sampling accepts a built-in scheme by name", {
  out <- caret:::parse_sampling("down")
  expect_named(out, c("name", "func", "first"))
  expect_identical(out$name, "down")
  expect_true(out$first)
})

test_that("parse_sampling checks that an installed package is available", {
  # the schemes that need another package ask for it before train() starts;
  # checkInstall is mocked so the test never tries to install anything
  asked <- NULL
  with_mocked_bindings(
    caret:::parse_sampling("rose"),
    checkInstall = function(pkg) {
      asked <<- pkg
      invisible(NULL)
    },
    .package = "caret"
  )
  expect_identical(asked, "ROSE")

  # and it can be told not to look
  expect_identical(
    caret:::parse_sampling("rose", check_install = FALSE)$name,
    "rose"
  )
})

test_that("parse_sampling wraps a function of x and y", {
  out <- caret:::parse_sampling(function(x, y) list(x = x, y = y))
  expect_identical(out$name, "custom")
  expect_true(out$first)
})

test_that("parse_sampling rejects anything it cannot use", {
  expect_snapshot(caret:::parse_sampling(1:3), error = TRUE)
  expect_snapshot(caret:::parse_sampling("not_a_scheme"), error = TRUE)
})

test_that("check_samp_func insists on arguments x and y", {
  expect_null(caret:::check_samp_func(function(x, y) NULL))
  # too few arguments
  expect_snapshot(caret:::check_samp_func(function(x) NULL), error = TRUE)
  # the right number, the wrong names
  expect_snapshot(caret:::check_samp_func(function(a, b) NULL), error = TRUE)
})

test_that("check_samp_list insists on the three elements it needs", {
  good <- list(name = "custom", func = function(x, y) NULL, first = TRUE)
  expect_null(caret:::check_samp_list(good))

  # too few elements
  expect_snapshot(
    caret:::check_samp_list(good[c("name", "func")]),
    error = TRUE
  )
  # the right number, the wrong names
  wrong <- good
  names(wrong) <- c("name", "func", "before_pp")
  expect_snapshot(caret:::check_samp_list(wrong), error = TRUE)
  # `first` says when the sampling happens, so it has to be a logical
  bad_first <- good
  bad_first$first <- "yes"
  expect_snapshot(caret:::check_samp_list(bad_first), error = TRUE)
})

# ------------------------------------------------------------------------------
# stand-ins for failed resamples

test_that("fill_failed_pred makes a placeholder of the right shape", {
  # classification placeholders are missing character values, so they can be
  # turned into a factor with the model's levels later on
  cls <- caret:::fill_failed_pred(1:3, lev = c("a", "b"), submod = NULL)
  expect_length(cls, 3)
  expect_all_true(is.na(cls))

  reg <- caret:::fill_failed_pred(1:3, lev = NULL, submod = NULL)
  expect_identical(reg, rep(NA, 3))

  # with sub-models there is one placeholder per candidate, plus the loop's own
  subs <- caret:::fill_failed_pred(
    1:3,
    lev = NULL,
    submod = data.frame(k = 1:2)
  )
  expect_length(subs, 3)
  expect_all_true(vapply(subs, function(x) all(is.na(x)), logical(1)))
})

test_that("fill_failed_prob makes a placeholder for each class", {
  out <- caret:::fill_failed_prob(1:2, lev = c("a", "b"), submod = NULL)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("a", "b"))
  expect_identical(nrow(out), 2L)
  expect_all_true(is.na(as.vector(as.matrix(out))))

  # one frame per sub-model candidate, plus the loop's own
  subs <- caret:::fill_failed_prob(
    1:2,
    lev = c("a", "b"),
    submod = data.frame(k = 1)
  )
  expect_length(subs, 2)
  expect_named(subs[[1]], c("a", "b"))
})

test_that("fail_warning reports the settings that failed", {
  settings <- data.frame(shift = 1, scale = 2)

  # the messages arrive as a list of try-errors from the workflow
  failures <- list(
    try(stop("first problem"), silent = TRUE),
    "not a failure",
    try(stop("second problem"), silent = TRUE)
  )
  expect_snapshot_warning(
    caret:::fail_warning(settings, failures, iter = "Fold1", verb = FALSE)
  )

  # verboseIter also prints the warning as it happens
  expect_snapshot(
    wrn <- caret:::fail_warning(
      settings,
      "boom",
      where = "predictions",
      iter = "Fold2",
      verb = TRUE
    )
  )
})

# ------------------------------------------------------------------------------
# evalSummaryFunction

test_that("evalSummaryFunction names the metrics for a numeric outcome", {
  ctrl <- trainControl(method = "cv")
  set.seed(7451)
  out <- caret:::evalSummaryFunction(
    rnorm(20),
    ctrl = ctrl,
    lev = NULL,
    metric = "RMSE",
    method = "lm"
  )
  expect_named(out, c("RMSE", "Rsquared", "MAE"))
})

test_that("evalSummaryFunction handles a survival outcome", {
  # a Surv outcome cannot be sampled like a vector, so the times are taken from
  # the matrix; the summary function has to be one that can read it
  y <- fake_surv(time = c(5, 10, 15, 20), status = c(1, 0, 1, 1))
  ctrl <- trainControl(
    method = "cv",
    summaryFunction = function(data, lev = NULL, model = NULL) {
      c(Rows = nrow(data), Cols = ncol(data))
    }
  )
  set.seed(2843)
  out <- caret:::evalSummaryFunction(
    y,
    ctrl = ctrl,
    lev = NULL,
    metric = "Rows",
    method = "coxph"
  )
  expect_identical(unname(out["Rows"]), 4L)
})

test_that("evalSummaryFunction appends extra performance columns", {
  ctrl <- trainControl(
    method = "cv",
    summaryFunction = function(data, lev = NULL, model = NULL) {
      c(Cols = ncol(data))
    }
  )
  perf <- data.frame(extra1 = 1:20, extra2 = 21:40)
  set.seed(9613)
  out <- caret:::evalSummaryFunction(
    rnorm(20),
    perf = perf,
    ctrl = ctrl,
    lev = NULL,
    metric = "Cols",
    method = "lm"
  )
  # pred, obs, the two extra columns and rowIndex
  expect_identical(unname(out["Cols"]), 5L)
})

test_that("evalSummaryFunction validates its arguments", {
  ctrl <- trainControl(method = "cv")

  # `perf` has to be a data frame, since it is bound to the predictions
  expect_snapshot(
    caret:::evalSummaryFunction(
      rnorm(20),
      perf = 1:20,
      ctrl = ctrl,
      lev = NULL,
      metric = "RMSE",
      method = "lm"
    ),
    error = TRUE
  )

  # the ROC metric needs class probabilities to compute
  expect_snapshot(
    caret:::evalSummaryFunction(
      factor(rep(c("a", "b"), 10)),
      ctrl = ctrl,
      lev = c("a", "b"),
      metric = "ROC",
      method = "glm"
    ),
    error = TRUE
  )
})

test_that("evalSummaryFunction passes case weights to the summary function", {
  ctrl <- trainControl(
    method = "cv",
    summaryFunction = function(data, lev = NULL, model = NULL) {
      c(HasWeights = as.numeric("weights" %in% names(data)))
    }
  )
  set.seed(6528)
  out <- caret:::evalSummaryFunction(
    rnorm(20),
    wts = runif(20),
    ctrl = ctrl,
    lev = NULL,
    metric = "HasWeights",
    method = "lm"
  )
  expect_identical(unname(out["HasWeights"]), 1)
})

# ------------------------------------------------------------------------------
# get_resample_perf

test_that("get_resample_perf pulls the resampled results out of each object", {
  # train keeps every candidate, so the chosen settings are merged back in
  trn <- caret:::get_resample_perf(fake_resample_perf_obj("train"))
  expect_named(trn, c("RMSE", "Rsquared", "Resample"))
  expect_identical(nrow(trn), 3L)

  # rfe keeps one row per subset size
  rfe_obj <- caret:::get_resample_perf(fake_resample_perf_obj("rfe"))
  expect_named(rfe_obj, c("RMSE", "Rsquared", "Resample"))
  expect_identical(nrow(rfe_obj), 2L)

  # sbf has nothing to select, so its resamples are returned as they are
  expect_identical(
    nrow(caret:::get_resample_perf(fake_resample_perf_obj("sbf"))),
    3L
  )

  # the two feature-selection searches report the external resamples of the
  # iteration that was chosen, with the iteration number dropped
  for (cls in c("safs", "gafs")) {
    out <- caret:::get_resample_perf(fake_resample_perf_obj(cls))
    expect_identical(nrow(out), 2L)
    expect_false("Iter" %in% names(out))
  }
})

test_that("get_resample_perf needs the resamples to have been saved", {
  for (cls in c("train", "rfe", "sbf")) {
    obj <- fake_resample_perf_obj(cls, return_resamp = "none")
    expect_snapshot(caret:::get_resample_perf(obj), error = TRUE)
  }
})

# ------------------------------------------------------------------------------
# rule-based model summaries

test_that("partRuleSummary counts conditions, variables and classes", {
  out <- caret:::partRuleSummary(fake_part_rules())
  expect_named(out, c("varUsage", "numCond", "classes"))
  # three conditions mention a variable, one per rule line
  expect_identical(out$numCond, 3L)
  expect_identical(out$varUsage$Var, c("Petal.Width", "Petal.Length"))
  expect_identical(out$varUsage$Overall, c(2, 1))
  # only the lines that end in a count name a class
  expect_identical(unname(out$classes), c(1L, 1L, 0L))
  expect_named(out$classes, c("setosa", "versicolor", "virginica"))
})

test_that("ripperRuleSummary counts conditions, variables and classes", {
  out <- caret:::ripperRuleSummary(fake_ripper_rules())
  expect_named(out, c("varUsage", "numCond", "classes"))
  expect_identical(out$numCond, 3L)
  expect_identical(out$varUsage$Var, c("Petal.Width", "Petal.Length"))
  # the conditions are parenthesised, so each variable is counted once per rule
  expect_identical(out$varUsage$Overall, c(2, 1))
  expect_identical(unname(out$classes), c(1L, 1L, 1L))
})

# ------------------------------------------------------------------------------
# parallel_check

test_that("parallel_check warns about multicore with the wrong package", {
  # doMC is not a dependency, so stand in for it on the search path; the
  # warning depends on the name being there and a backend being registered
  withr::defer(detach("package:doMC", character.only = TRUE))
  attach(new.env(), name = "package:doMC")
  foreach::registerDoSEQ()

  expect_snapshot_warning(caret:::parallel_check(
    "rJava",
    list(library = "rJava")
  ))
  # a model that does not use the package is fine
  expect_silent(caret:::parallel_check("rJava", list(library = "stats")))
})

# ------------------------------------------------------------------------------
# out-of-bag statistics

test_that("the *Stats helpers delegate to the model library's oob function", {
  skip_on_cran()
  skip_if_not_installed("earth")
  withr::local_package("plyr")

  set.seed(3120)
  bagged <- bagEarth(iris[, 1:3], iris[, 4], B = 2)
  # bagEarth stores its own out-of-bag estimates, which are summarised by median
  expect_named(caret:::bagEarthStats(bagged), c("RMSE", "Rsquared", "MAE"))
})

test_that("ipredStats summarises a bagged tree's out-of-bag predictions", {
  skip_on_cran()
  skip_if_not_installed("ipred")

  dat <- engine_three_class()
  set.seed(9204)
  # keepX is what lets the out-of-bag rows be predicted afterwards
  bagged <- ipred::bagging(Species ~ ., data = dat, nbagg = 3, keepX = TRUE)
  out <- caret:::ipredStats(bagged)
  expect_named(out, c("Accuracy", "Kappa", "AccuracySD", "KappaSD"))
})

test_that("cforestStats summarises a conditional forest's out-of-bag predictions", {
  skip_on_cran()
  skip_if_not_installed("party")

  dat <- engine_three_class()
  set.seed(7715)
  # enough trees that every row is left out of at least one of them
  forest <- suppressWarnings(party::cforest(
    Species ~ .,
    data = dat,
    controls = party::cforest_unbiased(ntree = 50, mtry = 2)
  ))
  expect_named(caret:::cforestStats(forest), c("Accuracy", "Kappa"))
})

test_that("subset_x subsets objects that do not take a drop argument", {
  skip_if_not_installed("Matrix")

  # a Matrix object is not a matrix as far as is.matrix() is concerned, so it
  # takes the branch that leaves `drop` out
  m <- Matrix::Matrix(as.numeric(1:12), nrow = 4)
  out <- caret:::subset_x(m, c(1, 3))
  expect_identical(dim(out), c(2L, 3L))
})

test_that("check_dims and get_range read a survival outcome by row", {
  x <- matrix(1:8, nrow = 4)
  y <- fake_surv(time = c(5, 10, 15, 20))

  # a Surv outcome has one row per observation rather than one element
  expect_null(caret:::check_dims(x, y))
  expect_snapshot(caret:::check_dims(x[1:3, ], y), error = TRUE)

  # its range comes from the event times
  expect_identical(caret:::get_range(y), extendrange(c(5, 10, 15, 20)))
})
