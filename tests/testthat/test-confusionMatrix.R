test_that("Confusion matrix works", {
  skip_on_cran()
  set.seed(442)
  train <- twoClassSim(
    n = 1000,
    intercept = -8,
    linearVars = 3,
    noiseVars = 10,
    corrVars = 4,
    corrValue = 0.6
  )

  ctrl <- trainControl(method = "cv", classProbs = TRUE)

  fullModel <- train(
    Class ~ .,
    data = train,
    method = "knn",
    preProc = c("center", "scale"),
    tuneLength = 4,
    trControl = ctrl
  )
  dat <- train$Class
  ref <- predict(fullModel, newdata = train)
  dat2 <- as.character(dat)
  ref2 <- as.character(ref)
  dat2 <- factor(dat2, levels = c("Class2", "Class1"))
  ref2 <- factor(ref2, levels = c("Class1", "Class2"))
  dat3 <- rep("Class1", length(ref2))
  dat3 <- factor(dat3)
  dat4 <- factor(dat3, levels = c("Class1", "Class4"))
  dat5 <- as.character(dat3)
  dat5[200] <- "Class4"
  dat5 <- factor(dat5, levels = c("Class1", "Class4"))
  cm1 <- confusionMatrix(dat, ref)
  cm2 <- suppressWarnings(confusionMatrix(dat2, ref2))
  cm3 <- suppressWarnings(confusionMatrix(dat3, ref2))
  cm4 <- suppressWarnings(confusionMatrix(dat4, ref2))
  expect_s3_class(cm1, "confusionMatrix")
  expect_s3_class(cm2, "confusionMatrix")
  expect_s3_class(cm3, "confusionMatrix")
  expect_s3_class(cm4, "confusionMatrix")
  expect_snapshot_warning(confusionMatrix(dat2, ref2))
  expect_snapshot_warning(confusionMatrix(dat3, ref2))
  expect_snapshot(confusionMatrix(dat5, ref2), error = TRUE)
  expect_snapshot(confusionMatrix(dat5, ref), error = TRUE)
  expect_identical(cm1$overall, cm2$overall)
  expect_identical(cm4$overall, cm3$overall)
  expect_true(identical(cm1, cm2))
  expect_true(identical(cm3, cm4))

  mat1 <- as.matrix(cm1$table)
  cm11 <- confusionMatrix(mat1)
  expect_equal(cm1, cm11)
})

# ------------------------------------------------------------------------------
# confusionMatrix.table / .matrix on fixed tables (cm_tab2, cm_tab3 live in
# helper-confusionMatrix.R)

test_that("confusionMatrix.table computes two-class statistics", {
  cm <- confusionMatrix(cm_tab2)
  expect_s3_class(cm, "confusionMatrix")
  # first level is the default positive class
  expect_identical(cm$positive, "yes")
  expect_identical(cm$mode, "sens_spec")
  # computed ratios: expect_equal (tolerance) rather than expect_identical, so
  # the test survives future changes to how the statistic is derived
  expect_equal(cm$byClass["Sensitivity"], c(Sensitivity = 0.8))
  expect_equal(cm$byClass["Specificity"], c(Specificity = 0.9))
  expect_identical(
    names(cm$overall),
    c(
      "Accuracy",
      "Kappa",
      "AccuracyLower",
      "AccuracyUpper",
      "AccuracyNull",
      "AccuracyPValue",
      "McnemarPValue"
    )
  )
})

test_that("confusionMatrix.table honours the positive and prevalence args", {
  # choose the other class as positive: sensitivity/specificity swap
  cm <- confusionMatrix(cm_tab2, positive = "no")
  expect_identical(cm$positive, "no")
  # computed ratio, so keep the tolerant comparison
  expect_equal(cm$byClass["Sensitivity"], c(Sensitivity = 0.9))

  # a supplied prevalence overrides the value taken from the table (a pass
  # through, so it round-trips exactly)
  cm_prev <- confusionMatrix(cm_tab2, prevalence = 0.25)
  expect_identical(cm_prev$byClass["Prevalence"], c(Prevalence = 0.25))
})

test_that("confusionMatrix.table handles the multiclass case", {
  cm <- confusionMatrix(cm_tab3)
  # per-class stats come back as a matrix, one row per class
  expect_true(is.matrix(cm$byClass))
  expect_identical(nrow(cm$byClass), 3L)
  expect_identical(rownames(cm$byClass), paste("Class:", LETTERS[1:3]))

  # a named per-class prevalence vector is accepted and used
  cm_prev <- confusionMatrix(cm_tab3, prevalence = c(A = 0.3, B = 0.3, C = 0.4))
  expect_identical(cm_prev$byClass["Class: A", "Prevalence"], 0.3)
})

test_that("confusionMatrix.matrix matches confusionMatrix.table", {
  from_matrix <- confusionMatrix(unclass(cm_tab2))
  from_table <- confusionMatrix(cm_tab2)
  expect_identical(from_matrix$table, from_table$table)
  expect_identical(from_matrix$byClass, from_table$byClass)
})

test_that("confusionMatrix.table rejects bad input", {
  expect_snapshot(confusionMatrix(cm_tab2, mode = "nope"), error = TRUE)
  expect_snapshot(
    confusionMatrix(as.table(matrix(1:6, nrow = 2))),
    error = TRUE
  )
  # a one-level table has too few classes
  one <- as.table(matrix(
    5,
    dimnames = list(Prediction = "a", Reference = "a")
  ))
  expect_snapshot(confusionMatrix(one), error = TRUE)
  # positive must be a string
  expect_snapshot(confusionMatrix(cm_tab2, positive = 1), error = TRUE)
  # row and column classes must match
  mismatched <- as.table(matrix(
    c(8, 2, 1, 9),
    nrow = 2,
    dimnames = list(c("a", "b"), c("a", "c"))
  ))
  expect_snapshot(confusionMatrix(mismatched), error = TRUE)
})

test_that("confusionMatrix.table rejects mis-sized prevalence vectors", {
  # two classes need a single prevalence
  expect_snapshot(
    confusionMatrix(cm_tab2, prevalence = c(0.2, 0.3)),
    error = TRUE
  )
  # multiclass needs one prevalence per class...
  expect_snapshot(
    confusionMatrix(cm_tab3, prevalence = c(0.5, 0.5)),
    error = TRUE
  )
  # ...and it must be named
  expect_snapshot(
    confusionMatrix(cm_tab3, prevalence = c(0.3, 0.3, 0.4)),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------
# as.matrix / as.table methods

test_that("as.matrix.confusionMatrix returns each requested piece", {
  cm <- confusionMatrix(cm_tab2)
  expect_identical(dim(as.matrix(cm, what = "xtabs")), c(2L, 2L))
  expect_identical(nrow(as.matrix(cm, what = "overall")), 7L)
  expect_identical(nrow(as.matrix(cm, what = "classes")), 11L)
  # the multiclass "classes" matrix is transposed to one column per class
  cm3 <- confusionMatrix(cm_tab3)
  expect_identical(colnames(as.matrix(cm3, what = "classes")), LETTERS[1:3])
  expect_snapshot(as.matrix(cm, what = "nope"), error = TRUE)
})

test_that("as.table.confusionMatrix returns the underlying table", {
  cm <- confusionMatrix(cm_tab2)
  expect_identical(as.table(cm), cm$table)
})

# ------------------------------------------------------------------------------
# print methods (deterministic tables -> safe to snapshot)

test_that("print.confusionMatrix renders each mode", {
  expect_snapshot(print(confusionMatrix(cm_tab2, mode = "sens_spec")))
  expect_snapshot(print(confusionMatrix(cm_tab2, mode = "prec_recall")))
  expect_snapshot(print(confusionMatrix(cm_tab2, mode = "everything")))
  expect_snapshot(print(confusionMatrix(cm_tab3)))
  # the table can be printed on its own, without the statistics block
  expect_snapshot(print(confusionMatrix(cm_tab2), printStats = FALSE))
})

# ------------------------------------------------------------------------------
# confusionMatrix.default input validation

test_that("confusionMatrix.default validates its arguments", {
  a <- factor(c("a", "b", "a", "b"))
  b <- factor(c("a", "a", "b", "b"))
  expect_snapshot(confusionMatrix(a, b, mode = "nope"), error = TRUE)
  # non-factor input
  expect_snapshot(confusionMatrix(1:4, b), error = TRUE)
  # positive must be a string
  expect_snapshot(confusionMatrix(a, b, positive = 1), error = TRUE)
  # data has a level the reference does not
  more <- factor(c("a", "b", "c", "b"))
  expect_snapshot(confusionMatrix(more, b), error = TRUE)
  # data and reference share no levels
  x <- factor(c("x", "y"), levels = c("x", "y"))
  y <- factor(c("a", "b"), levels = c("a", "b"))
  expect_snapshot(confusionMatrix(x, y), error = TRUE)
})

# ------------------------------------------------------------------------------
# resampName text for each resampling scheme (pure helper, fake control objects)

test_that("resampName describes each resampling method", {
  mk <- function(method, ...) {
    list(
      control = c(
        list(method = method, index = list(1:3, 1:3, 1:3)),
        list(...)
      )
    )
  }
  rn <- function(...) caret:::resampName(...)

  # no control element -> empty string
  expect_identical(caret:::resampName(list()), "")

  expect_identical(rn(mk("cv", number = 10)), "Cross-Validated (10 fold)")
  expect_identical(
    rn(mk("repeatedcv", number = 10, repeats = 5)),
    "Cross-Validated (10 fold, repeated 5 times)"
  )
  expect_identical(rn(mk("boot")), "Bootstrapped (3 reps)")
  expect_identical(rn(mk("boot632")), "Bootstrapped (3 reps)")
  expect_identical(
    rn(mk("LGOCV", p = 0.75)),
    "Repeated Train/Test Splits Estimated (3 reps, 75%)"
  )
  expect_identical(rn(mk("LOOCV")), "Leave-One-Out Cross-Validation")
  expect_identical(rn(mk("none")), "None")
  expect_identical(rn(mk("apparent")), "Apparent")
  expect_identical(rn(mk("custom")), "Custom Resampling (3 reps)")
  expect_identical(
    rn(mk("timeslice", horizon = 5, fixedWindow = TRUE)),
    "Rolling Forecasting Origin Resampling (5 held-out with a fixed window)"
  )
  expect_identical(rn(mk("oob")), "Out of Bag Resampling")
  expect_identical(
    rn(mk("adaptive_cv", number = 10, repeats = 5)),
    "Adaptively Cross-Validated (10 fold, repeated 5 times)"
  )

  # numbers = FALSE gives the short form
  expect_identical(
    rn(mk("cv", number = 10), numbers = FALSE),
    "(Cross-Validation)"
  )
})

# ------------------------------------------------------------------------------
# resampled confusion matrices from train / rfe / sbf objects

test_that("confusionMatrix.train summarises the resampled matrix", {
  skip_on_cran()

  set.seed(1)
  dat <- twoClassSim(200)
  fit <- train(
    Class ~ .,
    data = dat,
    method = "knn",
    tuneLength = 2,
    trControl = trainControl(method = "cv", number = 3, classProbs = TRUE)
  )

  cm <- confusionMatrix(fit)
  expect_s3_class(cm, "confusionMatrix.train")
  expect_identical(cm$norm, "overall")
  # percentages, so the whole table sums to ~100 (floating point -> expect_equal)
  expect_equal(sum(cm$table), 100)

  # the other normalisations run and print without error
  expect_identical(confusionMatrix(fit, norm = "none")$norm, "none")
  expect_identical(confusionMatrix(fit, norm = "average")$norm, "average")
  expect_output(print(cm), "Confusion Matrix")
})

test_that("confusionMatrix errors on regression and non-resampled fits", {
  skip_on_cran()

  set.seed(1)
  reg <- SLC14_1(100)
  reg_fit <- train(
    y ~ .,
    data = reg,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_snapshot(confusionMatrix(reg_fit), error = TRUE)

  set.seed(1)
  dat <- twoClassSim(120)
  loo_fit <- train(
    Class ~ .,
    data = dat,
    method = "knn",
    tuneLength = 1,
    trControl = trainControl(method = "LOOCV")
  )
  expect_snapshot(confusionMatrix(loo_fit), error = TRUE)
})

test_that("confusionMatrix works for rfe and sbf objects", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  set.seed(1)
  dat <- twoClassSim(200)

  set.seed(1)
  rf <- rfe(
    dat[, 1:6],
    dat$Class,
    sizes = c(2, 4),
    rfeControl = rfeControl(functions = lrFuncs, method = "cv", number = 3)
  )
  expect_s3_class(confusionMatrix(rf), "confusionMatrix.rfe")

  set.seed(1)
  sf <- sbf(
    dat[, 1:6],
    dat$Class,
    sbfControl = sbfControl(
      functions = ldaSBF,
      method = "cv",
      number = 3,
      saveDetails = TRUE
    )
  )
  expect_s3_class(confusionMatrix(sf), "confusionMatrix.sbf")
})
