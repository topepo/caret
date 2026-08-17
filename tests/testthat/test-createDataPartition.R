test_that("createTimeSlices works as expected", {
  skip_on_cran()
  s1 <- createTimeSlices(1:8, 5, horizon = 1)
  s2 <- createTimeSlices(1:8, 5, horizon = 1, skip = 3)
  s3 <- createTimeSlices(1:10, 5, horizon = 1, fixedWindow = FALSE, skip = 3)
  s4 <- createTimeSlices(1:10, 5, horizon = 2, skip = 2)

  expect_equal(
    s1,
    list(
      train = list(Training5 = 1:5, Training6 = 2:6, Training7 = 3:7),
      test = list(Testing5 = 6L, Testing6 = 7L, Testing7 = 8L)
    )
  )

  expect_equal(
    s2,
    list(
      train = structure(list(Training5 = 1:5)),
      test = structure(list(Testing5 = 6L))
    )
  )

  expect_equal(
    s3,
    list(
      train = list(Training5 = 1:5, Training9 = 1:9),
      test = list(Testing5 = 6L, Testing9 = 10L)
    )
  )

  expect_equal(
    s4,
    list(
      train = list(Training5 = 1:5, Training8 = 4:8),
      test = list(Testing5 = 6:7, Testing8 = 9:10)
    )
  )
})

# ------------------------------------------------------------------------------
# createDataPartition

test_that("createDataPartition stratifies a factor outcome", {
  set.seed(5539)
  y <- factor(rep(c("a", "b"), times = c(40, 20)))
  idx <- createDataPartition(y, p = 0.5)
  expect_named(idx, "Resample1")
  # the class balance is preserved in the sample
  expect_identical(as.vector(table(y[idx$Resample1])), c(20L, 10L))
})

test_that("createDataPartition bins a numeric outcome before sampling", {
  set.seed(2861)
  y <- rnorm(100)
  idx <- createDataPartition(y, times = 2, p = 0.6, groups = 4)
  expect_named(idx, c("Resample1", "Resample2"))
  expect_all_true(lengths(idx) >= 60)
})

test_that("createDataPartition can return a matrix", {
  set.seed(7294)
  y <- factor(rep(c("a", "b"), each = 20))
  out <- createDataPartition(y, times = 3, list = FALSE)
  expect_identical(ncol(out), 3L)
  expect_identical(colnames(out), paste0("Resample", 1:3))
})

test_that("createDataPartition warns about empty and single-record classes", {
  # a dropped level has no records at all
  y <- factor(c(rep("a", 10), rep("b", 10)), levels = c("a", "b", "c"))
  expect_snapshot_warning(createDataPartition(y, p = 0.5))

  # a class with one record is always selected
  y2 <- factor(c(rep("a", 10), "b"))
  expect_snapshot_warning(idx <- createDataPartition(y2, p = 0.5))
  expect_contains(idx$Resample1, which(y2 == "b"))
})

test_that("createDataPartition needs at least two data points", {
  expect_snapshot(createDataPartition(factor("a")), error = TRUE)
})

test_that("createDataPartition samples the times of a Surv outcome", {
  set.seed(4062)
  y <- fake_surv(rnorm(40, 10))
  idx <- createDataPartition(y, p = 0.5)
  expect_length(idx, 1)
})

# ------------------------------------------------------------------------------
# createFolds, createMultiFolds and groupKFold

test_that("createFolds splits a factor outcome into k folds", {
  set.seed(3106)
  y <- factor(rep(c("a", "b"), each = 25))
  folds <- createFolds(y, k = 5)
  expect_named(folds, paste0("Fold", 1:5))
  # every observation appears in exactly one hold-out fold
  expect_setequal(unlist(folds), seq_along(y))
})

test_that("createFolds can return training rows or a fold vector", {
  set.seed(8617)
  y <- factor(rep(c("a", "b"), each = 25))

  train_rows <- createFolds(y, k = 5, returnTrain = TRUE)
  expect_all_true(lengths(train_rows) == 40)

  vec <- createFolds(y, k = 5, list = FALSE)
  expect_length(vec, 50)
  expect_setequal(unique(vec), 1:5)
})

test_that("createFolds bins numeric outcomes and handles small samples", {
  set.seed(1275)
  folds <- createFolds(rnorm(60), k = 4)
  expect_setequal(unlist(folds), 1:60)

  # with few samples per fold the numeric binning falls back to two groups
  small <- createFolds(rnorm(8), k = 4)
  expect_setequal(unlist(small), 1:8)
})

test_that("createFolds splits the times of a Surv outcome", {
  set.seed(9548)
  y <- fake_surv(rnorm(40, 10))
  folds <- createFolds(y, k = 4)
  expect_setequal(unlist(folds), 1:40)
})

test_that("createMultiFolds repeats a k-fold split", {
  set.seed(2233)
  y <- factor(rep(c("a", "b"), each = 20))
  folds <- createMultiFolds(y, k = 4, times = 2)
  expect_length(folds, 8)
  expect_contains(names(folds), c("Fold1.Rep1", "Fold1.Rep2"))
})

test_that("createMultiFolds splits the times of a Surv outcome", {
  set.seed(6395)
  y <- fake_surv(rnorm(40, 10))
  folds <- createMultiFolds(y, k = 4, times = 2)
  expect_length(folds, 8)
})

test_that("groupKFold keeps a group together within a fold", {
  set.seed(7841)
  group <- rep(letters[1:6], each = 4)
  folds <- groupKFold(group, k = 3)
  expect_length(folds, 3)
  # a held-out group never appears in the matching training set
  held_out <- setdiff(seq_along(group), folds[[1]])
  expect_disjoint(group[held_out], group[folds[[1]]])
})

test_that("groupKFold cannot ask for more folds than groups", {
  expect_snapshot(groupKFold(rep(letters[1:3], each = 2), k = 5), error = TRUE)
})

# ------------------------------------------------------------------------------
# make_resamples

test_that("make_resamples builds indices for each resampling method", {
  y <- factor(rep(c("a", "b"), each = 20))

  methods <- c("cv", "repeatedcv", "boot", "boot632", "LGOCV", "LOOCV")
  for (m in methods) {
    set.seed(3378)
    # `repeats` only means something for repeatedcv, and trainControl warns
    # about it otherwise
    ctrl_args <- list(method = m, number = 3, p = 0.75)
    if (m == "repeatedcv") {
      ctrl_args$repeats <- 2
    }
    ctrl <- caret:::make_resamples(do.call(trainControl, ctrl_args), y)
    expect_true(length(ctrl$index) > 0, info = m)
    expect_length(ctrl$indexOut, length(ctrl$index))
  }
})

test_that("make_resamples handles the degenerate resampling methods", {
  y <- factor(rep(c("a", "b"), each = 10))

  # "none" and "apparent" both train on everything
  none <- caret:::make_resamples(trainControl(method = "none"), y)
  expect_identical(none$index[[1]], seq_along(y))

  app <- caret:::make_resamples(trainControl(method = "apparent"), y)
  expect_named(app$indexOut, "all")

  # out-of-bag resampling leaves the indices empty
  oob <- caret:::make_resamples(trainControl(method = "oob"), y)
  expect_null(oob$index)
})

test_that("make_resamples records the extra bootstrap indices", {
  set.seed(4536)
  y <- factor(rep(c("a", "b"), each = 10))
  ctrl <- caret:::make_resamples(
    trainControl(method = "optimism_boot", number = 2),
    y
  )
  expect_length(ctrl$indexExtra, 2)
  expect_named(ctrl$indexExtra[[1]], c("origIndex", "bootIndex"))
})

test_that("make_resamples builds time slices", {
  ctrl <- caret:::make_resamples(
    trainControl(
      method = "timeslice",
      initialWindow = 10,
      horizon = 2,
      fixedWindow = TRUE
    ),
    rnorm(20)
  )
  expect_true(length(ctrl$index) > 0)
  expect_all_true(lengths(ctrl$indexOut) == 2)
})

test_that("make_resamples validates user-supplied indices", {
  y <- factor(rep(c("a", "b"), each = 10))

  # supplied indices must be integers
  expect_snapshot(
    caret:::make_resamples(
      trainControl(method = "cv", index = list(Fold1 = c(1.5, 2.5))),
      y
    ),
    error = TRUE
  )
  expect_snapshot(
    caret:::make_resamples(
      trainControl(
        method = "cv",
        index = list(Fold1 = 1:5L),
        indexOut = list(Fold1 = c(6.5, 7.5))
      ),
      y
    ),
    error = TRUE
  )
  # "custom" only makes sense alongside supplied indices
  expect_snapshot(
    caret:::make_resamples(trainControl(method = "custom"), y),
    error = TRUE
  )
  # and an unknown method is rejected
  expect_snapshot(
    caret:::make_resamples(trainControl(method = "nope"), y),
    error = TRUE
  )
})

test_that("make_resamples names unnamed user indices", {
  y <- factor(rep(c("a", "b"), each = 10))
  ctrl <- caret:::make_resamples(
    trainControl(method = "cv", index = list(1:10L, 11:20L)),
    y
  )
  expect_named(ctrl$index, c("Resample1", "Resample2"))
  expect_named(ctrl$indexOut, c("Resample1", "Resample2"))
})

test_that("make_resamples computes hold-outs for a Surv outcome", {
  set.seed(1902)
  y <- fake_surv(rnorm(30, 10))
  ctrl <- caret:::make_resamples(trainControl(method = "cv", number = 3), y)
  expect_length(ctrl$indexOut, 3)
})

test_that("createDataPartition needs at least two bins", {
  set.seed(4855)
  # fewer than two groups is raised to two
  idx <- createDataPartition(rnorm(40), p = 0.5, groups = 1)
  expect_length(idx$Resample1, 20)
})

test_that("createFolds falls back to two bins for small numeric samples", {
  set.seed(3094)
  # with 8 points and 5 folds there is not enough data for finer binning
  folds <- createFolds(rnorm(8), k = 5)
  expect_length(folds, 5)
  expect_setequal(unlist(folds), 1:8)
})

test_that("make_resamples names user-supplied hold-out indices", {
  y <- factor(rep(c("a", "b"), each = 10))
  ctrl <- caret:::make_resamples(
    trainControl(
      method = "cv",
      index = list(1:10L, 11:20L),
      indexOut = list(11:20L, 1:10L)
    ),
    y
  )
  expect_named(ctrl$indexOut, c("Resample1", "Resample2"))
})
