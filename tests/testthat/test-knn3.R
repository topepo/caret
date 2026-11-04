# Tests for the k-nearest-neighbour classification wrapper knn3 (R/knn3.R):
# the formula/matrix/data.frame interfaces, print, predict, and the low-level
# knn3Train engine. Its regression twin knnreg is tested in test-knnreg.R.

test_that("knn3 fits from a formula, matrix and data frame", {
  expect_s3_class(knn3(Species ~ ., data = iris, k = 5), "knn3")
  expect_s3_class(knn3(as.matrix(iris[, 1:4]), iris$Species, k = 5), "knn3")
  expect_s3_class(knn3(iris[, 1:4], iris$Species, k = 5), "knn3")

  # the default method requires a matrix/data frame/formula
  expect_snapshot(knn3(iris$Sepal.Length), error = TRUE)
})

test_that("predict.knn3 returns probabilities and classes", {
  fit <- knn3(Species ~ ., data = iris, k = 5)
  expect_identical(fit$k, 5)

  probs <- predict(fit, iris[, 1:4], type = "prob")
  expect_identical(dim(probs), c(150L, 3L))
  expect_identical(colnames(probs), levels(iris$Species))
  # each row of probabilities sums to exactly 1 (neighbour-count fractions)
  expect_identical(unname(rowSums(probs)), rep(1, nrow(iris)))

  cls <- predict(fit, iris[, 1:4], type = "class")
  expect_length(cls, nrow(iris))
  expect_identical(levels(cls), levels(iris$Species))
})

test_that("knn3Train predicts held-out cases directly", {
  # a clean split of iris: train on 100, predict the other 50
  train_x <- iris[1:100, 1:4]
  test_x <- iris[101:150, 1:4]
  train_y <- iris$Species[1:100]
  out <- knn3Train(train_x, test_x, train_y, k = 5)
  expect_length(out, 50)
})

test_that("print.knn3 shows the neighbour count and class distribution", {
  expect_snapshot(print(knn3(Species ~ ., data = iris, k = 5)))
})

test_that("knn3 handles a single predictor and single-row prediction", {
  fit <- knn3(iris[, 1, drop = FALSE], iris$Species, k = 5)
  # one predictor: still one prediction per row
  expect_length(
    predict(fit, iris[, 1, drop = FALSE], type = "class"),
    nrow(iris)
  )
  # a single new row returns a single set of class probabilities
  expect_identical(
    nrow(predict(fit, iris[1, 1, drop = FALSE], type = "prob")),
    1L
  )
})

# ------------------------------------------------------------------------------
# The C engine (src/caret.c). knn3Train() is the only route to most of it, so
# these cases are chosen to reach each branch of the neighbour search: keeping
# or discarding tied distances, the doubt threshold, and the tie ceiling.
# `fake_surv`-style structure is not needed here; the fixtures are tiny
# matrices whose distances are exact.

test_that("knn3Train keeps every tied neighbour when use.all is TRUE", {
  # the origin is 1 away from rows 2 and 3 and 5 away from row 4
  train <- matrix(c(0, 0, 1, 0, -1, 0, 0, 5), ncol = 2, byrow = TRUE)
  cl <- factor(c("a", "b", "b", "a"))
  test <- matrix(c(0, 0), ncol = 2)

  out <- knn3Train(train, test, cl, k = 3, use.all = TRUE, prob = TRUE)
  expect_identical(as.character(out), "b")
  # the two tied "b" rows and the exact match both count
  probs <- attr(out, "prob")
  expect_identical(colnames(probs), c("a", "b"))
  expect_equal(sum(probs), 1)
})

test_that("knn3Train samples among tied neighbours when use.all is FALSE", {
  train <- matrix(c(0, 0, 1, 0, -1, 0, 0, 5), ncol = 2, byrow = TRUE)
  cl <- factor(c("a", "b", "b", "a"))
  test <- matrix(c(0, 0), ncol = 2)

  set.seed(4)
  out <- knn3Train(train, test, cl, k = 3, use.all = FALSE, prob = FALSE)
  expect_in(as.character(out), c("a", "b"))
})

test_that("knn3Train handles distinct distances with use.all FALSE", {
  # every distance differs, so there is no tie at the k-th neighbour
  train <- matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE)
  cl <- factor(c("a", "b", "b", "a"))
  test <- matrix(c(0, 0), ncol = 2)

  out <- knn3Train(train, test, cl, k = 2, use.all = FALSE, prob = FALSE)
  expect_identical(as.character(out), "a")
})

test_that("knn3Train reports doubt when the vote is below l", {
  train <- matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE)
  cl <- factor(c("a", "b", "b", "a"))
  test <- matrix(c(0, 0), ncol = 2)

  # the two nearest neighbours disagree, so a required majority of 2 is unmet
  out <- knn3Train(train, test, cl, k = 2, l = 2, prob = FALSE)
  expect_true(is.na(out))
})

test_that("knn3Train stops when there are too many tied neighbours", {
  # every row sits on top of the test point, so the tie list overflows
  train <- matrix(rep(0, 2002), ncol = 2)
  cl <- factor(rep(c("a", "b"), length.out = 1001))
  test <- matrix(c(0, 0), ncol = 2)

  expect_snapshot(knn3Train(train, test, cl, k = 1), error = TRUE)
})

test_that("knn3Train validates its arguments", {
  train <- matrix(c(0, 0, 1, 1, 2, 2), ncol = 2, byrow = TRUE)
  cl <- factor(c("a", "b", "a"))
  test <- matrix(c(0, 0), ncol = 2)

  # k larger than the training set is reduced, with a warning
  expect_snapshot_warning(knn3Train(train, test, cl, k = 10, prob = FALSE))
  expect_snapshot(knn3Train(train, test, cl, k = 0), error = TRUE)
  expect_snapshot(knn3Train(train, test, cl[1:2], k = 1), error = TRUE)
  expect_snapshot(
    knn3Train(train, test[, 1, drop = FALSE], cl, k = 1),
    error = TRUE
  )
  expect_snapshot(
    knn3Train(train, test, factor(c("a", "b", NA)), k = 1),
    error = TRUE
  )
  # a bare vector of test values is treated as a single row
  expect_length(knn3Train(train, c(0, 0), cl, k = 1, prob = FALSE), 1)
})

test_that("the C engine can leave each training point out in turn", {
  # the wrappers always pass cv = FALSE, so the leave-one-out branch of the
  # C code is only reachable by calling it directly
  train <- matrix(c(0, 0, 1, 0, 2, 0, 3, 0), ncol = 2, byrow = TRUE)
  cl <- factor(c("a", "b", "b", "a"))
  nc <- length(levels(cl))

  Z <- .C(
    "knn3",
    as.integer(1),
    as.integer(0),
    as.integer(nrow(train)),
    as.integer(nrow(train)),
    as.integer(ncol(train)),
    as.double(train),
    as.integer(unclass(cl)),
    as.double(train),
    integer(nc + 1),
    as.integer(nc),
    as.integer(TRUE),
    as.integer(TRUE),
    all_vote = double(nrow(train) * nc),
    PACKAGE = "caret"
  )
  votes <- matrix(Z$all_vote, nrow = nrow(train), ncol = nc, byrow = TRUE)
  # each row is classified by its neighbours only, never by itself
  expect_identical(dim(votes), c(4L, 2L))
  expect_all_equal(rowSums(votes), 1)
})

test_that("knn3Train subsamples when more neighbours tie than k allows", {
  # all four rows sit exactly one unit from the test point, so the tie list
  # grows past k and the extras are subsampled down to k
  train <- matrix(c(1, 0, -1, 0, 0, 1, 0, -1), ncol = 2, byrow = TRUE)
  cl <- factor(c("a", "a", "b", "b"))
  test <- matrix(c(0, 0), ncol = 2)

  set.seed(9)
  out <- knn3Train(train, test, cl, k = 2, use.all = FALSE, prob = TRUE)
  expect_in(as.character(out), c("a", "b"))
  # exactly k neighbours are counted, so the probabilities are halves
  expect_equal(sum(attr(out, "prob")), 1)
})

# ------------------------------------------------------------------------------
# formula, matrix and predict branches

test_that("knn3.formula rejects a malformed formula", {
  expect_snapshot(knn3(~., data = iris), error = TRUE)
})

test_that("knn3.formula accepts a matrix as the data argument", {
  # the matrix is converted to a data frame before the model frame is built
  dat <- cbind(iris[, 1:4], Species = as.numeric(iris$Species))
  fit <- knn3(Species ~ ., data = as.matrix(dat), k = 5)
  expect_s3_class(fit, "knn3")
})

test_that("knn3.formula records the na.action that dropped rows", {
  with_na <- iris
  with_na$Sepal.Length[c(3, 9)] <- NA
  fit <- knn3(Species ~ ., data = with_na, k = 5, na.action = na.omit)
  expect_s3_class(fit, "knn3")
  expect_false(is.null(fit$na.action))
})

test_that("knn3.matrix needs a factor outcome", {
  expect_snapshot(
    knn3(as.matrix(iris[, 1:4]), as.numeric(iris$Species)),
    error = TRUE
  )
  # a data frame is coerced to a matrix first
  expect_s3_class(knn3(iris[, 1:4], iris$Species, k = 5), "knn3")
})

test_that("print.knn3 summarises a non-factor outcome", {
  # knn3 objects are normally classification, so build one by hand to reach
  # the numeric-outcome branch of the print method
  fit <- knn3(Species ~ ., data = iris, k = 5)
  fit$learn$y <- as.numeric(iris$Species)
  expect_snapshot(print(fit))
})

test_that("predict.knn3 checks the object and can reuse the model frame", {
  expect_snapshot(
    caret:::predict.knn3(structure(list(), class = "nope")),
    error = TRUE
  )

  # with no newdata the model frame is rebuilt from the formula, so its
  # variables have to be reachable from the formula's environment
  dat <- iris[, c(1, 2, 5)]
  Sepal.Length <- dat$Sepal.Length
  Sepal.Width <- dat$Sepal.Width
  Species <- dat$Species
  fit <- knn3(Species ~ Sepal.Length + Sepal.Width, data = dat, k = 5)
  expect_identical(nrow(predict(fit)), 150L)
})
