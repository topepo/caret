test_that("errors working", {
  skip_on_cran()
  trainSet = 1:3

  expect_snapshot(
    distData <- classDist(
      iris[trainSet, 1:4],
      iris$Species[trainSet]
    ),
    error = TRUE
  )
})

test_that("Object matches expectations - factor y", {
  skip_on_cran()
  trainSet <- sample(1:150, 100)
  x = iris[trainSet, 1:4]
  y = iris$Species[trainSet]

  distData <- classDist(x, y, pca = FALSE)

  # values
  expect_length(distData$values, length(levels(y)))

  # classes
  expect_in(distData$classes, levels(y))

  ## n
  expect_all_true(as.vector(table(y) == distData$n))

  ## cuts
  expect_null(distData$cuts)

  ## p
  expect_equal(distData$p, ncol(x))

  ## PCA - FALSE
  expect_null(distData$pca)

  ## PCA - TRUE
  distData2 <- classDist(x, y, pca = TRUE)
  PCA <- prcomp(x, center = TRUE, scale = TRUE, tol = sqrt(.Machine$double.eps))

  expect_equal(distData2$pca$sdev, PCA$sdev)
  expect_equal(distData2$pca$rotation, PCA$rotation)
})


test_that("Object matches expectations - numeric y", {
  skip_on_cran()
  trainSet <- sample(1:150, 100)
  x = iris[trainSet, 1:4]
  y = as.numeric(iris$Species[trainSet])
  groups = 4

  distData <- classDist(x, y, pca = FALSE, groups = groups)

  # values
  expect_length(distData$values, length(unique(y)) - 1)

  # classes
  expect_in(distData$classes, as.character(seq(0, 100, 25))[2:5])

  ## p
  expect_equal(distData$p, ncol(x))

  ## PCA - FALSE
  expect_null(distData$pca)

  ## PCA - TRUE
  distData2 <- classDist(x, y, pca = TRUE)
  PCA <- prcomp(x, center = TRUE, scale = TRUE, tol = sqrt(.Machine$double.eps))

  expect_equal(distData2$pca$sdev, PCA$sdev)
  expect_equal(distData2$pca$rotation, PCA$rotation)
})


test_that("predictions", {
  skip_on_cran()
  trainSet <- sample(1:150, 100)
  x = iris[trainSet, 1:4]
  y = iris$Species[trainSet]
  groups = 4

  distData <- classDist(x, y, pca = FALSE, groups = groups)
  distData2 <- classDist(x, y, pca = TRUE, groups = groups)

  ## PCA and non-PCA preds match
  expect_equal(predict(distData, x), predict(distData2, x), tolerance = 0.0001)
})

test_that("print.classDist describes a factor-outcome model", {
  skip_on_cran()

  # a fixed subset keeps the printed sample counts deterministic
  train_set <- c(1:30, 51:80, 101:130)
  x <- iris[train_set, 1:4]
  y <- iris$Species[train_set]

  expect_snapshot(print(classDist(x, y, pca = FALSE)))
  # with PCA the retained component count is reported too
  expect_snapshot(print(classDist(x, y, pca = TRUE)))
})

test_that("print.classDist describes a binned numeric outcome", {
  skip_on_cran()

  train_set <- c(1:30, 51:80, 101:130)
  x <- iris[train_set, 1:4]
  y <- as.numeric(iris$Species[train_set])

  expect_snapshot(print(classDist(x, y, pca = FALSE, groups = 3)))
})

test_that("classDist needs more rows than columns in every class", {
  skip_on_cran()

  # class "b" has fewer rows than predictors, so its covariance is singular
  x <- data.frame(a = rnorm(8), b = rnorm(8), c = rnorm(8), d = rnorm(8))
  y <- factor(c(rep("a", 6), rep("b", 2)))
  expect_snapshot(classDist(x, y), error = TRUE)
})

test_that("classDist reports an uninvertible covariance matrix", {
  skip_on_cran()

  # duplicated predictors make the covariance matrix singular
  x <- data.frame(a = c(1, 2, 3, 4, 5, 6))
  x$b <- x$a
  x$c <- x$a
  y <- factor(rep(c("one", "two"), each = 3))
  expect_snapshot(classDist(x, y), error = TRUE)
})
