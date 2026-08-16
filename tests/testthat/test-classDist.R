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
