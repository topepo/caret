# Tests for levels.train (R/classLevels.R).

test_that("levels.train reads the stored levels of a classification fit", {
  skip_on_cran()

  set.seed(3609)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneGrid = data.frame(k = 5),
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_identical(levels(fit), levels(iris$Species))

  # without a stored levels element, the final model's obsLevels are used
  # (these carry an 'ordered' attribute, hence as.character)
  no_levels <- fit[names(fit) != "levels"]
  class(no_levels) <- "train"
  expect_identical(as.character(levels(no_levels)), levels(iris$Species))

  # without those either, the registry levels function is consulted (looked
  # up by method name when modelInfo is absent)
  no_obs <- no_levels
  no_obs$finalModel$obsLevels <- NULL
  no_obs$modelInfo <- NULL
  expect_identical(levels(no_obs), levels(iris$Species))

  # and a registry without a levels function gives NULL
  no_code <- no_levels
  no_code$finalModel$obsLevels <- NULL
  no_code$modelInfo$levels <- NULL
  expect_null(levels(no_code))
})

test_that("levels.train is NULL for regression fits", {
  skip_on_cran()

  set.seed(4470)
  fit <- train(
    mpg ~ .,
    data = mtcars,
    method = "lm",
    trControl = trainControl(method = "none")
  )
  no_levels <- fit[names(fit) != "levels"]
  class(no_levels) <- "train"
  expect_null(levels(no_levels))
})

test_that("levels.train loads the registry packages before asking for levels", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  # lda's registry entry declares MASS as a dependency, so the lookup walks
  # the package-loading loop before calling the levels function
  set.seed(9295)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "lda",
    trControl = trainControl(method = "cv", number = 3)
  )
  no_levels <- fit[names(fit) != "levels"]
  class(no_levels) <- "train"
  no_levels$finalModel$obsLevels <- NULL
  expect_identical(as.character(levels(no_levels)), levels(iris$Species))
})
