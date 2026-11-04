# Tests for plot.train. The function builds lattice objects, so the tests check
# that a trellis object is returned for each plot type (and cover the data-prep
# and error paths); the visual output itself isn't asserted.

test_that("plot.train draws scatter and line plots for a tuned model", {
  skip_on_cran()

  set.seed(1)
  fit <- train(
    Species ~ .,
    data = iris,
    method = "knn",
    tuneLength = 4,
    trControl = trainControl(method = "cv", number = 3)
  )

  expect_s3_class(draw_trellis(plot(fit, plotType = "scatter")), "trellis")
  expect_s3_class(draw_trellis(plot(fit, plotType = "line")), "trellis")
  # an unknown plot type is rejected
  expect_snapshot(plot(fit, plotType = "nope"), error = TRUE)
})

test_that("plot.train draws a level plot for two tuning parameters", {
  skip_on_cran()
  skip_if_not_installed("earth")

  set.seed(1)
  dat <- SLC14_1(80)
  fit <- suppressWarnings(suppressMessages(train(
    y ~ .,
    data = dat,
    method = "earth",
    tuneGrid = expand.grid(nprune = 2:4, degree = 1:2),
    trControl = trainControl(method = "cv", number = 3)
  )))

  expect_s3_class(draw_trellis(plot(fit, plotType = "level")), "trellis")
  expect_s3_class(draw_trellis(plot(fit, plotType = "scatter")), "trellis")
})

test_that("plot.train errors when no tuning parameter varies", {
  skip_on_cran()

  set.seed(1)
  dat <- SLC14_1(80)
  fit <- train(
    y ~ .,
    data = dat,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )

  expect_snapshot(plot(fit), error = TRUE)
})

# ------------------------------------------------------------------------------
# the parameter labels plot.train tidies up

test_that("plot.train relabels a naive Bayes kernel switch", {
  skip_on_cran()
  skip_if_not_installed("klaR")

  dat <- engine_three_class()
  set.seed(4118)
  fit <- suppressWarnings(train(
    Species ~ .,
    data = dat,
    method = "nb",
    tuneGrid = expand.grid(fL = 0, usekernel = c(TRUE, FALSE), adjust = 1),
    trControl = trainControl(method = "cv", number = 3)
  ))

  # the logical parameter is drawn as "Nonparametric"/"Gaussian" on the axis,
  # even though the results table holds TRUE/FALSE
  drawn <- plot(fit)
  expect_s3_class(drawn, "trellis")
  expect_setequal(drawn$x.limits, c("Gaussian", "Nonparametric"))
  expect_setequal(as.character(fit$results$usekernel), c("TRUE", "FALSE"))
  draw_trellis(drawn)
})

test_that("plot.train relabels the other logical parameters", {
  skip_on_cran()

  # These branches only relabel a column of `x$results` for the plot, keyed on
  # the method name, so the objects are doctored rather than paying for a fit
  # per model (qrnn and M5 are not installed here in any case; nb and C5.0 get
  # real fits above).
  dat <- engine_three_class()
  set.seed(6210)
  base <- train(
    Species ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "cv", number = 3)
  )

  relabelled <- list(
    gam = list(select = c(TRUE, FALSE), method = "GCV.Cp"),
    qrnn = list(bag = c(TRUE, FALSE), n.hidden = 1, penalty = 0),
    M5 = list(rules = c("Yes", "No"), pruned = "Yes", smoothed = "Yes")
  )

  for (nm in names(relabelled)) {
    fake <- base
    fake$method <- nm
    params <- relabelled[[nm]]
    fake$results <- data.frame(
      params,
      Accuracy = c(0.7, 0.8),
      Kappa = c(0.5, 0.6),
      stringsAsFactors = FALSE
    )
    fake$modelInfo$parameters <- data.frame(
      parameter = names(params),
      class = "character",
      label = names(params),
      stringsAsFactors = FALSE
    )
    fake$bestTune <- fake$results[2, names(params), drop = FALSE]
    drawn <- plot(fake)
    expect_s3_class(drawn, "trellis")
    draw_trellis(drawn)
  }
})

# ------------------------------------------------------------------------------
# more than two tuning parameters

test_that("plot.train conditions on the extra tuning parameters", {
  skip_on_cran()
  skip_if_not_installed("C50")

  # C5.0 has three parameters, one of them character, so this covers the panel
  # strips, the winnow relabelling and the conversion of a character parameter
  dat <- engine_two_class(120)
  grid <- expand.grid(
    trials = c(1, 5),
    model = c("tree", "rules"),
    winnow = c(TRUE, FALSE)
  )
  set.seed(9928)
  fit <- train(
    Class ~ .,
    data = dat,
    method = "C5.0",
    tuneGrid = grid,
    trControl = trainControl(method = "cv", number = 2)
  )

  # with three varying parameters the extras become panel strips
  for (type in c("scatter", "line")) {
    drawn <- plot(fit, plotType = type)
    expect_s3_class(drawn, "trellis")
    expect_gt(prod(dim(drawn)), 1)
    draw_trellis(drawn)
  }

  # the strips can name the parameters as well as their values
  draw_trellis(plot(fit, nameInStrip = TRUE))

  # and a level plot uses the two most-varied parameters
  draw_trellis(plot(fit, plotType = "level"))
})

test_that("plot.train transforms the x axis when asked", {
  skip_on_cran()

  dat <- engine_three_class()
  set.seed(3355)
  fit <- train(
    Species ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = c(1, 3, 9, 27)),
    trControl = trainControl(method = "cv", number = 3)
  )

  drawn <- plot(fit, xTrans = log10)
  expect_s3_class(drawn, "trellis")
  draw_trellis(drawn)
})

test_that("plot.train needs something to plot", {
  skip_on_cran()

  reg <- engine_regression(30)
  # glm has no tuning parameters at all
  set.seed(7712)
  none <- train(
    y ~ .,
    data = reg,
    method = "glm",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_snapshot(plot(none), error = TRUE)

  # and a level plot needs two parameters that vary
  set.seed(7712)
  one <- train(
    y ~ .,
    data = reg,
    method = "knn",
    tuneGrid = data.frame(k = c(3, 5)),
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_snapshot(plot(one, plotType = "level"), error = TRUE)
})

test_that("plot.train warns about adaptive resampling", {
  skip_on_cran()
  skip_if_not_installed("nlme")

  dat <- engine_three_class()
  set.seed(5866)
  fit <- suppressWarnings(train(
    Species ~ .,
    data = dat,
    method = "knn",
    tuneGrid = data.frame(k = c(1, 9, 17)),
    trControl = trainControl(
      method = "adaptive_cv",
      number = 5,
      adaptive = list(min = 3, alpha = 0.05, method = "gls", complete = TRUE)
    )
  ))

  # the candidates were not all scored on the same resamples
  expect_snapshot_warning(drawn <- plot(fit))
  draw_trellis(drawn)
})
