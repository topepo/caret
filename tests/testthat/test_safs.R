test_that("safsControl errors working", {
  expect_snapshot(safsControl(method = "larry"), error = TRUE)

  expect_snapshot(
    safsControl(metric = c("larry", "harry", "moe")),
    error = TRUE
  )

  expect_snapshot(
    safsControl(maximize = c("larry", "harry", "moe")),
    error = TRUE
  )

  expect_snapshot(safsControl(holdout = -1), error = TRUE)

  expect_snapshot(safsControl(improve = 1), error = TRUE)
})

test_that("high level tests", {
  expect_silent(pop <- safs_initial(vars = 10, popSize = 10))

  expect_silent(selected_vars <- safs_initial(vars = 10, prob = 0.2))

  expect_silent(safs_perturb(selected_vars, vars = 10, number = 1))

  set.seed(1)
  train_data <- twoClassSim(100, noiseVars = 10)
  test_data <- twoClassSim(10, noiseVars = 10)

  ## A short example
  expect_silent(
    ctrl <- safsControl(functions = rfSA, method = "cv", number = 3)
  )

  # rf_search <- safs(x = train_data[, -ncol(train_data)],
  #                   y = train_data$Class,
  #                   iters = 3,
  #                   safsControl = ctrl)
})

test_that("safs runs with random-forest functions", {
  skip_on_cran()

  set.seed(1)
  train_data <- caret::twoClassSim(100, noiseVars = 10)
  test_data <- caret::twoClassSim(10, noiseVars = 10)

  ## A short example
  ctrl <- caret::safsControl(functions = rfSA, method = "cv", number = 3)

  expect_snapshot_warning({
    set.seed(2)
    caret::safs(
      x = train_data[, -ncol(train_data)],
      y = train_data$Class,
      iters = 3,
      safsControl = ctrl
    )
  })
  expect_silent({
    set.seed(2)
    caret::safs(
      x = train_data[, -ncol(train_data)],
      y = train_data$Class,
      iters = 5,
      safsControl = ctrl
    )
  })
})
