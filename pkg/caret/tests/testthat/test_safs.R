context('safs')

test_that("safsControl errors working", {

  expect_error(safsControl(method = "larry")
                ,"method should be one of")

  expect_error(safsControl(metric = c("larry", "harry", "moe"))
                ,"should be a two-element named vector")

  expect_error(safsControl(maximize = c("larry", "harry", "moe"))
                ,"should be a two-element named vector")

  expect_error(safsControl(holdout = -1)
                ,"'holdout' should be")

  expect_error(safsControl(improve = 1)
                ,"'improve' should be")

})

test_that("high level tests", {

  expect_silent(pop <- safs_initial(vars = 10, popSize = 10))

  expect_silent(selected_vars <- safs_initial(vars = 10 , prob = 0.2))

  expect_silent(safs_perturb(selected_vars, vars = 10, number = 1))

  set.seed(1)
  train_data <- twoClassSim(100, noiseVars = 10)
  test_data  <- twoClassSim(10,  noiseVars = 10)

  ## A short example
  expect_silent(
    ctrl <- safsControl(functions = rfSA,
                        method = "cv",
                        number = 3)
  )

  # rf_search <- safs(x = train_data[, -ncol(train_data)],
  #                   y = train_data$Class,
  #                   iters = 3,
  #                   safsControl = ctrl)

})

test_that("" , {
  skip_on_cran()

  set.seed(1)
  train_data <- caret::twoClassSim(100, noiseVars = 10)
  test_data  <- caret::twoClassSim(10,  noiseVars = 10)

  ## A short example
  ctrl <- caret::safsControl(functions = rfSA,
                             method = "cv",
                             number = 3)

  expect_warning({
    set.seed(2)
    caret::safs(x = train_data[, -ncol(train_data)],
                y = train_data$Class,
                iters = 3,
                safsControl = ctrl)
  },
  "Variable differences could not be compute"
  )
  expect_silent({
    set.seed(2)
    caret::safs(x = train_data[, -ncol(train_data)],
                y = train_data$Class,
                iters = 5,
                safsControl = ctrl)
  }
  )
})

