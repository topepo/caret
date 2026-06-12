context('gafs')

test_that("gafsControl errors working", {

  expect_error(gafsControl(method = "larry")
                ,"method should be one of")

  expect_error(gafsControl(metric = c("larry", "harry", "moe"))
                ,"should be a two-element named vector")

  expect_error(gafsControl(maximize = c("larry", "harry", "moe"))
                ,"should be a two-element named vector")

})

test_that("high level tests", {

  expect_silent(pop <- gafs_initial(vars = 10, popSize = 10))
  expect_silent(gafs_lrSelection(population = pop, fitness = 1:10))
  expect_silent(gafs_spCrossover(population = pop, fitness = 1:10, parents = 1:2))


  train_data <- twoClassSim(10, noiseVars = 1)
  test_data  <- twoClassSim(10,  noiseVars = 2)

  expect_silent(
    ctrl <- gafsControl(functions = rfGA,
                     method = "cv",
                     number = 3)
    )

  ## Too slow
  # expect_silent(
  #   rf_search <- gafs(x = train_data[, -ncol(train_data)],
  #                      y = train_data$Class,
  #                      iters = 2,
  #                      gafsControl = ctrl)
  #   )

})
