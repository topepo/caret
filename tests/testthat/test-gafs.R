test_that("gafsControl errors working", {
  skip_on_cran()
  expect_snapshot(gafsControl(method = "larry"), error = TRUE)

  expect_snapshot(
    gafsControl(metric = c("larry", "harry", "moe")),
    error = TRUE
  )

  expect_snapshot(
    gafsControl(maximize = c("larry", "harry", "moe")),
    error = TRUE
  )
})

test_that("high level tests", {
  skip_on_cran()
  expect_silent(pop <- gafs_initial(vars = 10, popSize = 10))
  expect_silent(gafs_lrSelection(population = pop, fitness = 1:10))
  expect_silent(gafs_spCrossover(
    population = pop,
    fitness = 1:10,
    parents = 1:2
  ))

  train_data <- twoClassSim(10, noiseVars = 1)
  test_data <- twoClassSim(10, noiseVars = 2)

  expect_silent(
    ctrl <- gafsControl(functions = rfGA, method = "cv", number = 3)
  )

  ## Too slow
  # expect_silent(
  #   rf_search <- gafs(x = train_data[, -ncol(train_data)],
  #                      y = train_data$Class,
  #                      iters = 2,
  #                      gafsControl = ctrl)
  #   )
})

# ------------------------------------------------------------------------------
# the genetic operators

test_that("the selection operators pick a new population of the same size", {
  pop <- withr::with_seed(6104, gafs_initial(vars = 8, popSize = 6))
  fitness <- c(1, 5, 3, 9, 7, 2)

  # each operator returns a population and the fitness that came with it
  for (op in list(
    gafs_lrSelection,
    gafs_nlrSelection,
    gafs_rwSelection,
    gafs_tourSelection
  )) {
    out <- withr::with_seed(2951, op(population = pop, fitness = fitness))
    expect_named(out, c("population", "fitness"))
    expect_identical(dim(out$population), dim(pop))
    # every selected row is one of the originals, with its own fitness
    expect_in(out$fitness, fitness)
    expect_identical(nrow(out$population), length(out$fitness))
  }
})

test_that("tournament selection favours the fittest of each sample", {
  pop <- withr::with_seed(3327, gafs_initial(vars = 8, popSize = 4))
  # with k equal to the population size, every tournament sees everyone, so the
  # best individual wins every time
  out <- gafs_tourSelection(pop, fitness = c(1, 2, 3, 10), k = 4)
  expect_all_equal(out$fitness, 10)
})

test_that("the crossover operators return two children", {
  pop <- withr::with_seed(8817, gafs_initial(vars = 8, popSize = 4))
  fitness <- c(1, 5, 3, 9)

  sp <- withr::with_seed(4402, gafs_spCrossover(pop, fitness, parents = 1:2))
  expect_named(sp, c("children", "fitness"))
  expect_identical(dim(sp$children), c(2L, 8L))
  # the children are made of their parents' values
  expect_in(as.vector(sp$children), c(0, 1))

  un <- withr::with_seed(4402, gafs_uCrossover(pop, parents = 1:2))
  expect_identical(dim(un$children), c(2L, 8L))
  # the uniform operator swaps whole positions, so the pair together still
  # holds what the parents held at each position
  expect_setequal(
    as.vector(un$children[, 1]),
    as.vector(pop[1:2, 1])
  )
})

test_that("single-point crossover copies the parents at the extremes", {
  pop <- withr::with_seed(1288, gafs_initial(vars = 4, popSize = 2))
  fitness <- c(2, 8)

  # The crossover point is drawn from 0:n. At 0 the parents swap wholesale and
  # take their fitness with them; at n they are passed through unchanged. These
  # seeds are the ones that draw those two values for a four-variable problem.
  # the children are built in a double matrix, so the values are compared
  # rather than the storage type
  swapped <- withr::with_seed(1, gafs_spCrossover(pop, fitness, parents = 1:2))
  expect_equal(swapped$children, unname(pop[2:1, ]))
  expect_identical(swapped$fitness, fitness[2:1])

  passed <- withr::with_seed(2, gafs_spCrossover(pop, fitness, parents = 1:2))
  expect_equal(passed$children, unname(pop[1:2, ]))
  expect_identical(passed$fitness, fitness)
})

test_that("random mutation flips exactly one position", {
  pop <- withr::with_seed(9975, gafs_initial(vars = 10, popSize = 3))
  mutated <- withr::with_seed(5525, gafs_raMutation(pop, parent = 2))

  expect_length(mutated, 10)
  expect_identical(sum(mutated != pop[2, ]), 1L)
  expect_in(mutated, c(0, 1))
})

# ------------------------------------------------------------------------------
# the function list

test_that("ga_func_check reports the functions a search needs", {
  # caretGA is complete, so it passes
  expect_invisible(caret:::ga_func_check(caretGA))

  # dropping a required element is reported by name
  expect_snapshot(
    caret:::ga_func_check(caretGA[c("fit", "pred")]),
    error = TRUE
  )
})

test_that("ga_func_check checks each function's arguments", {
  wrong_args <- caretGA
  wrong_args$mutation <- function(x, y) x

  expect_snapshot(caret:::ga_func_check(wrong_args), error = TRUE)
})

# ------------------------------------------------------------------------------
# methods on a fitted search (fixtures live in helper-feature-selection.R)

test_that("print.gafs describes the search", {
  skip_on_cran()

  ga <- gafs_fixture()
  expect_snapshot(print(ga), transform = mask_decimals)
})

test_that("print.gafs names the classes and a varying mutation rate", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  dat <- fs_data(classification = TRUE)
  ctrl <- gafsControl(functions = caretGA, method = "cv", number = 3)
  set.seed(9214)
  ga <- gafs(
    x = dat[, 1:4],
    y = dat$y,
    gafsControl = ctrl,
    popSize = 4,
    iters = 2,
    # a probability that changes with the generation prints as "variable"
    pmutation = function(generation) 0.5 / generation,
    differences = TRUE,
    method = "lda",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_snapshot(print(ga), transform = mask_decimals)
})

test_that("print.gafs says when the generation was chosen by hand", {
  skip_on_cran()

  ga <- gafs_fixture()
  new_iter <- ifelse(ga$optIter == 1, 2, 1)
  dat <- fs_data()
  manual <- update(ga, iter = new_iter, x = dat[, 1:4], y = dat$y)

  expect_false(manual$auto)
  expect_match(
    paste(capture.output(print(manual)), collapse = " "),
    "Best iteration chosen manually"
  )
})

test_that("varImp.gafs ranks the variables by their effect on performance", {
  skip_on_cran()

  # a longer search, so every variable has been in and out of the population
  # often enough for the differences to be estimated
  ga <- gafs_fixture(popSize = 8, iters = 8)
  vi <- varImp(ga)
  expect_s3_class(vi, "data.frame")
  expect_named(vi, "RMSE")
  # RMSE is minimized, so the importances are negated and sorted downwards. A
  # variable that never moved in or out often enough has no estimate.
  present <- vi[[1]][!is.na(vi[[1]])]
  expect_gt(length(present), 0)
  expect_identical(present, sort(present, decreasing = TRUE))
})

test_that("varImp.gafs needs the differences to have been computed", {
  skip_on_cran()

  ga <- gafs_fixture(differences = FALSE)
  expect_snapshot(varImp(ga), error = TRUE)
})

test_that("plot.gafs draws the search history", {
  skip_on_cran()

  ga <- gafs_fixture()

  gg <- plot(ga)
  expect_s3_class(gg, "ggplot")
  built <- ggplot2::ggplot_build(gg)
  # the GA counts generations rather than iterations
  expect_identical(built$plot$labels$x, "Generation")
  expect_contains(names(built$data[[1]]), "colour")

  expect_s3_class(plot(ga, estimate = "internal"), "ggplot")
  expect_s3_class(ggplot2::ggplot(ga), "ggplot")

  dat <- plot(ga, output = "data")
  expect_contains(names(dat), c("Iter", "Resample", "Estimate"))

  drawn <- plot(ga, output = "lattice")
  expect_s3_class(drawn, "trellis")
  draw_trellis(drawn)
  draw_trellis(plot(ga, estimate = "internal", output = "lattice"))
})

test_that("plot.gafs checks the metric it was asked for", {
  skip_on_cran()

  ga <- gafs_fixture()
  expect_snapshot(plot(ga, metric = "Bogus"), error = TRUE)
  expect_snapshot(
    plot(ga, metric = "Bogus", estimate = "internal"),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------
# control validation, suggestions and elitism

test_that("gafs needs a seed per resample plus one", {
  skip_on_cran()

  dat <- fs_data()
  ctrl <- gafsControl(
    functions = caretGA,
    method = "cv",
    number = 3,
    seeds = 1:2
  )
  expect_snapshot(
    gafs(
      x = dat[, 1:4],
      y = dat$y,
      gafsControl = ctrl,
      popSize = 4,
      iters = 2,
      method = "lm",
      trControl = trainControl(method = "cv", number = 3)
    ),
    error = TRUE
  )
})

test_that("gafs checks a suggested starting population", {
  skip_on_cran()

  dat <- fs_data()
  ctrl <- gafsControl(functions = caretGA, method = "cv", number = 3)

  # One column per predictor is expected; this has too few. Not snapshotted: the
  # error is raised inside the resampling loop, so foreach re-raises it with the
  # loop body as its call, and covr rewrites that when it instruments the
  # package.
  expect_error(
    gafs(
      x = dat[, 1:4],
      y = dat$y,
      gafsControl = ctrl,
      popSize = 4,
      iters = 2,
      suggestions = matrix(1, nrow = 2, ncol = 2),
      method = "lm",
      trControl = trainControl(method = "cv", number = 3)
    ),
    "do not match number of variables"
  )
})

test_that("gafs accepts a suggested starting population", {
  skip_on_cran()

  dat <- fs_data()
  ctrl <- gafsControl(functions = caretGA, method = "cv", number = 3)

  set.seed(1058)
  fit <- gafs(
    x = dat[, 1:4],
    y = dat$y,
    gafsControl = ctrl,
    popSize = 4,
    iters = 2,
    # two of the four individuals are given, the rest are drawn
    suggestions = matrix(c(1, 0, 1, 0, 0, 1, 0, 1), nrow = 2, byrow = TRUE),
    differences = FALSE,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_s3_class(fit, "gafs")
})

test_that("gafs names an unnamed external fitness result", {
  skip_on_cran()

  dat <- fs_data()
  unnamed <- caretGA
  unnamed$fitness_extern <- function(data, lev = NULL, model = NULL) {
    unname(defaultSummary(data, lev, model))
  }
  ctrl <- gafsControl(functions = unnamed, method = "cv", number = 3)

  set.seed(3364)
  # two warnings: the unnamed result, then the metric that is therefore missing
  expect_snapshot(
    fit <- gafs(
      x = dat[, 1:4],
      y = dat$y,
      gafsControl = ctrl,
      popSize = 4,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv", number = 3)
    )
  )
  expect_in("external1", names(fit$external))
})

test_that("gafs falls back when the external metric is not computed", {
  skip_on_cran()

  dat <- fs_data()
  ctrl <- gafsControl(
    functions = caretGA,
    method = "cv",
    number = 3,
    metric = c(internal = "RMSE", external = "Bogus")
  )

  set.seed(9903)
  expect_snapshot_warning(
    fit <- gafs(
      x = dat[, 1:4],
      y = dat$y,
      gafsControl = ctrl,
      popSize = 4,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv", number = 3)
    )
  )
  expect_identical(unname(fit$control$metric["external"]), "RMSE")
})

test_that("gafs can hold out data and keep an elite", {
  skip_on_cran()

  dat <- fs_data(n = 80)
  ctrl <- gafsControl(
    functions = caretGA,
    method = "cv",
    number = 3,
    holdout = 0.25
  )

  set.seed(7551)
  fit <- gafs(
    x = dat[, 1:4],
    y = dat$y,
    gafsControl = ctrl,
    popSize = 6,
    iters = 3,
    # the best individuals are carried into the next generation unchanged
    elite = 2,
    differences = FALSE,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_identical(fit$control$holdout, 0.25)
  expect_identical(fit$ga_param$elite, 2)
})

test_that("gafs reports each generation when asked", {
  skip_on_cran()

  dat <- fs_data()
  ctrl <- gafsControl(
    functions = caretGA,
    method = "cv",
    number = 2,
    verbose = TRUE
  )

  # as for safs, the lines carry resampled performance values, so the text is
  # matched rather than snapshotted
  set.seed(2242)
  progress <- capture.output(
    fit <- gafs(
      x = dat[, 1:4],
      y = dat$y,
      gafsControl = ctrl,
      popSize = 4,
      iters = 3,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv", number = 3)
    )
  )
  joined <- paste(progress, collapse = " ")
  expect_match(joined, "Fold1")
  expect_match(joined, "->")
  expect_s3_class(fit, "gafs")
})
