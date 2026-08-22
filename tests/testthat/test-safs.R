test_that("safsControl errors working", {
  skip_on_cran()
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
  skip_on_cran()
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

# ------------------------------------------------------------------------------
# the annealing helpers

test_that("safs_prob always accepts an improvement", {
  # a lower value is better, so a better candidate is taken outright
  expect_identical(safs_prob(old = 10, new = 5), 1)

  # a worse candidate is accepted with a probability that falls as the search
  # goes on
  early <- safs_prob(old = 10, new = 11, iteration = 1)
  late <- safs_prob(old = 10, new = 11, iteration = 20)
  expect_lt(early, 1)
  expect_lt(late, early)
  expect_gt(late, 0)
})

test_that("safs_initial and safs_perturb move around the variable set", {
  # the initial subset is a proportion of the variables, rounded up
  start <- withr::with_seed(3092, safs_initial(vars = 20, prob = 0.2))
  expect_length(start, 5)
  expect_in(start, 1:20)
  # sorted, so the subset reads the same however it was drawn
  expect_identical(start, sort(start))

  # perturbing flips a given number of positions in or out
  moved <- withr::with_seed(7314, safs_perturb(start, vars = 20, number = 3))
  expect_in(moved, 1:20)
  expect_identical(
    length(union(setdiff(start, moved), setdiff(moved, start))),
    3L
  )
})

test_that("sa_bl_correct measures each iteration against the first", {
  withr::local_package("plyr")

  # two resamples, three iterations each; the first iteration is the baseline
  internal <- data.frame(
    Iter = rep(1:3, 2),
    RMSE = c(10, 8, 6, 20, 15, 25),
    Resample = rep(c("Fold1", "Fold2"), each = 3),
    Size = 5,
    stringsAsFactors = FALSE
  )

  out <- caret:::sa_bl_correct(internal)
  # within each resample the baseline becomes zero and the rest are differences
  expect_identical(out$RMSE[out$Resample == "Fold1"], c(0, -2, -4))
  expect_identical(out$RMSE[out$Resample == "Fold2"], c(0, -5, 5))
  # the bookkeeping columns are left alone
  expect_identical(out$Iter, rep(1:3, 2))
  expect_all_equal(out$Size, 5)
})

test_that("sa_func_check reports the functions a search needs", {
  expect_invisible(caret:::sa_func_check(caretSA))

  # a missing element is named
  expect_snapshot(
    caret:::sa_func_check(caretSA[c("fit", "pred")]),
    error = TRUE
  )

  # so are the wrong arguments
  wrong_args <- caretSA
  wrong_args$perturb <- function(x, y) x
  expect_snapshot(caret:::sa_func_check(wrong_args), error = TRUE)
})

test_that("sa_func_check accepts a starting subset instead of a function", {
  # `initial` may be the variable subset to start from, and then it has no
  # arguments to check
  fixed_start <- caretSA
  fixed_start$initial <- 1:3
  expect_invisible(caret:::sa_func_check(fixed_start))
})

# ------------------------------------------------------------------------------
# methods on a fitted search (fixtures live in helper-feature-selection.R)

test_that("print.safs describes the search", {
  skip_on_cran()

  sa <- safs_fixture()
  # the performance values are resampled floats, so they are masked
  expect_snapshot(print(sa), transform = mask_decimals)
})

test_that("print.safs names the classes and the restart rule", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  # a classification search prints the class levels; `improve` adds the restart
  # line, and `holdout` the internal-subsampling line
  sa <- safs_fixture(
    classification = TRUE,
    improve = 2,
    holdout = 0.2
  )
  expect_snapshot(print(sa), transform = mask_decimals)
})

test_that("print.safs can be asked for more or fewer variables", {
  skip_on_cran()

  sa <- safs_fixture()
  # `top` limits the variables listed in both summaries
  one <- capture.output(print(sa, top = 1))
  expect_true(any(grepl("the top 1 selected variables", one)))
})

test_that("print.safs says when the iteration was chosen by hand", {
  skip_on_cran()

  sa <- safs_fixture()
  new_iter <- ifelse(sa$optIter == 1, 2, 1)
  dat <- fs_data()
  manual <- update(sa, iter = new_iter, x = dat[, 1:4], y = dat$y)

  expect_false(manual$auto)
  expect_match(
    paste(capture.output(print(manual)), collapse = " "),
    "Best iteration chosen manually"
  )
})

test_that("varImp.safs ranks the variables by their effect on performance", {
  skip_on_cran()

  sa <- safs_fixture()
  vi <- varImp(sa)
  expect_s3_class(vi, "data.frame")
  expect_named(vi, "RMSE")
  expect_setequal(rownames(vi), paste0("x", 1:4))
  # RMSE is minimized, so the importances are negated and sorted downwards. A
  # variable that never moved in or out often enough has no estimate.
  present <- vi[[1]][!is.na(vi[[1]])]
  expect_gt(length(present), 0)
  expect_identical(present, sort(present, decreasing = TRUE))

  # a metric that is maximized keeps its sign
  vi_max <- varImp(sa, metric = "Rsquared", maximize = TRUE)
  expect_named(vi_max, "Rsquared")
})

test_that("varImp.safs needs the differences to have been computed", {
  skip_on_cran()

  sa <- safs_fixture(differences = FALSE)
  expect_snapshot(varImp(sa), error = TRUE)
})

test_that("plot.safs draws the search history", {
  skip_on_cran()

  sa <- safs_fixture()

  # by default both estimates are shown, coloured
  gg <- plot(sa)
  expect_s3_class(gg, "ggplot")
  built <- ggplot2::ggplot_build(gg)
  expect_identical(built$plot$labels$x, "Iteration")
  expect_contains(names(built$data[[1]]), "colour")

  # one estimate at a time
  expect_s3_class(plot(sa, estimate = "internal"), "ggplot")
  expect_s3_class(plot(sa, estimate = "external"), "ggplot")

  # ggplot() is the same plot by another name
  expect_s3_class(ggplot2::ggplot(sa), "ggplot")
})

test_that("plot.safs can return the data or a lattice plot", {
  skip_on_cran()

  sa <- safs_fixture()

  # the data behind the plot, one row per iteration per resample
  dat <- plot(sa, output = "data")
  expect_contains(names(dat), c("Iter", "Resample", "Estimate"))

  drawn <- plot(sa, output = "lattice")
  expect_s3_class(drawn, "trellis")
  expect_identical(drawn$xlab, "Iteration")
  draw_trellis(drawn)
  # and with a single estimate there are no groups to colour
  draw_trellis(plot(sa, estimate = "internal", output = "lattice"))
})

test_that("plot.safs checks the metric it was asked for", {
  skip_on_cran()

  sa <- safs_fixture()
  expect_snapshot(plot(sa, metric = "Bogus"), error = TRUE)
  expect_snapshot(
    plot(sa, metric = "Bogus", estimate = "internal"),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------
# control validation and the external fitness function

test_that("safs needs a seed per resample plus one", {
  skip_on_cran()

  dat <- fs_data()
  ctrl <- safsControl(
    functions = caretSA,
    method = "cv",
    number = 3,
    seeds = 1:2
  )
  expect_snapshot(
    safs(
      x = dat[, 1:4],
      y = dat$y,
      safsControl = ctrl,
      iters = 2,
      method = "lm",
      trControl = trainControl(method = "cv", number = 3)
    ),
    error = TRUE
  )
})

test_that("safs names an unnamed external fitness result", {
  skip_on_cran()

  dat <- fs_data()
  unnamed <- caretSA
  unnamed$fitness_extern <- function(data, lev = NULL, model = NULL) {
    unname(defaultSummary(data, lev, model))
  }
  ctrl <- safsControl(functions = unnamed, method = "cv", number = 3)

  set.seed(3364)
  # two warnings: the unnamed result, then the metric that is therefore missing
  expect_snapshot(
    fit <- safs(
      x = dat[, 1:4],
      y = dat$y,
      safsControl = ctrl,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv", number = 3)
    )
  )
  # the results are named for their position instead
  expect_in("external1", names(fit$external))
})

test_that("safs falls back when the external metric is not computed", {
  skip_on_cran()

  dat <- fs_data()
  ctrl <- safsControl(
    functions = caretSA,
    method = "cv",
    number = 3,
    metric = c(internal = "RMSE", external = "Bogus")
  )

  set.seed(9903)
  expect_snapshot_warning(
    fit <- safs(
      x = dat[, 1:4],
      y = dat$y,
      safsControl = ctrl,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv", number = 3)
    )
  )
  expect_identical(unname(fit$control$metric["external"]), "RMSE")
})

test_that("safs can hold out data for the internal fitness", {
  skip_on_cran()

  dat <- fs_data(n = 80)
  ctrl <- safsControl(
    functions = caretSA,
    method = "cv",
    number = 3,
    holdout = 0.25
  )

  set.seed(7551)
  fit <- safs(
    x = dat[, 1:4],
    y = dat$y,
    safsControl = ctrl,
    iters = 3,
    differences = FALSE,
    method = "lm",
    trControl = trainControl(method = "cv", number = 3)
  )
  expect_identical(fit$control$holdout, 0.25)
  expect_s3_class(fit, "safs")
})

test_that("safs reports each iteration when asked", {
  skip_on_cran()

  dat <- fs_data()
  ctrl <- safsControl(
    functions = caretSA,
    method = "cv",
    number = 2,
    verbose = TRUE
  )

  # The per-iteration lines carry resampled performance values, and whether a
  # worse subset is accepted depends on comparing them, so the text is matched
  # rather than snapshotted.
  set.seed(2242)
  progress <- capture.output(
    fit <- safs(
      x = dat[, 1:4],
      y = dat$y,
      safsControl = ctrl,
      iters = 3,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv", number = 3)
    )
  )
  joined <- paste(progress, collapse = " ")
  # the first iteration reports the subset size, later ones the change
  expect_match(joined, "Fold1")
  expect_match(joined, "->")
  expect_s3_class(fit, "safs")
})
