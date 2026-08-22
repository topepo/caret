# Shared fixtures (recipe_fs_x, recipe_fs_dat) live in helper-recipe_fs.R

# ------------------------------------------------------------------------------

test_that("sbf with recipes", {
  skip_on_cran()
  ctrl <- sbfControl(functions = lmSBF, method = "cv")

  set.seed(3997)
  sbf_xy <-
    sbf(x = recipe_fs_x[-(1:100), ], y = logBBB[-(1:100)], sbfControl = ctrl)
  pred_xy <- predict(sbf_xy, recipe_fs_x[1:100, ])

  rec <- recipe(y ~ ., data = recipe_fs_dat) %>% step_log(mw)

  set.seed(3997)
  sbf_rec <- sbf(rec, data = recipe_fs_dat[-(1:100), ], sbfControl = ctrl)
  pred_rec <- predict(sbf_rec, recipe_fs_dat[1:100, -4])

  expect_equal(coef(sbf_xy$fit), coef(sbf_rec$fit))
  expect_equal(pred_xy, pred_rec)
})


# ------------------------------------------------------------------------------

test_that("safs with recipes", {
  skip_on_cran()
  ctrl <- safsControl(functions = caretSA, method = "cv", number = 3)

  set.seed(3997)
  sa_xy <-
    safs(
      x = recipe_fs_x[-(1:100), ],
      y = logBBB[-(1:100)],
      safsControl = ctrl,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  pred_xy <- predict(sa_xy, recipe_fs_x[1:100, ])

  rec <- recipe(y ~ ., data = recipe_fs_dat) %>% step_log(mw)

  set.seed(3997)
  sa_rec <-
    safs(
      rec,
      data = recipe_fs_dat[-(1:100), ],
      safsControl = ctrl,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  pred_rec <- predict(sa_rec, recipe_fs_dat[1:100, -4])

  expect_equal(coef(sa_xy$fit$finalModel), coef(sa_rec$fit$finalModel))
  expect_equal(pred_xy, pred_rec)
})

# ------------------------------------------------------------------------------

test_that("gafs with recipes", {
  skip_on_cran()
  ctrl <- gafsControl(functions = caretGA, method = "cv", number = 3)

  set.seed(3997)
  ga_xy <-
    gafs(
      x = recipe_fs_x[-(1:100), ],
      y = logBBB[-(1:100)],
      gafsControl = ctrl,
      popSize = 4,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  pred_xy <- predict(ga_xy, recipe_fs_x[1:100, ])

  rec <- recipe(y ~ ., data = recipe_fs_dat) %>% step_log(mw)

  set.seed(3997)
  ga_rec <-
    gafs(
      rec,
      data = recipe_fs_dat[-(1:100), ],
      gafsControl = ctrl,
      popSize = 4,
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  pred_rec <- predict(ga_rec, recipe_fs_dat[1:100, -4])

  expect_equal(coef(ga_xy$fit$finalModel), coef(ga_rec$fit$finalModel))
  expect_equal(pred_xy, pred_rec)
})

# ------------------------------------------------------------------------------
# the recipe interfaces mirror the x/y validation

test_that("safs and gafs need a seed per resample plus one from a recipe", {
  skip_on_cran()

  rec <- recipe(y ~ ., data = recipe_fs_dat)

  expect_snapshot(
    safs(
      rec,
      data = recipe_fs_dat,
      safsControl = safsControl(
        functions = caretSA,
        method = "cv",
        number = 3,
        seeds = 1:2
      ),
      iters = 2,
      method = "lm",
      trControl = trainControl(method = "cv")
    ),
    error = TRUE
  )
  expect_snapshot(
    gafs(
      rec,
      data = recipe_fs_dat,
      gafsControl = gafsControl(
        functions = caretGA,
        method = "cv",
        number = 3,
        seeds = 1:2
      ),
      popSize = 4,
      iters = 2,
      method = "lm",
      trControl = trainControl(method = "cv")
    ),
    error = TRUE
  )
})

test_that("the recipe searches fall back when the external metric is missing", {
  skip_on_cran()

  rec <- recipe(y ~ ., data = recipe_fs_dat)

  set.seed(4155)
  expect_snapshot_warning(
    sa <- safs(
      rec,
      data = recipe_fs_dat,
      safsControl = safsControl(
        functions = caretSA,
        method = "cv",
        number = 3,
        metric = c(internal = "RMSE", external = "Bogus")
      ),
      iters = 2,
      differences = FALSE,
      method = "lm",
      trControl = trainControl(method = "cv")
    )
  )
  expect_identical(unname(sa$control$metric["external"]), "RMSE")
})

test_that("the recipe searches use a performance-var role and a holdout", {
  skip_on_cran()

  dat <- recipe_fs_dat
  dat$extra <- seq_len(nrow(dat))
  rec <- recipe(y ~ ., data = dat)
  rec <- update_role(rec, extra, new_role = "performance var")

  # the extra column reaches the fitness functions rather than the model
  saw_extra <- function(data, lev = NULL, model = NULL) {
    c(
      RMSE = sqrt(mean((data$obs - data$pred)^2)),
      HasExtra = as.numeric("extra" %in% names(data))
    )
  }
  funcs <- caretSA
  funcs$fitness_extern <- saw_extra

  set.seed(6002)
  sa <- safs(
    rec,
    data = dat,
    safsControl = safsControl(
      functions = funcs,
      method = "cv",
      number = 3,
      holdout = 0.25,
      metric = c(internal = "RMSE", external = "RMSE")
    ),
    iters = 2,
    differences = FALSE,
    method = "lm",
    trControl = trainControl(method = "cv")
  )
  expect_all_equal(sa$external$HasExtra, 1)
  expect_disjoint(sa$optVariables, "extra")
})

test_that("the recipe searches compute variable differences", {
  skip_on_cran()

  rec <- recipe(y ~ ., data = recipe_fs_dat)

  # `differences = TRUE` needs a long enough search for every variable to have
  # been in and out of the subset more than once
  set.seed(3388)
  sa <- safs(
    rec,
    data = recipe_fs_dat,
    safsControl = safsControl(functions = caretSA, method = "cv", number = 3),
    iters = 8,
    differences = TRUE,
    method = "lm",
    trControl = trainControl(method = "cv")
  )
  expect_s3_class(sa$differences, "data.frame")
  expect_s3_class(varImp(sa), "data.frame")
})
