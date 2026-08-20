# Fitted safs()/gafs() objects for the print, varImp and plot tests.
#
# They are builders rather than objects so nothing is fit when the file is
# sourced, and the tests can gate them with skip_on_cran(). Everything is kept
# deliberately small: a few predictors, a handful of iterations and three-fold
# resampling inside and out. The searches are still real, so the tests seed
# them.
#
# The inner `trControl` has to resample: caretSA/caretGA read the inner model's
# resampled performance as the internal fitness, so method = "none" leaves them
# nothing to work with.

fs_data <- function(n = 60, classification = FALSE) {
  withr::with_seed(3517, {
    dat <- data.frame(
      x1 = rnorm(n),
      x2 = rnorm(n),
      x3 = rnorm(n),
      x4 = rnorm(n)
    )
    if (classification) {
      dat$y <- factor(
        ifelse(dat$x1 + rnorm(n, sd = 0.5) > 0, "one", "two"),
        levels = c("one", "two")
      )
    } else {
      dat$y <- dat$x1 + dat$x2 + rnorm(n, sd = 0.5)
    }
    dat
  })
}

safs_fixture <- function(
  classification = FALSE,
  differences = TRUE,
  iters = 4,
  seed = 4471,
  ...
) {
  dat <- fs_data(classification = classification)
  ctrl <- safsControl(
    functions = caretSA,
    method = "cv",
    number = 3,
    ...
  )
  withr::with_seed(seed, {
    safs(
      x = dat[, 1:4],
      y = dat$y,
      safsControl = ctrl,
      iters = iters,
      differences = differences,
      method = if (classification) "lda" else "lm",
      trControl = trainControl(method = "cv", number = 3)
    )
  })
}

# `differences = TRUE` only yields a table when each variable has been both in
# and out of the population at least twice, so the varImp tests need a bigger
# search than the print and plot tests (the computation fails quietly
# otherwise, and varImp() then reports that differences were not asked for).
gafs_fixture <- function(
  classification = FALSE,
  differences = TRUE,
  iters = 3,
  popSize = 4,
  seed = 4471,
  ...
) {
  dat <- fs_data(classification = classification)
  ctrl <- gafsControl(
    functions = caretGA,
    method = "cv",
    number = 3,
    ...
  )
  withr::with_seed(seed, {
    gafs(
      x = dat[, 1:4],
      y = dat$y,
      gafsControl = ctrl,
      popSize = popSize,
      iters = iters,
      differences = differences,
      method = if (classification) "lda" else "lm",
      trControl = trainControl(method = "cv", number = 3)
    )
  })
}
