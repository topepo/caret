# Shared fixtures for test_recipe_fs.R

utils::data("BloodBrain", package = "caret", envir = environment())

# predictors for the x/y interface (mw already log-transformed)
recipe_fs_x <- bbbDescr[, c("tpsa", "clogp", "mw")]
recipe_fs_x$mw <- log(recipe_fs_x$mw)

# data frame (predictors + outcome) for the recipe interface (mw raw; logged via step_log)
recipe_fs_dat <- bbbDescr[, c("tpsa", "clogp", "mw")]
recipe_fs_dat$y <- logBBB
