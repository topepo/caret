## Test cases by Josh Brady (doublej2) from issue #344

test_that("dummyVars handles factors, missing data, and naming (issues #344, #390)", {
  skip_on_cran()
  dfTrain <- data.frame(xf = c('a', 'b', 'c'), stringsAsFactors = TRUE)
  dfTest <- data.frame(xf = c('a', 'b'), stringsAsFactors = TRUE)

  dummyObj1 <- dummyVars(~., dfTrain)

  expected_train <- diag(3)
  colnames(expected_train) <- paste0("xf.", letters[1:3])
  rownames(expected_train) <- paste(1:3)
  expected_test <- expected_train[1:2, ]

  expect_equal(predict(dummyObj1, newdata = dfTrain), expected_train)
  expect_equal(predict(dummyObj1, newdata = dfTest), expected_test)

  ###################################################################
  ## tests related to issue #390

  ## from ?dummyVars
  when <- data.frame(
    time = c(
      "afternoon",
      "night",
      "afternoon",
      "morning",
      "morning",
      "morning",
      "morning",
      "afternoon",
      "afternoon"
    ),
    day = c("Mon", "Mon", "Mon", "Wed", "Wed", "Fri", "Sat", "Sat", "Fri"),
    stringsAsFactors = TRUE
  )

  levels(when$time) <- list(
    morning = "morning",
    afternoon = "afternoon",
    night = "night"
  )
  levels(when$day) <- list(
    Mon = "Mon",
    Tue = "Tue",
    Wed = "Wed",
    Thu = "Thu",
    Fri = "Fri",
    Sat = "Sat",
    Sun = "Sun"
  )

  mainEffects <- dummyVars(~ day + time, data = when)

  # fmt: skip
  exp_main_nomissing <- structure(c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1,
                                    1, 0, 1, 0, 0, 0, 0, 0, 0, 0),
                                  .Dim = 9:10,
                                  .Dimnames = list(
                                    c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                    c("day.Mon",  "day.Tue", "day.Wed", "day.Thu", "day.Fri",
                                      "day.Sat", "day.Sun", "time.morning", "time.afternoon", "time.night")))
  res_main_nomissing <- predict(mainEffects, when)
  expect_equal(res_main_nomissing, exp_main_nomissing)

  when2 <- when
  when2[1, 1] <- NA

  # fmt: skip
  exp_main_missing <- structure(c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, NA, 0, 0, 1, 1, 1, 1, 0, 0, NA, 0, 1, 0, 0, 0, 0,
                                  1, 1, NA, 1, 0, 0, 0, 0, 0, 0, 0),
                                .Dim = 9:10,
                                .Dimnames = list(
                                  c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                                  c("day.Mon", "day.Tue", "day.Wed", "day.Thu",
                                    "day.Fri", "day.Sat", "day.Sun", "time.morning",
                                    "time.afternoon", "time.night")))

  res_main_missing <- predict(mainEffects, when2)
  expect_equal(res_main_missing, exp_main_missing)

  # fmt: skip
  exp_main_omit <- structure(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                               0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
                               0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                               1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0),
                             .Dim = c(8L, 10L),
                             .Dimnames = list(c("2", "3", "4", "5",  "6", "7", "8", "9"),
                                              c("day.Mon", "day.Tue", "day.Wed", "day.Thu",
                                                "day.Fri", "day.Sat", "day.Sun", "time.morning",
                                                "time.afternoon",  "time.night")))
  res_main_omit <- predict(mainEffects, when2, na.action = na.omit)
  expect_equal(res_main_omit, exp_main_omit)

  ###################################################################
  ## tests related to issue #390

  test_data <- data.frame(
    'id' = seq(1, 30, 1),
    'fooFactor' = factor(c(rep(1, 10), rep(2, 10), rep(3, 10))),
    'fooFactorBar' = factor(c(rep(4, 10), rep(5, 10), rep(6, 10))),
    'fooBarFactor' = factor(c(rep(7, 10), rep(8, 10), rep(9, 10))),
    stringsAsFactors = TRUE
  )

  foosbars <- dummyVars(
    formula = id ~ .,
    data = test_data,
    sep = '-'
  )

  exp_names <- c(
    paste("fooFactor", 1:3, sep = "-"),
    paste("fooFactorBar", 4:6, sep = "-"),
    paste("fooBarFactor", 7:9, sep = "-")
  )
  res_names <- colnames(predict(foosbars, test_data))
  expect_equal(exp_names, res_names)

  foosbarsbars <- dummyVars(
    formula = id ~ .,
    data = test_data,
    sep = '-',
    levelsOnly = TRUE
  )

  exp_names_lvls <- paste(1:9)
  res_names_lvls <- colnames(predict(foosbarsbars, test_data))
  expect_equal(exp_names_lvls, res_names_lvls)
})


test_that("Good names for dummies with reocurring patterns", {
  skip_on_cran()
  set.seed(176)
  # 200 all but guarantees (99.999% chance) 1:15 all represented, #1350
  data = data.frame(
    matrix(
      rep(
        as.factor(sample.int(
          15,
          size = 200,
          replace = TRUE,
          prob = rep(1 / 15, 15)
        )),
        15
      ),
      ncol = 15
    ),
    stringsAsFactors = TRUE
  )
  essai_dummyVars = caret::dummyVars(
    stats::as.formula(paste0("~ ", colnames(data), collapse = "+")),
    data
  )

  exp_names_lvls <- apply(
    expand.grid(paste0("X", 1:15), paste0(".", 1:15)),
    1,
    paste,
    collapse = ""
  )
  res_names_lvls <- colnames(predict(essai_dummyVars, data))
  expect_in(exp_names_lvls, res_names_lvls)
})

test_that("dummyVars print method", {
  skip_on_cran()
  # scrub the formula's environment address, which is not deterministic
  expect_snapshot(
    print(dummyVars(~ Species + Sepal.Length, data = iris)),
    transform = mask_env
  )
})

test_that("dummyVars expands a dot formula and handles all-numeric data", {
  # a dot picks up every column of the data
  dv <- dummyVars(~., data = iris)
  expect_setequal(dv$vars, names(iris))
  expect_identical(dv$facVars, "Species")

  # with no factors at all there is nothing to expand
  num_only <- dummyVars(~., data = iris[, 1:4])
  expect_null(num_only$facVars)
  expect_null(num_only$lvls)
  expect_identical(ncol(predict(num_only, iris[, 1:4])), 4L)
})

test_that("dummyVars rejects levelsOnly when levels are shared", {
  dat <- data.frame(
    a = factor(c("x", "y", "x", "y")),
    b = factor(c("x", "x", "y", "y"))
  )
  expect_snapshot(dummyVars(~., data = dat, levelsOnly = TRUE), error = TRUE)
})

test_that("dummyVars can drop the variable names from the columns", {
  dat <- data.frame(
    size = factor(c("small", "large", "small", "large")),
    hue = factor(c("red", "red", "blue", "blue"))
  )
  dv <- dummyVars(~., data = dat, levelsOnly = TRUE)
  expect_setequal(
    colnames(predict(dv, dat)),
    c("small", "large", "red", "blue")
  )
})

test_that("dummyVars can separate names and levels", {
  dat <- data.frame(size = factor(c("small", "large", "small", "large")))
  dv <- dummyVars(~., data = dat, sep = ".")
  expect_setequal(colnames(predict(dv, dat)), c("size.small", "size.large"))
})

test_that("print.dummyVars describes the encoding", {
  dat <- data.frame(
    size = factor(c("small", "large", "small", "large")),
    n = 1:4
  )
  # the printed formula carries its environment, whose address varies by
  # session, so mask_env (helper-snapshots.R) hides it
  expect_snapshot(print(dummyVars(~., data = dat)), transform = mask_env)
  expect_snapshot(
    print(dummyVars(~., data = dat, sep = ".")),
    transform = mask_env
  )
  expect_snapshot(
    print(dummyVars(~., data = dat, levelsOnly = TRUE)),
    transform = mask_env
  )
  expect_snapshot(
    print(dummyVars(~., data = dat, fullRank = TRUE)),
    transform = mask_env
  )
})

test_that("predict.dummyVars validates its newdata", {
  dat <- data.frame(size = factor(c("small", "large")), n = 1:2)
  dv <- dummyVars(~., data = dat)

  expect_snapshot(predict(dv, newdata = NULL), error = TRUE)
  expect_snapshot(predict(dv, newdata = dat[, "n", drop = FALSE]), error = TRUE)
  # a matrix is converted to a data frame first
  num <- dummyVars(~., data = iris[, 1:4])
  expect_identical(nrow(predict(num, as.matrix(iris[, 1:4]))), 150L)
})

test_that("contr.ltfr builds a less-than-full-rank contrast matrix", {
  # from a level count
  out <- contr.ltfr(3)
  expect_shape(out, dim = c(3L, 3L))
  expect_identical(colnames(out), c("1", "2", "3"))
  # from level names
  named <- contr.ltfr(c("a", "b"))
  expect_identical(rownames(named), c("a", "b"))
  # a sparse version routes through .RDiag's Matrix branch
  skip_if_not_installed("Matrix")
  sp <- contr.ltfr(3, sparse = TRUE)
  expect_identical(dim(sp), c(3L, 3L))
})

test_that("contr.ltfr needs at least two levels", {
  expect_snapshot(contr.ltfr(1), error = TRUE)
  expect_snapshot(contr.ltfr("a"), error = TRUE)
})

test_that("contr.dummy builds an identity contrast matrix", {
  out <- caret:::contr.dummy(3)
  expect_identical(unname(out), diag(3))
  expect_identical(rownames(out), c("1", "2", "3"))

  named <- caret:::contr.dummy(c("a", "b"))
  expect_identical(rownames(named), c("a", "b"))

  expect_snapshot(caret:::contr.dummy(1), error = TRUE)
})

test_that("class2ind converts a factor to indicator columns", {
  f <- factor(c("a", "b", "c", "a"))
  out <- class2ind(f)
  expect_shape(out, dim = c(4L, 3L))
  expect_identical(colnames(out), c("a", "b", "c"))

  # a two-level factor can collapse to a single binary vector
  two <- factor(c("yes", "no", "yes"))
  expect_identical(unname(class2ind(two, drop2nd = TRUE)), c(0, 1, 0))

  expect_snapshot(class2ind(1:4), error = TRUE)
})

test_that("dummyVars converts matrix input to a data frame", {
  x <- as.matrix(iris[, 1:4])
  dv <- dummyVars(~., data = x)
  expect_setequal(dv$vars, colnames(x))
  expect_identical(ncol(predict(dv, iris[, 1:4])), 4L)
})
