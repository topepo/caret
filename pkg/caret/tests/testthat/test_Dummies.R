context('Dummy Variables')

## Test cases by Josh Brady (doublej2) from issue #344

check_dummies <- function(x, expected = NULL) {
  dfTrain <- data.frame(xf = c('a','b','c'), stringsAsFactors = TRUE)
  dfTest <- data.frame(xf = c('a','b'), stringsAsFactors = TRUE)

  dummyObj1 <- dummyVars(~., dfTrain)

  expected_train <- diag(3)
  colnames(expected_train) <- paste0("xf.", letters[1:3])
  rownames(expected_train) <- paste(1:3)
  expected_test <- expected_train[1:2,]

  expect_equal(predict(dummyObj1, newdata = dfTrain),
               expected_train)
  expect_equal(predict(dummyObj1, newdata = dfTest),
               expected_test)

  ###################################################################
  ## tests related to issue #390

  ## from ?dummyVars
  when <- data.frame(time = c("afternoon", "night", "afternoon",
                              "morning", "morning", "morning",
                              "morning", "afternoon", "afternoon"),
                     day = c("Mon", "Mon", "Mon",
                             "Wed", "Wed", "Fri",
                             "Sat", "Sat", "Fri"),
                     stringsAsFactors = TRUE)

  levels(when$time) <- list(morning="morning",
                            afternoon="afternoon",
                            night="night")
  levels(when$day) <- list(Mon="Mon", Tue="Tue", Wed="Wed", Thu="Thu",
                           Fri="Fri", Sat="Sat", Sun="Sun")

  mainEffects <- dummyVars(~ day + time, data = when)
  interactionModel <- dummyVars(~ day + time + day:time,
                                data = when,
                                sep = ".")
  noNames <- dummyVars(~ day + time + day:time,
                       data = when,
                       levelsOnly = TRUE)


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
  expect_equal(res_main_nomissing,  exp_main_nomissing)

  when2 <- when
  when2[1, 1] <- NA

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
  expect_equal(res_main_missing,  exp_main_missing)

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
  expect_equal(res_main_omit,  exp_main_omit)

  ###################################################################
  ## tests related to issue #390

  test_data <- data.frame('id' = seq(1,30,1),
                          'fooFactor' = factor(c(rep(1,10), rep(2,10), rep(3,10))),
                          'fooFactorBar' = factor(c(rep(4,10), rep(5,10), rep(6,10))),
                          'fooBarFactor' = factor(c(rep(7,10), rep(8,10), rep(9,10))),
                          stringsAsFactors = TRUE)

  foosbars <- dummies <- dummyVars(formula = id ~.,
                                   data = test_data,
                                   sep = '-')

  exp_names <- c(paste("fooFactor", 1:3, sep = "-"),
                 paste("fooFactorBar", 4:6, sep = "-"),
                 paste("fooBarFactor", 7:9, sep = "-"))
  res_names <- colnames(predict(foosbars, test_data))
  expect_equal(exp_names,  res_names)

  foosbarsbars <- dummies <- dummyVars(formula = id ~.,
                                   data = test_data,
                                   sep = '-',
                                   levelsOnly = TRUE)

  exp_names_lvls <- paste(1:9)
  res_names_lvls <- colnames(predict(foosbarsbars, test_data))
  expect_equal(exp_names_lvls,  res_names_lvls)

}


test_that("Good names for dummies with reocurring patterns", {
  data = data.frame(
    matrix(
      rep(
        as.factor(sample.int(15, size = 100, replace = TRUE, prob = rep(1 / 15, 15))
        ),
        15
      ),
      ncol = 15
    ),
    stringsAsFactors = TRUE
  )
  essai_dummyVars = caret::dummyVars(stats::as.formula(paste0("~ ", colnames(data), collapse = "+")), data)

  exp_names_lvls <- apply(expand.grid(paste0("X",1:15), paste0(".",1:15)), 1, paste, collapse="")
  res_names_lvls <- colnames(predict(essai_dummyVars, data))
  expect_true(all(exp_names_lvls %in% res_names_lvls))
})

