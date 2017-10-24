library(caret)
library(testthat)

###################################################################
## test range

context('preProcess/range')

test_that('conversion to range trans', {
  skip_on_cran()
  set.seed(1)
  rng_dat1 <- twoClassSim(30)[, 1:5]
  rng_dat2 <- twoClassSim(30)[, 1:5]
  
  rng_dat1_min <- apply(rng_dat1, 2, min, na.rm = TRUE)
  rng_dat1_max <- apply(rng_dat1, 2, max, na.rm = TRUE)
  rng_dat1_rng <- rng_dat1_max - rng_dat1_min

  # Default range [0, 1]:
  rng_dat2_ranged_exp <- rng_dat2
  for(i in 1:ncol(rng_dat2_ranged_exp)) 
    rng_dat2_ranged_exp[,i] <- (rng_dat2_ranged_exp[,i] - rng_dat1_min[i])/rng_dat1_rng[i]
  
  rng_dat2_pp <- preProcess(rng_dat1, "range")
  rng_dat2_ranged <- predict(rng_dat2_pp, rng_dat2)
  expect_equal(rng_dat2_ranged_exp, rng_dat2_ranged)

  # Custom range:
  rangeBounds = c(-0.7, 0.4)

  rng_dat2_ranged_custom_exp <- rng_dat2_ranged_exp
  for(i in 1:ncol(rng_dat2_ranged_custom_exp))
    rng_dat2_ranged_custom_exp[,i] <-
      rng_dat2_ranged_custom_exp[,i] * (rangeBounds[2] - rangeBounds[1]) + rangeBounds[1]

  rng_dat2_custom_pp <- preProcess(rng_dat1, "range", rangeBounds = rangeBounds)
  rng_dat2_ranged_custom <- predict(rng_dat2_custom_pp, rng_dat2)
  expect_equal(rng_dat2_ranged_custom_exp, rng_dat2_ranged_custom)

  expect_error(preProcess(rng_dat1, "range", rangeBounds = ""), "'rangeBounds' should be a two-element numeric vector")

  expect_error(preProcess(rng_dat1, "range", rangeBounds = c(0.4, -0.7)), "'rangeBounds' interval is empty")
})

test_that('conversion to range trans with missing data', {
  skip_on_cran()
  set.seed(1)
  rng_dat1 <- twoClassSim(30)[, 1:5]
  rng_dat2 <- twoClassSim(30)[, 1:5]
  rng_dat1[1, 3] <- NA
  rng_dat1[13, 5] <- NA
    
  rng_dat1_min <- apply(rng_dat1, 2, min, na.rm = TRUE)
  rng_dat1_max <- apply(rng_dat1, 2, max, na.rm = TRUE)
  rng_dat1_rng <- rng_dat1_max - rng_dat1_min
  
  rng_dat2_ranged_exp <- rng_dat2
  for(i in 1:ncol(rng_dat2_ranged_exp)) 
    rng_dat2_ranged_exp[,i] <- (rng_dat2_ranged_exp[,i] - rng_dat1_min[i])/rng_dat1_rng[i]
  
  rng_dat2_pp <- preProcess(rng_dat1, "range")
  rng_dat2_ranged <- predict(rng_dat2_pp, rng_dat2)
  expect_equal(rng_dat2_ranged_exp, rng_dat2_ranged)

  # Custom range:
  rangeBounds = c(-0.7, 0.4)

  rng_dat2_ranged_custom_exp <- rng_dat2_ranged_exp
  for(i in 1:ncol(rng_dat2_ranged_custom_exp))
    rng_dat2_ranged_custom_exp[,i] <-
      rng_dat2_ranged_custom_exp[,i] * (rangeBounds[2] - rangeBounds[1]) + rangeBounds[1]

  rng_dat2_custom_pp <- preProcess(rng_dat1, "range", rangeBounds = rangeBounds)
  rng_dat2_ranged_custom <- predict(rng_dat2_custom_pp, rng_dat2)
  expect_equal(rng_dat2_ranged_custom_exp, rng_dat2_ranged_custom)
})
