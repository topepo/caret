library(caret)
library(fastICA)
library(testthat)
library(MASS)
library(car)

###################################################################
## test centering and scaling

test_that('centering and scaling trans', {
  skip_on_cran()
  set.seed(1)
  cs_dat1 <- twoClassSim(30)[, 1:5]
  cs_dat2 <- twoClassSim(30)[, 1:5]
  
  cs_dat1_means <- apply(cs_dat1, 2, mean)
  cs_dat1_sds <- apply(cs_dat1, 2, sd)
  
  cs_dat2_centered_exp <- cs_dat2
  for(i in 1:ncol(cs_dat2_centered_exp)) 
    cs_dat2_centered_exp[,i] <- cs_dat2_centered_exp[,i] - cs_dat1_means[i]
  
  cs_dat2_pp_centered <- preProcess(cs_dat1, "center")
  cs_dat2_centered <- predict(cs_dat2_pp_centered, cs_dat2)
  expect_equal(cs_dat2_centered_exp, cs_dat2_centered)
  
  cs_dat2_scaled_exp <- cs_dat2
  for(i in 1:ncol(cs_dat2_scaled_exp)) 
    cs_dat2_scaled_exp[,i] <- cs_dat2_scaled_exp[,i]/cs_dat1_sds[i]
  
  cs_dat2_pp_scaled <- preProcess(cs_dat1, "scale")
  cs_dat2_scaled <- predict(cs_dat2_pp_scaled, cs_dat2)
  expect_equal(cs_dat2_scaled_exp, cs_dat2_scaled)
})

test_that('centering and scaling trans with missing data', {
  skip_on_cran()
  set.seed(1)
  cs_dat1 <- twoClassSim(30)[, 1:5]
  cs_dat2 <- twoClassSim(30)[, 1:5]
  cs_dat1[1, 3] <- NA
  cs_dat1[13, 5] <- NA
  
  cs_dat1_means <- apply(cs_dat1, 2, mean, na.rm = TRUE)
  cs_dat1_sds <- apply(cs_dat1, 2, sd, na.rm = TRUE)
  
  cs_dat2_centered_exp <- cs_dat2
  for(i in 1:ncol(cs_dat2_centered_exp)) 
    cs_dat2_centered_exp[,i] <- cs_dat2_centered_exp[,i] - cs_dat1_means[i]
  
  cs_dat2_pp_centered <- preProcess(cs_dat1, "center")
  cs_dat2_centered <- predict(cs_dat2_pp_centered, cs_dat2)
  expect_equal(cs_dat2_centered_exp, cs_dat2_centered)
  
  cs_dat2_scaled_exp <- cs_dat2
  for(i in 1:ncol(cs_dat2_scaled_exp)) 
    cs_dat2_scaled_exp[,i] <- cs_dat2_scaled_exp[,i]/cs_dat1_sds[i]
  
  cs_dat2_pp_scaled <- preProcess(cs_dat1, "scale")
  cs_dat2_scaled <- predict(cs_dat2_pp_scaled, cs_dat2)
  expect_equal(cs_dat2_scaled_exp, cs_dat2_scaled)
})



###################################################################
## test range

test_that('conversion to range trans', {
  skip_on_cran()
  set.seed(1)
  rng_dat1 <- twoClassSim(30)[, 1:5]
  rng_dat2 <- twoClassSim(30)[, 1:5]
  
  rng_dat1_min <- apply(rng_dat1, 2, min, na.rm = TRUE)
  rng_dat1_max <- apply(rng_dat1, 2, max, na.rm = TRUE)
  rng_dat1_rng <- rng_dat1_max - rng_dat1_min
  
  rng_dat2_ranged_exp <- rng_dat2
  for(i in 1:ncol(rng_dat2_ranged_exp)) 
    rng_dat2_ranged_exp[,i] <- (rng_dat2_ranged_exp[,i] - rng_dat1_min[i])/rng_dat1_rng[i]
  
  rng_dat2_pp <- preProcess(rng_dat1, "range")
  rng_dat2_ranged <- predict(rng_dat2_pp, rng_dat2)
  expect_equal(rng_dat2_ranged_exp, rng_dat2_ranged)
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
})

###################################################################
## test pca

test_that('PCA trans', {
  skip_on_cran()
  set.seed(1)
  pca_dat1 <- twoClassSim(30)[, 1:5]
  pca_dat2 <- twoClassSim(30)[, 1:5]
  
  pc_obj <- prcomp(pca_dat1, center = TRUE, scale. = TRUE)
  pca_dat2_exp <- predict(pc_obj, pca_dat2)
  
  pca_dat2_pp <- preProcess(pca_dat1, "pca")
  pca_dat2_pca <- as.data.frame(predict(pca_dat2_pp, pca_dat2))
  expect_equal(pca_dat2_pca, pca_dat2_exp[, 1:ncol(pca_dat2_pca)])
})


test_that('PCA trans with missing data', {
  skip_on_cran()
  ## This will produce different results than prcomp with complete
  ## since preProcess calculates means and sds by column whereas
  ## prcomp does casewise deletion
  set.seed(1)
  pca_dat1 <- twoClassSim(30)[, 1:5]
  pca_dat2 <- twoClassSim(30)[, 1:5]
  pca_dat1[1, 3] <- NA
  pca_dat1[13, 5] <- NA
  
  pc_obj <- prcomp(pca_dat1[complete.cases(pca_dat1),], 
                   center = TRUE, scale. = TRUE)
  
  pca_dat2_pp <- preProcess(pca_dat1, "pca")
  expect_equal(pc_obj$rotation[, 1:ncol(pca_dat2_pp$rotation)], pca_dat2_pp$rotation)
})

###################################################################
## test ica

test_that('ICA trans', {
  skip_on_cran()
  set.seed(1)
  ica_dat1 <- twoClassSim(30)[, 1:5]
  ica_dat2 <- twoClassSim(30)[, 1:5]
  
  set.seed(1)
  ica_dat2_pp <- preProcess(ica_dat1, method = "ica", n.comp = 3)
  ica_dat2_ica <- predict(ica_dat2_pp, ica_dat2)
  
  ica_dat1_means <- apply(ica_dat1, 2, mean)
  ica_dat1_sds <- apply(ica_dat1, 2, sd)
  ica_dat2_scaled <- ica_dat2
  for(i in 1:ncol(ica_dat2_scaled)) 
    ica_dat2_scaled[,i] <- (ica_dat2_scaled[,i]-ica_dat1_means[i])/ica_dat1_sds[i]
  
  set.seed(1)
  ic_obj <- fastICA(scale(ica_dat1, center = TRUE, scale = TRUE),
                    n.comp = 3)
  ica_dat2_exp <- as.matrix(ica_dat2_scaled) %*% ic_obj$K %*% ic_obj$W
  colnames(ica_dat2_exp) <- paste("ICA", 1:ncol(ic_obj$W), sep = "")
  expect_equal(as.data.frame(ica_dat2_exp), ica_dat2_ica, tolerance = .00001)
})


###################################################################
## test SS

test_that('Spatial sign trans', {
  skip_on_cran()
  set.seed(1)
  ss_dat1 <- twoClassSim(30)[, 1:5]
  ss_dat2 <- twoClassSim(30)[, 1:5]
  
  ss_dat2_pp <- preProcess(ss_dat1, method = "spatialSign")
  ss_dat2_ss <- predict(ss_dat2_pp, ss_dat2)
  
  ss_dat1_means <- apply(ss_dat1, 2, mean)
  ss_dat1_sds <- apply(ss_dat1, 2, sd)
  ss_dat2_scaled <- ss_dat2
  for(i in 1:ncol(ss_dat2_scaled)) 
    ss_dat2_scaled[,i] <- (ss_dat2_scaled[,i]-ss_dat1_means[i])/ss_dat1_sds[i]
  
  ss_dat2_ss_exp <- t(apply(ss_dat2_scaled, 1, function(x) x/sqrt(sum(x^2))))
  expect_equal(as.data.frame(ss_dat2_ss_exp), ss_dat2_ss)
})


###################################################################
## test BC trans

test_that('Box-Cox trans', {
  skip_on_cran()
  set.seed(1)
  bc_dat1 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]))
  bc_dat2 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]))
  
  bc_dat2_pp <- preProcess(bc_dat1, method = "BoxCox")
  bc_dat2_bc <- predict(bc_dat2_pp, bc_dat2)
  
  bc_trans <- lapply(bc_dat1, 
                     function(x) MASS::boxcox(x ~ rep(1, length(x)), 
                                              plotit = FALSE))
  bc_dat2_bc_exp <- bc_dat2
  for(i in 1:ncol(bc_dat2)) {
    lambda <- bc_trans[[i]]$x[which.max(bc_trans[[i]]$y)]
    bc_dat2_bc_exp[, i] <- (bc_dat2_bc_exp[, i]^lambda - 1)/lambda
  }
  expect_equal(bc_dat2_bc_exp, bc_dat2_bc)
})


test_that('Box-Cox trans with missing data', {
  skip_on_cran()
  set.seed(1)
  bc_dat1 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]))
  bc_dat2 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]))
  bc_dat1[1, 3] <- NA
  bc_dat1[13, 5] <- NA
  
  bc_dat2_pp <- preProcess(bc_dat1, method = "BoxCox")
  bc_dat2_bc <- predict(bc_dat2_pp, bc_dat2)
  
  bc_trans <- lapply(bc_dat1, 
                     function(x) {
                       x <- x[!is.na(x)]
                       MASS::boxcox(x ~ rep(1, length(x)), 
                                              plotit = FALSE)
                     })
  
  bc_dat2_bc_exp <- bc_dat2
  for(i in 1:ncol(bc_dat2)) {
    lambda <- bc_trans[[i]]$x[which.max(bc_trans[[i]]$y)]
    bc_dat2_bc_exp[, i] <- (bc_dat2_bc_exp[, i]^lambda - 1)/lambda
  }
  expect_equal(bc_dat2_bc_exp, bc_dat2_bc)
})

###################################################################
## test YJ trans

test_that('Yeo-Johnson trans', {
  skip_on_cran()
  set.seed(1)
  yj_dat1 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]))
  yj_dat2 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]))
  
  yj_dat2_pp <- preProcess(yj_dat1, method = "YeoJohnson")
  yj_dat2_yj <- predict(yj_dat2_pp, yj_dat2)
  
  yj_trans <- lapply(yj_dat1, 
                     function(x) powerTransform(lm(x ~ rep(1, length(x))),
                                                family = "yjPower"))
  yj_dat2_yj_exp <- yj_dat2
  for(i in 1:ncol(yj_dat2)) 
    yj_dat2_yj_exp[, i] <- yjPower(yj_dat2[,i], yj_trans[[i]]$lambda)
  expect_equal(yj_dat2_yj_exp, yj_dat2_yj)
})


test_that('Yeo-Johnson trans with mising data', {
  skip_on_cran()
  set.seed(1)
  yj_dat1 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]))
  yj_dat2 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]))
  yj_dat1[1, 3] <- NA
  yj_dat1[13, 5] <- NA
  
  yj_dat2_pp <- preProcess(yj_dat1, method = "YeoJohnson")
  yj_dat2_yj <- predict(yj_dat2_pp, yj_dat2)
  
  yj_trans <- lapply(yj_dat1, 
                     function(x)  {
                       x <- x[!is.na(x)]
                       powerTransform(lm(x ~ rep(1, length(x))),
                                      family = "yjPower")
                     })
  
  yj_dat2_yj_exp <- yj_dat2
  for(i in 1:ncol(yj_dat2)) 
    yj_dat2_yj_exp[, i] <- yjPower(yj_dat2[,i], yj_trans[[i]]$lambda)
  expect_equal(yj_dat2_yj_exp, yj_dat2_yj)
})

###################################################################
## test variable filtering

test_that('filters', {
  skip_on_cran()
  dat <- data.frame(x1 = 1:50, 
                    x2 = 1, 
                    x3 = c(rep(1, 49), 0), 
                    x4 = c(rep(0, 50), 1:50), 
                    y = factor(rep(letters[1:2], each = 50)))
  
  no_zv  <- preProcess(dat, method = "zv")
  no_nzv <- preProcess(dat, method = "nzv") 
  no_xgy <- preProcess(dat, method = "conditionalX", outcome = dat$y)  
  filter_mean <- preProcess(dat,
                            method = list(conditionalX = names(dat)[1:4], center = "x1"), 
                            outcome = dat$y)    
  no_zv_pred  <- predict(no_zv, dat)
  no_nzv_pred <- predict(no_nzv, dat)
  no_xgy_pred <- predict(no_xgy, dat[, 1:4])
  filter_mean_pred <- predict(filter_mean, dat[, 1:4])
  
  x1_exp <- dat$x1 - mean(dat$x1)
  
  expect_equal(colnames(no_zv_pred), c("x1", "x3", "x4", "y"))
  expect_equal(colnames(no_nzv_pred), c("x1", "x4", "y"))  
  expect_equal(colnames(no_xgy_pred), c("x1", "x3"))    
  expect_equal(colnames(filter_mean_pred), c("x1", "x3"))    
  expect_equal(filter_mean_pred$x1, x1_exp)  
})


