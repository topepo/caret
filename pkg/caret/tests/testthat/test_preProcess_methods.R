library(caret)
library(fastICA)
library(testthat)
library(MASS)

context('preProcess/methods')

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
})

###################################################################
## test pca

test_that('PCA trans', {
  skip_on_cran()
  set.seed(1)
  pca_dat1 <- twoClassSim(30)[, 1:5]
  pca_dat2 <- twoClassSim(30)[, 1:5]

  pc_obj <- prcomp(pca_dat1, center = TRUE, scale. = TRUE)
  pca_dat2_exp <- as.data.frame(predict(pc_obj, pca_dat2), stringsAsFactors = TRUE)

  pca_dat2_pp <- preProcess(pca_dat1, "pca")
  pca_dat2_pca <- as.data.frame(predict(pca_dat2_pp, pca_dat2), stringsAsFactors = TRUE)
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


test_that('issue #825 for pca threshold choice', {
  skip_on_cran()
  expect_equal(preProcess(mtcars, method = "pca", thresh = 0.999)$numComp,
               11)
  expect_equal(preProcess(mtcars, method = "pca", thresh = 1)$numComp,
               11)
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
  expect_equal(as.data.frame(ica_dat2_exp, stringsAsFactors = TRUE), ica_dat2_ica, tolerance = .00001)
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
  expect_equal(as.data.frame(ss_dat2_ss_exp, stringsAsFactors = TRUE), ss_dat2_ss)
})


###################################################################
## test BC trans

test_that('Box-Cox trans', {
  skip_on_cran()
  set.seed(1)
  bc_dat1 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]), stringsAsFactors = TRUE)
  bc_dat2 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]), stringsAsFactors = TRUE)

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
  bc_dat1 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]), stringsAsFactors = TRUE)
  bc_dat2 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]), stringsAsFactors = TRUE)
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
  yj_dat1 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]), stringsAsFactors = TRUE)
  yj_dat2 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]), stringsAsFactors = TRUE)

  yj_dat2_pp <- preProcess(yj_dat1, method = "YeoJohnson")
  yj_dat2_yj <- predict(yj_dat2_pp, yj_dat2)

  ## values derived from
  ##  car::powerTransform(lm(x ~ rep(1, length(x))),
  ##                      family = "yjPower")

  yj_lambda_exp <- structure(
    c(
      -1.17126152733178,-0.824731066670367,-0.706588079282242,
      -1.03306249319229,-0.75439768809812
    ),
    .Names = c("TwoFactor1", "TwoFactor2", "Linear01",
               "Linear02", "Linear03")
  )

  expect_equal(yj_lambda_exp, yj_dat2_pp$yj, tolerance = 0.00001)

  yj_pred_exp <-
  structure(
    list(
      TwoFactor1 = c(0.420560715546019, 0.598378458024916,
                     0.673003010711217, 0.409422458736258, 0.371394756255572, 0.318908430839341,
                     0.285646719095963, 0.72916730138736, 0.734268076707233, 0.43551493952166,
                     0.290666454757254, 0.570964727404932, 0.263160240412776, 0.663420355641077,
                     0.534820290852273, 0.648861162529164, 0.375342714187168, 0.655069041209095,
                     0.356196716995193, 0.754580709735319, 0.815397568471886, 0.231018640782111,
                     0.834233681694831, 0.5143818322544, 0.605099195272536, 0.239342349309458,
                     0.325877110845926, 0.556910621166843, 0.656052500704136, 0.24465996399532
      ),
      TwoFactor2 = c(0.301615328120378, 0.461445322028633, 0.707248808552887,
                     0.244768984552592, 1.01067529242088, 0.332615908226651, 0.370035788857244,
                     0.317957630575641, 0.431769991391304, 0.692643661966023, 0.264252145311897,
                     0.353880052286319, 0.470078031545033, 0.835560538413397, 0.604194627876356,
                     1.05840350841711, 0.618051438984101, 0.822322489981997, 1.12386389607311,
                     0.421962949614479, 0.517329161765426, 0.451124787548069, 1.05969439552782,
                     0.443643138202623, 0.510803120628854, 0.180141819953847, 0.410113908133513,
                     0.776831077228084, 0.524988359258372, 0.254416243052048),
      Linear01 = c(0.864108782192729,
                   0.506534494233161, 0.753535622067997, 0.860253617916964, 0.313567775794412,
                   0.30436144554366, 1.05735702776074, 0.534978869083642, 0.355974373056103,
                   0.755176403452401, 0.521648982558502, 0.320219904091857, 0.585541844373729,
                   1.13713060232867, 0.214875434194013, 0.516620862140554, 0.906242460460816,
                   0.517168699796354, 0.87193258014986, 0.589911997782569, 1.19526454070895,
                   0.915927115144444, 0.497212378712002, 1.35616124009747, 0.550617084276063,
                   0.412020915684127, 0.453856531631132, 0.961762046406687, 0.50326734704833,
                   0.515182583270066),
      Linear02 = c(0.281937013949507, 0.511134450487616,
                   0.503099179141131, 0.92837770431677, 0.700822354054209, 0.795814987202248,
                   0.69186082261781, 0.715544411078669, 0.719683350297892, 0.452487476967914,
                   0.568653097760424, 0.760480774493129, 0.52478703672863, 0.810734477261759,
                   0.928178968632499, 0.36987132660124, 0.854057804871416, 0.454516797058032,
                   0.587249892224224, 0.854756545648468, 0.452035356468675, 0.456792695615081,
                   0.902832766379788, 0.678172210837119, 0.375769901612575, 0.55844738028494,
                   0.753293517878738, 0.434506299792077, 0.615740571333, 0.751245083135553
      ),
      Linear03 = c(0.797817560972044, 0.842890525491348, 0.73909086917419,
                   1.26084441662679, 0.459654364199453, 1.16332639149668, 0.646711567029346,
                   0.725775316422072, 0.326201989218444, 0.815996808114906, 0.74862498637551,
                   0.956541559335439, 0.507194706291381, 0.813874511711738, 0.415064030185865,
                   0.628661842981387, 0.312877518999078, 0.602008999383141, 0.746089364393084,
                   0.740571102400277, 0.482810402342108, 1.25094448369284, 0.646306153405863,
                   0.699357380858777, 0.689749274634193, 0.512604738920554, 0.934295181373473,
                   1.02826478194202, 0.680507562338646, 0.571037802377044)),
    .Names = c("TwoFactor1", "TwoFactor2", "Linear01", "Linear02", "Linear03"),
    row.names = c(NA, -30L), class = "data.frame")

  expect_equal(yj_pred_exp, yj_dat2_yj, tolerance = 0.00001)
})


test_that('Yeo-Johnson trans with mising data', {
  skip_on_cran()
  set.seed(1)
  yj_dat1 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]), stringsAsFactors = TRUE)
  yj_dat2 <- as.data.frame(1/abs(twoClassSim(30)[, 1:5]), stringsAsFactors = TRUE)
  yj_dat1[1, 3] <- NA
  yj_dat1[13, 5] <- NA

  yj_dat2_pp <- preProcess(yj_dat1, method = "YeoJohnson")
  yj_dat2_yj <- predict(yj_dat2_pp, yj_dat2)

  yj_lambda_exp <-
    structure(
      c(
        -1.17126152733178,
        -0.824731066670367,
        -0.715673444662049,
        -1.03306249319229,
        -0.736848020368422
      ),
      .Names = c("TwoFactor1", "TwoFactor2", "Linear01", "Linear02", "Linear03")
    )
  expect_equal(yj_lambda_exp, yj_dat2_pp$yj, tolerance = 0.00001)

  # yj_trans <- lapply(yj_dat1,
  #                    function(x)  {
  #                      x <- x[!is.na(x)]
  #                      powerTransform(lm(x ~ rep(1, length(x))),
  #                                     family = "yjPower")
  #                    })

  yj_pred_exp <-
    structure(
      list(
        TwoFactor1 = c(0.420560715546019, 0.598378458024916,
                       0.673003010711217, 0.409422458736258, 0.371394756255572, 0.318908430839341,
                       0.285646719095963, 0.72916730138736, 0.734268076707233, 0.43551493952166,
                       0.290666454757254, 0.570964727404932, 0.263160240412776, 0.663420355641077,
                       0.534820290852273, 0.648861162529164, 0.375342714187168, 0.655069041209095,
                       0.356196716995193, 0.754580709735319, 0.815397568471886, 0.231018640782111,
                       0.834233681694831, 0.5143818322544, 0.605099195272536, 0.239342349309458,
                       0.325877110845926, 0.556910621166843, 0.656052500704136, 0.24465996399532
        ),
        TwoFactor2 = c(0.301615328120378, 0.461445322028633, 0.707248808552887,
                       0.244768984552592, 1.01067529242088, 0.332615908226651, 0.370035788857244,
                       0.317957630575641, 0.431769991391304, 0.692643661966023, 0.264252145311897,
                       0.353880052286319, 0.470078031545033, 0.835560538413397, 0.604194627876356,
                       1.05840350841711, 0.618051438984101, 0.822322489981997, 1.12386389607311,
                       0.421962949614479, 0.517329161765426, 0.451124787548069, 1.05969439552782,
                       0.443643138202623, 0.510803120628854, 0.180141819953847, 0.410113908133513,
                       0.776831077228084, 0.524988359258372, 0.254416243052048),
        Linear01 = c(0.859697552530382,
                     0.505200378017733, 0.7503246827416, 0.855888682989617, 0.31308441235727,
                     0.303907192021672, 1.05012555512123, 0.533477428995901, 0.35534406181835,
                     0.751949477622746, 0.520227395531799, 0.319714893812003, 0.583713717377287,
                     1.12837971026997, 0.214654343323898, 0.51522873399003, 0.901302407307642,
                     0.515773379454426, 0.867426342877088, 0.588053820678508, 1.18522771771748,
                     0.910859379040186, 0.495930605674975, 1.34127944309918, 0.549018671634491,
                     0.411162871887377, 0.452802483285271, 0.956056882269354, 0.501951714221192,
                     0.513798815505467),
        Linear02 = c(0.281937013949507, 0.511134450487616,
                     0.503099179141131, 0.92837770431677, 0.700822354054209, 0.795814987202248,
                     0.69186082261781, 0.715544411078669, 0.719683350297892, 0.452487476967914,
                     0.568653097760424, 0.760480774493129, 0.52478703672863, 0.810734477261759,
                     0.928178968632499, 0.36987132660124, 0.854057804871416, 0.454516797058032,
                     0.587249892224224, 0.854756545648468, 0.452035356468675, 0.456792695615081,
                     0.902832766379788, 0.678172210837119, 0.375769901612575, 0.55844738028494,
                     0.753293517878738, 0.434506299792077, 0.615740571333, 0.751245083135553
        ),
        Linear03 = c(0.805118298846952, 0.85121450654463, 0.745194683843413,
                     1.2860525878536, 0.461776644848524, 1.18271554878404, 0.651210085828673,
                     0.731627675071425, 0.32722557069373, 0.823698767964377, 0.75491324104535,
                     0.967916002781324, 0.509821503332055, 0.821528847028035, 0.416768926547462,
                     0.632882793009867, 0.313815323629623, 0.605840186293055, 0.752328204612178,
                     0.746703310040928, 0.485170543055165, 1.27545020954066, 0.650798314402513,
                     0.70473143893381, 0.694955980686517, 0.515292996577666, 0.945013657354694,
                     1.04198422179449, 0.68555654074888, 0.574445121616549)),
      .Names = c("TwoFactor1", "TwoFactor2", "Linear01", "Linear02", "Linear03"),
      row.names = c(NA, -30L),
      class = "data.frame")

  expect_equal(yj_pred_exp, yj_dat2_yj, tolerance = 0.00001)
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


