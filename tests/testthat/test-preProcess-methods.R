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
  for (i in seq_len(ncol(cs_dat2_centered_exp))) {
    cs_dat2_centered_exp[, i] <- cs_dat2_centered_exp[, i] - cs_dat1_means[i]
  }

  cs_dat2_pp_centered <- preProcess(cs_dat1, "center")
  cs_dat2_centered <- predict(cs_dat2_pp_centered, cs_dat2)
  expect_equal(cs_dat2_centered_exp, cs_dat2_centered)

  cs_dat2_scaled_exp <- cs_dat2
  for (i in seq_len(ncol(cs_dat2_scaled_exp))) {
    cs_dat2_scaled_exp[, i] <- cs_dat2_scaled_exp[, i] / cs_dat1_sds[i]
  }

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
  for (i in seq_len(ncol(cs_dat2_centered_exp))) {
    cs_dat2_centered_exp[, i] <- cs_dat2_centered_exp[, i] - cs_dat1_means[i]
  }

  cs_dat2_pp_centered <- preProcess(cs_dat1, "center")
  cs_dat2_centered <- predict(cs_dat2_pp_centered, cs_dat2)
  expect_equal(cs_dat2_centered_exp, cs_dat2_centered)

  cs_dat2_scaled_exp <- cs_dat2
  for (i in seq_len(ncol(cs_dat2_scaled_exp))) {
    cs_dat2_scaled_exp[, i] <- cs_dat2_scaled_exp[, i] / cs_dat1_sds[i]
  }

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
  for (i in seq_len(ncol(rng_dat2_ranged_exp))) {
    rng_dat2_ranged_exp[, i] <- (rng_dat2_ranged_exp[, i] - rng_dat1_min[i]) /
      rng_dat1_rng[i]
  }

  rng_dat2_pp <- preProcess(rng_dat1, "range")
  rng_dat2_ranged <- predict(rng_dat2_pp, rng_dat2)
  expect_equal(rng_dat2_ranged_exp, rng_dat2_ranged)

  # Custom range:
  rangeBounds = c(-0.7, 0.4)

  rng_dat2_ranged_custom_exp <- rng_dat2_ranged_exp
  for (i in seq_len(ncol(rng_dat2_ranged_custom_exp))) {
    rng_dat2_ranged_custom_exp[, i] <-
      rng_dat2_ranged_custom_exp[, i] *
      (rangeBounds[2] - rangeBounds[1]) +
      rangeBounds[1]
  }

  rng_dat2_custom_pp <- preProcess(rng_dat1, "range", rangeBounds = rangeBounds)
  rng_dat2_ranged_custom <- predict(rng_dat2_custom_pp, rng_dat2)
  expect_equal(rng_dat2_ranged_custom_exp, rng_dat2_ranged_custom)

  expect_snapshot(
    preProcess(rng_dat1, "range", rangeBounds = ""),
    error = TRUE
  )

  expect_snapshot(
    preProcess(rng_dat1, "range", rangeBounds = c(0.4, -0.7)),
    error = TRUE
  )
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
  for (i in seq_len(ncol(rng_dat2_ranged_exp))) {
    rng_dat2_ranged_exp[, i] <- (rng_dat2_ranged_exp[, i] - rng_dat1_min[i]) /
      rng_dat1_rng[i]
  }

  rng_dat2_pp <- preProcess(rng_dat1, "range")
  rng_dat2_ranged <- predict(rng_dat2_pp, rng_dat2)
  expect_equal(rng_dat2_ranged_exp, rng_dat2_ranged)

  # Custom range:
  rangeBounds = c(-0.7, 0.4)

  rng_dat2_ranged_custom_exp <- rng_dat2_ranged_exp
  for (i in seq_len(ncol(rng_dat2_ranged_custom_exp))) {
    rng_dat2_ranged_custom_exp[, i] <-
      rng_dat2_ranged_custom_exp[, i] *
      (rangeBounds[2] - rangeBounds[1]) +
      rangeBounds[1]
  }

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
  pca_dat2_exp <- as.data.frame(
    predict(pc_obj, pca_dat2),
    stringsAsFactors = TRUE
  )

  pca_dat2_pp <- preProcess(pca_dat1, "pca")
  pca_dat2_pca <- as.data.frame(
    predict(pca_dat2_pp, pca_dat2),
    stringsAsFactors = TRUE
  )
  expect_equal(pca_dat2_pca, pca_dat2_exp[, seq_len(ncol(pca_dat2_pca))])
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

  pc_obj <- prcomp(
    pca_dat1[complete.cases(pca_dat1), ],
    center = TRUE,
    scale. = TRUE
  )

  pca_dat2_pp <- preProcess(pca_dat1, "pca")
  expect_equal(
    pc_obj$rotation[, seq_len(ncol(pca_dat2_pp$rotation))],
    pca_dat2_pp$rotation
  )
})


test_that('issue #825 for pca threshold choice', {
  skip_on_cran()
  expect_equal(preProcess(mtcars, method = "pca", thresh = 0.999)$numComp, 11)
  expect_equal(preProcess(mtcars, method = "pca", thresh = 1)$numComp, 11)
})

###################################################################
## test ica

test_that('ICA trans', {
  skip_on_cran()
  skip_if_not_installed("fastICA")
  set.seed(1)
  ica_dat1 <- twoClassSim(30)[, 1:5]
  ica_dat2 <- twoClassSim(30)[, 1:5]

  set.seed(1)
  ica_dat2_pp <- preProcess(ica_dat1, method = "ica", n.comp = 3)
  ica_dat2_ica <- predict(ica_dat2_pp, ica_dat2)

  ica_dat1_means <- apply(ica_dat1, 2, mean)
  ica_dat1_sds <- apply(ica_dat1, 2, sd)
  ica_dat2_scaled <- ica_dat2
  for (i in seq_len(ncol(ica_dat2_scaled))) {
    ica_dat2_scaled[, i] <- (ica_dat2_scaled[, i] - ica_dat1_means[i]) /
      ica_dat1_sds[i]
  }

  set.seed(1)
  ic_obj <- fastICA::fastICA(
    scale(ica_dat1, center = TRUE, scale = TRUE),
    n.comp = 3
  )
  ica_dat2_exp <- as.matrix(ica_dat2_scaled) %*% ic_obj$K %*% ic_obj$W
  colnames(ica_dat2_exp) <- paste("ICA", seq_len(ncol(ic_obj$W)), sep = "")
  expect_equal(
    as.data.frame(ica_dat2_exp, stringsAsFactors = TRUE),
    ica_dat2_ica,
    tolerance = 0.00001
  )
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
  for (i in seq_len(ncol(ss_dat2_scaled))) {
    ss_dat2_scaled[, i] <- (ss_dat2_scaled[, i] - ss_dat1_means[i]) /
      ss_dat1_sds[i]
  }

  ss_dat2_ss_exp <- t(apply(ss_dat2_scaled, 1, function(x) x / sqrt(sum(x^2))))
  expect_equal(
    as.data.frame(ss_dat2_ss_exp, stringsAsFactors = TRUE),
    ss_dat2_ss
  )
})


###################################################################
## test BC trans

test_that('Box-Cox trans', {
  skip_on_cran()
  set.seed(1)
  bc_dat1 <- as.data.frame(
    1 / abs(twoClassSim(30)[, 1:5]),
    stringsAsFactors = TRUE
  )
  bc_dat2 <- as.data.frame(
    1 / abs(twoClassSim(30)[, 1:5]),
    stringsAsFactors = TRUE
  )

  bc_dat2_pp <- preProcess(bc_dat1, method = "BoxCox")
  bc_dat2_bc <- predict(bc_dat2_pp, bc_dat2)

  bc_trans <- lapply(bc_dat1, function(x) {
    MASS::boxcox(x ~ rep(1, length(x)), plotit = FALSE)
  })
  bc_dat2_bc_exp <- bc_dat2
  for (i in seq_len(ncol(bc_dat2))) {
    lambda <- bc_trans[[i]]$x[which.max(bc_trans[[i]]$y)]
    bc_dat2_bc_exp[, i] <- (bc_dat2_bc_exp[, i]^lambda - 1) / lambda
  }
  expect_equal(bc_dat2_bc_exp, bc_dat2_bc)
})


test_that('Box-Cox trans with missing data', {
  skip_on_cran()
  set.seed(1)
  bc_dat1 <- as.data.frame(
    1 / abs(twoClassSim(30)[, 1:5]),
    stringsAsFactors = TRUE
  )
  bc_dat2 <- as.data.frame(
    1 / abs(twoClassSim(30)[, 1:5]),
    stringsAsFactors = TRUE
  )
  bc_dat1[1, 3] <- NA
  bc_dat1[13, 5] <- NA

  bc_dat2_pp <- preProcess(bc_dat1, method = "BoxCox")
  bc_dat2_bc <- predict(bc_dat2_pp, bc_dat2)

  bc_trans <- lapply(bc_dat1, function(x) {
    x <- x[!is.na(x)]
    MASS::boxcox(x ~ rep(1, length(x)), plotit = FALSE)
  })

  bc_dat2_bc_exp <- bc_dat2
  for (i in seq_len(ncol(bc_dat2))) {
    lambda <- bc_trans[[i]]$x[which.max(bc_trans[[i]]$y)]
    bc_dat2_bc_exp[, i] <- (bc_dat2_bc_exp[, i]^lambda - 1) / lambda
  }
  expect_equal(bc_dat2_bc_exp, bc_dat2_bc)
})

###################################################################
## test YJ trans

test_that('Yeo-Johnson trans', {
  skip_on_cran()
  set.seed(1)
  yj_dat1 <- as.data.frame(
    1 / abs(twoClassSim(30)[, 1:5]),
    stringsAsFactors = TRUE
  )
  yj_dat2 <- as.data.frame(
    1 / abs(twoClassSim(30)[, 1:5]),
    stringsAsFactors = TRUE
  )

  yj_dat2_pp <- preProcess(yj_dat1, method = "YeoJohnson")
  yj_dat2_yj <- predict(yj_dat2_pp, yj_dat2)

  ## values derived from
  ##  car::powerTransform(lm(x ~ rep(1, length(x))),
  ##                      family = "yjPower")

  yj_lambda_exp <- structure(
    c(
      -1.17126152733178,
      -0.824731066670367,
      -0.706588079282242,
      -1.03306249319229,
      -0.75439768809812
    ),
    .Names = c("TwoFactor1", "TwoFactor2", "Linear01", "Linear02", "Linear03")
  )

  expect_equal(yj_lambda_exp, yj_dat2_pp$yj, tolerance = 0.00001)

  # fmt: skip
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
  yj_dat1 <- as.data.frame(
    1 / abs(twoClassSim(30)[, 1:5]),
    stringsAsFactors = TRUE
  )
  yj_dat2 <- as.data.frame(
    1 / abs(twoClassSim(30)[, 1:5]),
    stringsAsFactors = TRUE
  )
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

  # fmt: skip
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
                       0.325877110845926, 0.556910621166843, 0.656052500704136, 0.24465996399532),
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
                     0.753293517878738, 0.434506299792077, 0.615740571333, 0.751245083135553),
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
  dat <- data.frame(
    x1 = 1:50,
    x2 = 1,
    x3 = c(rep(1, 49), 0),
    x4 = c(rep(0, 50), 1:50),
    y = factor(rep(letters[1:2], each = 50))
  )

  no_zv <- preProcess(dat, method = "zv")
  no_nzv <- preProcess(dat, method = "nzv")
  no_xgy <- preProcess(dat, method = "conditionalX", outcome = dat$y)
  filter_mean <- preProcess(
    dat,
    method = list(conditionalX = names(dat)[1:4], center = "x1"),
    outcome = dat$y
  )
  no_zv_pred <- predict(no_zv, dat)
  no_nzv_pred <- predict(no_nzv, dat)
  no_xgy_pred <- predict(no_xgy, dat[, 1:4])
  filter_mean_pred <- predict(filter_mean, dat[, 1:4])

  x1_exp <- dat$x1 - mean(dat$x1)

  expect_named(no_zv_pred, c("x1", "x3", "x4", "y"))
  expect_named(no_nzv_pred, c("x1", "x4", "y"))
  expect_named(no_xgy_pred, c("x1", "x3"))
  expect_named(filter_mean_pred, c("x1", "x3"))
  expect_equal(filter_mean_pred$x1, x1_exp)
})

###################################################################
## imputation and additional transforms

test_that("knnImpute fills in missing values", {
  skip_on_cran()
  skip_if_not_installed("RANN")
  set.seed(1)
  x <- as.data.frame(matrix(rnorm(200), ncol = 5))
  x[3, 1] <- NA
  x[7, 4] <- NA

  pp <- preProcess(x, method = "knnImpute")
  filled <- predict(pp, x)
  expect_all_false(as.vector(is.na(filled)))
})

test_that("bagImpute fills in missing values", {
  skip_on_cran()
  skip_if_not_installed("ipred")
  set.seed(1)
  x <- as.data.frame(matrix(rnorm(200), ncol = 5))
  x[3, 1] <- NA

  pp <- preProcess(x, method = "bagImpute")
  filled <- predict(pp, x)
  expect_all_false(as.vector(is.na(filled)))
})

test_that("expoTrans and invHyperbolicSine transforms run and invert", {
  skip_on_cran()
  set.seed(1)
  x <- data.frame(a = rexp(50) + 1, b = rexp(50) + 1)

  et <- predict(preProcess(x, method = "expoTrans"), x)
  expect_identical(dim(et), dim(x))

  ihs <- predict(preProcess(x, method = "invHyperbolicSine"), x)
  # applied elementwise; equal (not identical) since caret uses log(x+sqrt(...))
  expect_equal(ihs$a, asinh(x$a))
})

test_that("print.preProcess summarises the transformations", {
  skip_on_cran()
  set.seed(1)
  x <- as.data.frame(matrix(rnorm(40), ncol = 4))
  # a fixed object with no fitted values -> deterministic print
  expect_snapshot(print(preProcess(x, method = c("center", "scale"))))
})

# --- Box-Cox lambda estimation -----------------------------------------------

test_that("BoxCox lambdas match MASS::boxcox", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  set.seed(1)
  dat <- matrix(runif(30), ncol = 3)
  dat[, 1] <- exp(dat[, 1])
  colnames(dat) <- paste0("x", 1:3)

  check_BoxCox <- function(x, expected = NULL) {
    pp1 <- preProcess(x, method = "BoxCox")
    obs_lambdas1 <- unlist(lapply(pp1$bc, function(x) x$lambda))
    names(obs_lambdas1) <- NULL
    expect_equal(obs_lambdas1, expected)
  }

  exp_lambdas <- rep(NA, 3)
  for (i in seq_len(ncol(dat))) {
    tmp <- as.data.frame(dat, stringsAsFactors = TRUE)[, i, drop = FALSE]
    names(tmp)[1] <- "x"
    tmp_bc <- MASS::boxcox(
      x ~ 1,
      data = tmp,
      plotit = FALSE,
      lambda = seq(-2, 2, by = 0.1)
    )
    exp_lambdas[i] <- tmp_bc$x[which.max(tmp_bc$y)]
  }

  check_BoxCox(dat, expected = exp_lambdas)
  check_BoxCox(
    as.data.frame(dat, stringsAsFactors = TRUE),
    expected = exp_lambdas
  )
})

###################################################################
## transformations that cannot be estimated for every column

test_that("YeoJohnson skips the columns it cannot estimate", {
  skip_on_cran()

  # two distinct values is too few to estimate a transformation from, so that
  # column is left out of the ones the object carries
  dat <- data.frame(
    ok = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    too_few = rep(c(1, 2), 5)
  )
  pp <- preProcess(dat, method = "YeoJohnson")
  expect_named(pp$yj, "ok")
  expect_identical(pp$method$YeoJohnson, "ok")
})

test_that("YeoJohnson reports when nothing could be transformed", {
  skip_on_cran()

  dat <- data.frame(a = rep(c(1, 2), 5), b = rep(c(3, 4), 5))
  # with every column refused the transformation is dropped altogether
  expect_snapshot(pp <- preProcess(dat, method = "YeoJohnson", verbose = TRUE))
  expect_null(pp$yj)
})

test_that("expoTrans is dropped for columns it sends to infinity", {
  skip_on_cran()

  # the exponential transformation of large values overflows
  set.seed(1508)
  dat <- data.frame(big = c(rnorm(20, 100, 10), 1000), small = rnorm(21))
  expect_snapshot_warning(pp <- preProcess(dat, method = "expoTrans"))
  expect_false("big" %in% names(pp$et))
})

test_that("expoTrans works on a matrix", {
  skip_on_cran()

  # matrices are transformed column-wise with apply() rather than lapply()
  set.seed(9143)
  x <- cbind(a = rnorm(30, 10), b = rnorm(30, 20))
  pp <- preProcess(x, method = "expoTrans")
  expect_named(pp$et, c("a", "b"))
  expect_identical(dim(predict(pp, x)), dim(x))
})

test_that("scaling substitutes one for a standard deviation it cannot compute", {
  skip_on_cran()

  # an all-missing column has no standard deviation, so scaling would divide by
  # NA; the column is scaled by one instead
  dat <- data.frame(ok = c(1, 5, 9, 13), empty = rep(NA_real_, 4))
  expect_snapshot_warning(pp <- preProcess(dat, method = c("center", "scale")))
  expect_identical(unname(pp$std["empty"]), 1)
})

test_that("preProcess keeps the components that reach the variance threshold", {
  skip_on_cran()

  set.seed(6017)
  x <- matrix(rnorm(300), ncol = 6)
  colnames(x) <- paste0("x", 1:6)
  # thresh picks the number of components, unlike pcaComp which sets it
  pp <- preProcess(x, method = "pca", thresh = 0.75)
  expect_lt(pp$numComp, 6)
  expect_equal(ncol(predict(pp, x)), pp$numComp)
})

###################################################################
## progress reporting

test_that("preProcess reports its progress when asked", {
  skip_on_cran()
  skip_if_not_installed("ipred")
  skip_if_not_installed("RANN")

  dat <- data.frame(
    a = c(1, 2, NA, 4, 5, 6, 7, 8),
    b = c(2, 4, 6, 8, NA, 12, 14, 16),
    c = c(1, 3, 5, 7, 9, 11, 13, NA)
  )

  # each method announces itself, and the imputers say when they are done
  expect_snapshot(pp <- preProcess(dat, method = "bagImpute", verbose = TRUE))
  expect_snapshot(
    pp <- preProcess(dat, method = "medianImpute", verbose = TRUE)
  )
  expect_snapshot(pp <- preProcess(dat, method = "range", verbose = TRUE))
})

test_that("preProcess reports progress for the component methods", {
  skip_on_cran()
  skip_if_not_installed("fastICA")

  set.seed(3729)
  dat <- as.data.frame(matrix(rnorm(120), ncol = 4))
  expect_snapshot(
    pp <- preProcess(dat, method = "ica", n.comp = 2, verbose = TRUE)
  )
})

###################################################################
## predict.preProcess

test_that("predict.preProcess imputes missing rows of a matrix", {
  skip_on_cran()
  skip_if_not_installed("RANN")

  x <- cbind(a = c(1, 2, 3, 4, 5, 6), b = c(2, 4, 6, 8, 10, 12))
  pp <- preProcess(x, method = "knnImpute", k = 2)

  new_x <- cbind(a = c(1, NA), b = c(2, 8))
  out <- predict(pp, new_x)
  # the imputed matrix keeps its shape and has no missing values left
  expect_identical(dim(out), dim(new_x))
  expect_all_false(is.na(as.vector(out)))
})

test_that("predict.preProcess bag-imputes missing rows of a matrix", {
  skip_on_cran()
  skip_if_not_installed("ipred")

  set.seed(4291)
  x <- cbind(a = rnorm(20), b = rnorm(20), c = rnorm(20))
  pp <- preProcess(x, method = "bagImpute")

  new_x <- x[1:3, ]
  new_x[2, "a"] <- NA
  out <- predict(pp, new_x)
  expect_all_false(is.na(as.vector(out)))
})

test_that("predict.preProcess median-imputes a matrix without column names", {
  skip_on_cran()

  x <- cbind(a = c(1, 2, 3, 4, 5), b = c(2, 4, 6, 8, 10))
  pp <- preProcess(x, method = "medianImpute")

  new_x <- x[1:2, ]
  new_x[1, 1] <- NA
  out <- predict(pp, new_x)
  expect_all_false(is.na(as.vector(out)))
})

test_that("predict.preProcess names a single principal component", {
  skip_on_cran()

  set.seed(8620)
  x <- data.frame(a = rnorm(20), b = rnorm(20))
  pp <- preProcess(x, method = "pca", pcaComp = 1)
  out <- predict(pp, x)
  expect_named(out, "PC1")
})

test_that("predict.preProcess errors when everything was filtered out", {
  skip_on_cran()

  # a constant predictor is removed, leaving nothing to return
  dat <- data.frame(a = rep(1, 10))
  pp <- preProcess(dat, method = "zv")
  expect_snapshot(predict(pp, dat), error = TRUE)
})

test_that("predict.preProcess keeps the columns it was told to keep", {
  skip_on_cran()

  set.seed(2504)
  dat <- data.frame(a = rnorm(20), b = rnorm(20))
  dat$dup <- dat$a + rnorm(20, sd = 0.01)

  # the columns that went into the components are normally dropped; naming one
  # under `keep` holds on to it. The methods have to be given per column, since
  # only the list form of `method` can say which columns `keep` refers to
  pp <- preProcess(
    dat,
    method = list(
      pca = c("a", "dup"),
      keep = "b",
      center = c("a", "dup"),
      scale = c("a", "dup")
    ),
    pcaComp = 1
  )
  expect_named(predict(pp, dat), c("b", "PC1"))
})

test_that("preProcess can be given a plain character method vector to predict", {
  skip_on_cran()

  # objects from older versions carry a character vector rather than a list, and
  # predict() converts it before using it
  dat <- data.frame(a = c(1, 5, 9, 13), b = c(2, 4, 6, 8))
  pp <- preProcess(dat, method = c("center", "scale"))
  pp$method <- c("center", "scale")
  out <- predict(pp, dat)
  expect_equal(unname(colMeans(out)), c(0, 0))
})

test_that("preProcess warns when the correlation matrix cannot be computed", {
  skip_on_cran()

  # every column is constant, so the zero-variance filter removes them all and
  # there is nothing left to correlate
  dat <- data.frame(a = rep(1, 5), b = rep(2, 5))
  expect_snapshot_warning(pp <- preProcess(dat, method = c("zv", "corr")))
  expect_null(pp$method$corr)
})

test_that("preProcess has nothing to correlate with a single predictor", {
  skip_on_cran()

  # the all-missing column is dropped as zero variance, leaving one predictor;
  # a correlation filter needs two, so it is skipped rather than erroring
  dat <- data.frame(a = c(1, 2, 3, 4), b = rep(NA_real_, 4))
  pp <- suppressWarnings(preProcess(dat, method = "corr"))
  expect_identical(pp$method$remove, "b")
  expect_null(pp$method$corr)
})

test_that("preProcess keeps every component when none reaches the threshold", {
  skip_on_cran()

  set.seed(7482)
  x <- matrix(rnorm(60), ncol = 3)
  colnames(x) <- c("a", "b", "c")
  # no cumulative proportion of variance can exceed one
  pp <- preProcess(x, method = "pca", thresh = 1.5)
  expect_identical(pp$numComp, 3L)
})

test_that("predict.preProcess median-imputes a matrix with no column names", {
  skip_on_cran()

  x <- cbind(a = c(1, 2, 3, 4, 5), b = c(2, 4, 6, 8, 10))
  pp <- preProcess(x, method = "medianImpute")

  # preProcess needs names to work out the column types, but the data being
  # predicted may not have them, and then the columns to impute are found by
  # position instead
  new_x <- x[1:2, ]
  new_x[1, 1] <- NA
  colnames(new_x) <- NULL
  out <- predict(pp, new_x)
  expect_all_false(is.na(as.vector(out)))
})

test_that("nearest-neighbour imputation needs something to go on", {
  skip_on_cran()
  skip_if_not_installed("RANN")

  x <- cbind(a = c(1, 2, 3, 4, 5), b = c(2, 4, 6, 8, 10))
  pp <- preProcess(x, method = "knnImpute", k = 2)

  all_missing <- rbind(c(a = NA_real_, b = NA_real_))
  expect_snapshot(predict(pp, all_missing), error = TRUE)
})

test_that("preProcess applies the spatial sign to the components", {
  skip_on_cran()

  set.seed(5169)
  dat <- data.frame(a = rnorm(30), b = rnorm(30), c = rnorm(30))

  # "_PC_" is the wildcard for "whatever comes out of PCA"
  pp <- preProcess(
    dat,
    method = list(
      pca = c("a", "b", "c"),
      spatialSign = "_PC_",
      center = c("a", "b", "c"),
      scale = c("a", "b", "c")
    ),
    pcaComp = 2
  )
  out <- predict(pp, dat)
  expect_named(out, c("PC1", "PC2"))
  # the spatial sign projects each row onto the unit circle
  expect_equal(unname(rowSums(out^2)), rep(1, nrow(dat)))

  # and the object says so when printed
  expect_snapshot(print(pp))
})

test_that("preProcess applies the spatial sign to independent components", {
  skip_on_cran()
  skip_if_not_installed("fastICA")

  set.seed(4526)
  dat <- data.frame(a = rnorm(30), b = rnorm(30), c = rnorm(30))

  pp <- preProcess(
    dat,
    method = list(
      ica = c("a", "b", "c"),
      spatialSign = "_IC_",
      center = c("a", "b", "c"),
      scale = c("a", "b", "c")
    ),
    n.comp = 2
  )
  out <- predict(pp, dat)
  expect_named(out, c("ICA1", "ICA2"))
  expect_snapshot(print(pp))
})

test_that("predict.preProcess keeps a column asked for alongside components", {
  skip_on_cran()
  skip_if_not_installed("fastICA")

  set.seed(3078)
  dat <- data.frame(a = rnorm(30), b = rnorm(30), c = rnorm(30))
  pp <- preProcess(
    dat,
    method = list(
      ica = c("a", "c"),
      keep = "b",
      center = c("a", "c"),
      scale = c("a", "c")
    ),
    n.comp = 2
  )
  expect_named(predict(pp, dat), c("b", "ICA1", "ICA2"))
})

test_that("print.preProcess counts the Box-Cox transformations it could not fit", {
  skip_on_cran()

  set.seed(2841)
  dat <- data.frame(a = rlnorm(30), b = rlnorm(30))
  pp <- preProcess(dat, method = "BoxCox")

  # objects made before caret dropped the failures carry a missing lambda
  pp$bc$b$lambda <- NA_real_
  expect_snapshot(print(pp))
})
