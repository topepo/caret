context('Box Cox transformations')

###################################################################
## Generate data and do BC using the source function to get
## expected results

library(MASS)

set.seed(1)
dat <- matrix(runif(30), ncol = 3)
dat[,1] <- exp(dat[,1])
colnames(dat) <- paste0("x", 1:3)

exp_lambdas <- rep(NA, 3)
for(i in 1:ncol(dat)) {
  tmp <- as.data.frame(dat, stringsAsFactors = TRUE)[,i,drop = FALSE]
  names(tmp)[1] <- "x"
  tmp_bc <- boxcox(x ~ 1, data = tmp, plotit = FALSE, lambda = seq(-2, 2, by = .1))
  exp_lambdas[i] <- tmp_bc$x[which.max(tmp_bc$y)]
}

check_BoxCox <- function(x, expected = NULL) {
  pp1 <- preProcess(x, method = "BoxCox")
  obs_lambdas1 <- unlist(lapply(pp1$bc, function(x) x$lambda))
  names(obs_lambdas1) <- NULL
  expect_equal(obs_lambdas1, expected)
}

check_BoxCox(dat, expected = exp_lambdas)
check_BoxCox(as.data.frame(dat, stringsAsFactors = TRUE), expected = exp_lambdas)

