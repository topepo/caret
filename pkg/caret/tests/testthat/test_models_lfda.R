library(caret)

test_that('test lfda model training and prediction', {
  skip_on_cran()
  set.seed(1)
  tr_dat <- twoClassSim(200)
  te_dat <- twoClassSim(200)
  
  lfda.model <- lfda(x=tr_dat[,-16],y=tr_dat[,16],r=3)
})


