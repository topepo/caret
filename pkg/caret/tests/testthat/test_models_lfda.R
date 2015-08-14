library(caret)
library(lfda)
library(testthat)

test_that('test lfda model training and prediction', {
  skip_on_cran()
  set.seed(1)
  tr_dat <- twoClassSim(200)
  te_dat <- twoClassSim(200)

#   lfda.model <- train(
#     x=tr_dat[,-16],y=tr_dat[,16],
#     metric = "plain",
#     method = "lfda"
#   )
#
#   # lfda.model <- lfda(x=tr_dat[,-16],y=tr_dat[,16],r=3)
#
#   transform.metric <- lfda.model$T
#   transformed.train <- lfda.model$Z

  # transformed.test <- predict(lfda.model, newdata=te_dat[,-16])

})


