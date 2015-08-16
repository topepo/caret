# library(caret)
# library(testthat)
# library(lfda)
#
# test_that('test lfda model training and prediction', {
#   skip_on_cran()
#   set.seed(1)
#   x <- iris[,-5]
#   y <- iris[,5]
#
#   modelInfo <- getModelInfo('lfda', regex=FALSE)[[1]]
#   fit <- modelInfo$fit(x, y, modelInfo$grid(1))
#   predict <- modelInfo$predict(fit, x)
#   probs <- modelInfo$prob(fit, x)
#
#   lfda.model <- train(x,y,method = modelInfo)
# })
#
#
