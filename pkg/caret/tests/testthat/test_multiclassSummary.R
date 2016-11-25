context('multiClassSummary')

test_that("multiClassSummary presenting warnings from train", {

library(caret)
N = 1000;
M = 2;
set.seed(1)
xTrain = matrix( runif(N*M), nrow = N);
colnames(xTrain) <-
  sapply(1:M, function(u) paste0(collapse = '', letters[sample(26, 3, replace = TRUE)] ) )
yTrain = as.factor( letters[sample(3, N, replace = TRUE)])

trCntlListMulti  <- trainControl(method = "cv", number = 3, verboseIter = FALSE, classProbs = TRUE,
                                 summaryFunction = multiClassSummary )

expect_silent({
  enFitMulti <-
    train( x = xTrain, y = yTrain, trControl = trCntlListMulti,
           method = "knn", tuneLength = 2 )
})

})
