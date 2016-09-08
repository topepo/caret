

context('twoClassSummary')


test_that("twoClassSummary is calculating correctly", {

library(caret)

set.seed(1)
tr_dat <- twoClassSim(500)
te_dat <- tr_dat
tr_dat$Class = factor(tr_dat$Class, levels = rev(levels(te_dat$Class)))

set.seed(35)
mod1 <- train(Class ~ ., data = tr_dat,
              method = "fda",
              tuneLength = 10,
              metric = "ROC",
              trControl = trainControl(classProbs = TRUE,
                                       summaryFunction = twoClassSummary))

set.seed(35)
mod2 <- train(Class ~ ., data = te_dat,
              method = "fda",
              tuneLength = 10,
              metric = "ROC",
              trControl = trainControl(classProbs = TRUE,
                                       summaryFunction = twoClassSummary))

expect_equal(mod1$resample$ROC, mod2$resample$ROC)
expect_equal(mod1$resample$Sens, mod2$resample$Spec)
expect_equal(mod1$resample$Spec, mod2$resample$Sens)

})
