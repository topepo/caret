library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "smda"

#########################################################################

set.seed(2)
training <- twoClassSim(100)
testing <- twoClassSim(500)
trainX <- training[, -ncol(training)]
trainY <- training$Class

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
cctrl2 <- trainControl(method = "LOOCV")
cctrl3 <- trainControl(method = "none")

set.seed(849)
test_class_cv_model <- train(trainX[, 1:3], trainY, 
                             method = "smda", 
                             trControl = cctrl1,
                             tuneGrid = expand.grid(.NumVars = 2:3,
                                                    .R = 2:3,
                                                    .lambda = c(.1, .2)),
                             preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, 1:3])
# 
# set.seed(849)
# test_class_loo_model <- train(trainX[, 1:3], trainY, 
#                               method = "smda", 
#                               trControl = cctrl2,
#                               tuneGrid = expand.grid(.NumVars = 2:3,
#                                                      .R = 2:3,
#                                                      .lambda = c(.1, .2)),
#                               preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- train(trainX[, 1:3], trainY, 
                               method = "smda", 
                               trControl = cctrl3,
                               tuneGrid = test_class_cv_model$bestTune,
                               preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, 1:3])

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")

