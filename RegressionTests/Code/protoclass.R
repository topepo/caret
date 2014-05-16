library(caret)
timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M")

model <- "protoclass"

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
test_class_cv_model <- train(trainX, trainY, 
                             method = "protoclass", 
                             trControl = cctrl1,
                             preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "protoclass", 
                              trControl = cctrl2,
                              preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "protoclass", 
                               trControl = cctrl3,
                               tuneGrid = test_class_cv_model$bestTune,
                               preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()

save(list = c(tests, "sInfo", "timestamp"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

q("no")

