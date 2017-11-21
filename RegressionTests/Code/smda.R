timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "smda"



#########################################################################

set.seed(2)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ TwoFactor1 + TwoFactor2 + Linear1, 
                  data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

cctrl1 <- trainControl(method = "cv", number = 3, 
                       returnResamp = "all", seeds = seeds)
cctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
cctrl3 <- trainControl(method = "none", seeds = seeds)
cctrlR <- trainControl(method = "cv", number = 3, 
                       returnResamp = "all", search = "random", 
                       seeds = seeds)

set.seed(849)
test_class_cv_model <- train(trainX[, 1:3], trainY, 
                             method = "smda", 
                             trControl = cctrl1,
                             tuneGrid = expand.grid(.NumVars = 2:3,
                                                    .R = 2:3,
                                                    .lambda = c(.1, .2)),
                             preProc = c("center", "scale"))

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training[, 5:8], 
                            method = "smda", 
                            trControl = cctrl1,
                            tuneGrid = expand.grid(.NumVars = 2:3,
                                                   .R = 2:3,
                                                   .lambda = c(.1, .2)),
                            preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_rand <- train(trainX[, 1:3], trainY, 
                         method = "smda", 
                         trControl = cctrlR,
                         tuneLength = 4, 
                         preProc = c("center", "scale"))

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

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "smda", 
                        tuneGrid = expand.grid(NumVars = 2:3,
                                               R = 2:3,
                                               lambda = c(.1, .2)),
                        trControl = cctrl1)


if(
  !isTRUE(
    all.equal(test_class_cv_model$results, 
              test_class_rec$results))
)
  stop("CV weights not giving the same results")

test_class_imp_rec <- varImp(test_class_rec)


test_class_pred_rec <- predict(test_class_rec, testing[, -ncol(testing)])

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

test_class_predictors1 <- predictors(test_class_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")

