timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "CSimca"



#########################################################################

set.seed(1)
training <- twoClassSim(50, linearVars = 2)
testing <- twoClassSim(500, linearVars = 2)
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all", seeds = seeds)
cctrl2 <- trainControl(method = "LOOCV", seeds = seeds)
cctrl3 <- trainControl(method = "none", seeds = seeds)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "CSimca", 
                             trControl = cctrl1,
                             preProc = c("center", "scale"))

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "CSimca", 
                            trControl = cctrl1,
                            preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                            method = "CSimca", 
                            trControl = cctrl2,
                            preProc = c("center", "scale"))

set.seed(849)
test_class_none_model <- train(trainX, trainY, 
                               method = "CSimca", 
                               trControl = cctrl3,
                               tuneLength = 1,
                               preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "CSimca", 
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

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


