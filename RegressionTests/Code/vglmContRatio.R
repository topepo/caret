timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "vglmContRatio"


  

#########################################################################

set.seed(2)
training <- twoClassSim(100, ordinal = TRUE)
testing <- twoClassSim(500, ordinal = TRUE)
trainX <- training[, -ncol(training)]
trainY <- training$Class

rec_cls <- recipe(Class ~ ., data = training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

weight_test <- function (data, lev = NULL, model = NULL)  {
  mean(data$weights)
  postResample(data[, "pred"], data[, "obs"])
}

grid <- expand.grid(parallel = TRUE, link = c("logit", "probit"))

seeds <- vector(mode = "list", length = nrow(training) + 1)
seeds <- lapply(seeds, function(x) 1:20)

cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all",
                       classProbs = TRUE, seeds = seeds)
cctrl2 <- trainControl(method = "LOOCV",
                       classProbs = TRUE, seeds = seeds)
cctrl3 <- trainControl(method = "none",
                       classProbs = TRUE, seeds = seeds)

cctrl4 <- trainControl(method = "cv", number = 3, 
                       summaryFunction = weight_test)
cctrl5 <- trainControl(method = "LOOCV", summaryFunction = weight_test)

set.seed(849)
test_class_cv_model <- train(trainX, trainY, 
                             method = "vglmContRatio", 
                             trControl = cctrl1,
                             tuneGrid = grid,
                             metric = "Kappa", 
                             preProc = c("center", "scale"))

set.seed(849)
test_class_cv_form <- train(Class ~ ., data = training, 
                            method = "vglmContRatio", 
                            trControl = cctrl1,
                            tuneGrid = grid,
                            metric = "Kappa", 
                            preProc = c("center", "scale"))

test_class_pred <- predict(test_class_cv_model, testing[, -ncol(testing)])
test_class_prob <- predict(test_class_cv_model, testing[, -ncol(testing)], type = "prob")
test_class_pred_form <- predict(test_class_cv_form, testing[, -ncol(testing)])
test_class_prob_form <- predict(test_class_cv_form, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_loo_model <- train(trainX, trainY, 
                              method = "vglmContRatio", 
                              trControl = cctrl2,
                              tuneGrid = grid,
                              metric = "Kappa", 
                              preProc = c("center", "scale"))

set.seed(849)

test_class_none_model <- train(trainX, trainY, 
                               method = "vglmContRatio", 
                               trControl = cctrl3,
                               tuneGrid = grid[1,],
                               metric = "Kappa", 
                               preProc = c("center", "scale"))

test_class_none_pred <- predict(test_class_none_model, testing[, -ncol(testing)])
test_class_none_prob <- predict(test_class_none_model, testing[, -ncol(testing)], type = "prob")

set.seed(849)
test_class_cv_weight <- train(trainX, trainY, 
                              weights = runif(nrow(trainX)),
                              method = "vglmContRatio", 
                              trControl = cctrl4,
                              tuneGrid = grid,
                              metric = "Accuracy", 
                              preProc = c("center", "scale"))

set.seed(849)
test_class_loo_weight <- train(trainX, trainY, 
                               weights = runif(nrow(trainX)),
                               method = "vglmContRatio", 
                               trControl = cctrl5,
                               tuneGrid = grid,
                               metric = "Accuracy", 
                               preProc = c("center", "scale"))

set.seed(849)
test_class_rec <- train(x = rec_cls,
                        data = training,
                        method = "vglmContRatio", 
                        trControl = cctrl1,
                        tuneGrid = grid)


if(
  !isTRUE(
    all.equal(test_class_cv_model$results, 
              test_class_rec$results))
)
  stop("CV weights not giving the same results")

test_class_imp_rec <- varImp(test_class_rec)


test_class_pred_rec <- predict(test_class_rec, testing[, -ncol(testing)])
test_class_prob_rec <- predict(test_class_rec, testing[, -ncol(testing)], 
                               type = "prob")

test_levels <- levels(test_class_cv_model)
if(!all(levels(trainY) %in% test_levels))
  cat("wrong levels")

#########################################################################

test_class_predictors1 <- caret:::predictors.train(test_class_cv_model)

#########################################################################

test_class_imp <- varImp(test_class_cv_model)

#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


