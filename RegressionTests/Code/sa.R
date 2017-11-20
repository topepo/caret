timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "sa"

###################################################################
##

set.seed(1)
class_dat <- twoClassSim(100, noiseVars = 10)
class_dat2 <- twoClassSim(100, noiseVars = 10)

sa_class_ctrl <- safsControl(functions = caretSA, 
                             method = "cv",
                             number = 2,
                             allowParallel = FALSE)

set.seed(12)
test_class_caret <- safs(x = class_dat[, -ncol(class_dat)],
                         y = class_dat$Class,
                         iters = 10,
                         method = "lda",
                         trControl = trainControl(method = "cv", classProbs = TRUE),
                         safsControl = sa_class_ctrl)

sa_class_ctrl$functions <- treebagSA
set.seed(12)
test_class_bag <- safs(x = class_dat[, -ncol(class_dat)],
                       y = class_dat$Class,
                       iters = 10,
                       safsControl = sa_class_ctrl)

sa_class_ctrl$functions <- rfSA
set.seed(12)
test_class_rf <- safs(x = class_dat[, -ncol(class_dat)],
                      y = class_dat$Class,
                      iters = 10,
                      safsControl = sa_class_ctrl)

sa_class_ctrl$functions$initial <- 1:4
set.seed(12)
test_class_initial <- safs(x = class_dat[, -ncol(class_dat)],
                      y = class_dat$Class,
                      iters = 10,
                      safsControl = sa_class_ctrl)

sa_class_ctrl$metric <- c(internal = "Kappa", external = "Accuracy")
set.seed(12)
test_class_metric <- safs(x = class_dat[, -ncol(class_dat)],
                          y = class_dat$Class,
                          iters = 10,
                          safsControl = sa_class_ctrl)



test_class_caret_pred <- predict(test_class_caret, class_dat2)
test_class_bag_pred <- predict(test_class_bag, class_dat2)
test_class_rf_pred <- predict(test_class_rf, class_dat2)

test_class_caret_predictors <- predictors(test_class_caret)
test_class_bag_predictors <- predictors(test_class_bag)
test_class_rf_predictors <- predictors(test_class_rf)

###################################################################
##

set.seed(1)
reg_dat <- SLC14_1(100, noiseVars = 10)
reg_dat2 <- SLC14_1(100, noiseVars = 10)

sa_reg_ctrl <- safsControl(functions = caretSA, 
                           method = "cv",
                           number = 2,
                           allowParallel = FALSE)

set.seed(12)
test_reg_caret <- safs(x = reg_dat[, -ncol(reg_dat)],
                       y = reg_dat$y,
                       iters = 10,
                       method = "lm",
                       trControl = trainControl(method = "cv"),
                       safsControl = sa_reg_ctrl)

sa_reg_ctrl$functions <- treebagSA
sa_reg_ctrl$improve <- 3
set.seed(12)
test_reg_bag <- safs(x = reg_dat[, -ncol(reg_dat)],
                     y = reg_dat$y,
                     iters = 10,
                     safsControl = sa_reg_ctrl)

sa_reg_ctrl$functions <- rfSA
set.seed(12)
test_reg_rf <- safs(x = reg_dat[, -ncol(reg_dat)],
                    y = reg_dat$y,
                    iters = 10,
                    safsControl = sa_reg_ctrl)

sa_reg_ctrl$metric <- c(internal = "Rsquared", external = "RMSE")
sa_reg_ctrl$maximize <- c(internal = TRUE, external = FALSE)
set.seed(12)
test_class_metric <- safs(x = reg_dat[, -ncol(reg_dat)],
                          y = reg_dat$y,
                          iters = 10,
                          safsControl = sa_reg_ctrl)


test_reg_caret_pred <- predict(test_reg_caret, reg_dat2)
test_reg_bag_pred <- predict(test_reg_bag, reg_dat2)
test_reg_rf_pred <- predict(test_reg_rf, reg_dat2)

test_reg_caret_predictors <- predictors(test_reg_caret)
test_reg_bag_predictors <- predictors(test_reg_bag)
test_reg_rf_predictors <- predictors(test_reg_rf)


#########################################################################

tests <- grep("test_", ls(), fixed = TRUE, value = TRUE)

sInfo <- sessionInfo()
timestamp_end <- Sys.time()

save(list = c(tests, "sInfo", "timestamp", "timestamp_end"),
     file = file.path(getwd(), paste(model, ".RData", sep = "")))

if(!interactive())
   q("no")


