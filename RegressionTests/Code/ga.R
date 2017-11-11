timestamp <- Sys.time()
library(caret)
library(plyr)
library(recipes)
library(dplyr)

model <- "ga"

###################################################################
##

set.seed(1)
class_dat <- twoClassSim(100, noiseVars = 10)
class_dat2 <- twoClassSim(100, noiseVars = 10)

ga_class_ctrl <- gafsControl(functions = caretGA, 
                             method = "cv",
                             number = 3,
                             genParallel = FALSE,
                             allowParallel = FALSE)

set.seed(12)
test_class_caret <- gafs(x = class_dat[, -ncol(class_dat)],
                         y = class_dat$Class,
                         iters = 3,
                         method = "lda",
                         popSize = 8,
                         trControl = trainControl(method = "cv", classProbs = TRUE),
                         gafsControl = ga_class_ctrl)

ga_class_ctrl$functions <- treebagGA
set.seed(12)
test_class_bag <- gafs(x = class_dat[, -ncol(class_dat)],
                       y = class_dat$Class,
                       iters = 3,
                       gafsControl = ga_class_ctrl)

ga_class_ctrl$functions <- rfGA
set.seed(12)
test_class_rf <- gafs(x = class_dat[, -ncol(class_dat)],
                      y = class_dat$Class,
                      iters = 3,
                      gafsControl = ga_class_ctrl)


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

ga_reg_ctrl <- gafsControl(functions = caretGA, 
                           method = "cv",
                           number = 2,
                           genParallel = FALSE,
                           allowParallel = FALSE)

set.seed(12)
test_reg_caret <- gafs(x = reg_dat[, -ncol(reg_dat)],
                       y = reg_dat$y,
                       iters = 3,
                       method = "lm",
                       trControl = trainControl(method = "cv"),
                       gafsControl = ga_reg_ctrl)

ga_reg_ctrl$functions <- treebagGA
set.seed(12)
test_reg_bag <- gafs(x = reg_dat[, -ncol(reg_dat)],
                     y = reg_dat$y,
                     iters = 3,
                     gafsControl = ga_reg_ctrl)

ga_reg_ctrl$functions <- rfGA
set.seed(12)
test_reg_rf <- gafs(x = reg_dat[, -ncol(reg_dat)],
                    y = reg_dat$y,
                    iters = 3,
                    gafsControl = ga_reg_ctrl)

ga_reg_ctrl$metric <- c(internal = "Rsquared", external = "RMSE")
ga_reg_ctrl$maximize <- c(internal = TRUE, external = FALSE)
set.seed(12)
test_reg_metric <- gafs(x = reg_dat[, -ncol(reg_dat)],
                        y = reg_dat$y,
                        iters = 3,
                        gafsControl = ga_reg_ctrl)

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

