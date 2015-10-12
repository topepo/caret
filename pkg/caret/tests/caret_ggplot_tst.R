rm(list = ls())

devtools::install_github('topepo/caret/pkg/caret')
require(caret)

library(mlbench)
data(BostonHousing)

set.seed(1)
# Create models to check plots with different number of tuning parameters
mdl.avNNet <- train(medv ~ ., data = BostonHousing, "avNNet",
                 trControl=trainControl(method = "repeatedcv"))
mdl.bartMachine <- train(medv ~ ., data = BostonHousing, "bartMachine",
                    trControl=trainControl(method = "repeatedcv"))
mdl.dnn <- train(medv ~ ., data = BostonHousing, "dnn",
                    trControl=trainControl(method = "repeatedcv"),
                 tuneGrid=expand.grid(layer1=c(1, 2, 3),
                                      layer2=c(0, 1, 2),
                                      layer3=c(0, 1, 2),
                                      hidden_dropout=c(0, 1),
                                      visible_dropout=c(0, 1)))
mdl.gbm <- train(medv ~ ., data = BostonHousing, "gbm",
                 trControl=trainControl(method = "repeatedcv"),
                 tuneGrid=expand.grid(shrinkage=c(0.1, 0.2, 0.3),
                                      interaction.depth=c(1, 2, 3),
                                      n.minobsinnode=c(10, 20, 30),
                                      n.trees=c(50, 100, 150)))
mdl.glm <- train(medv ~ ., data = BostonHousing, "glm",
                    trControl=trainControl(method = "repeatedcv"))
mdl.glmnet <- train(medv ~ ., data = BostonHousing, "glmnet",
                    trControl=trainControl(method = "repeatedcv"))
mdl.rf.rcv <- train(medv ~ ., data = BostonHousing, "rf",
                    trControl=trainControl(method = "repeatedcv"))
mdl.rf.oob <- train(medv ~ ., data = BostonHousing, "rf",
                    trControl=trainControl(method = "oob"))
mdl.rpart <- train(medv ~ ., data = BostonHousing, "rpart",
                    trControl=trainControl(method = "repeatedcv"))
mdl.svmPoly <- train(medv ~ ., data = BostonHousing, "svmPoly",
                   trControl=trainControl(method = "repeatedcv"))
mdl.svmRadial <- train(medv ~ ., data = BostonHousing, "svmRadial",
                   trControl=trainControl(method = "repeatedcv"),
                   tuneGrid=expand.grid(sigma=c(0.1, 0.2, 0.3), C=c(0.25, 0.5, 1.0)))

mdls.df <- modelLookup()
mdls.nParams <- table(mdls.df$model)
print(mdls.nParams[mdls.nParams == 5])
print(subset(mdls.df, model %in% c("dnn")))

source("../R/confusionMatrix.R")
source("../R/ggplot.R")
# nParams == 0
ggplot(mdl.glm, plotType = "scatter")
ggplot(mdl.glm, plotType = "level")

# nParams == 1
ggplot(mdl.rpart, plotType = "scatter")
ggplot(mdl.rpart, plotType = "level")

ggplot(mdl.rf.rcv)
ggplot(mdl.rf.oob)

# nParams == 2
ggplot(mdl.glmnet, plotType = "scatter")
ggplot(mdl.glmnet, plotType = "level")

# nParams == 3
ggplot(mdl.svmPoly, plotType = "scatter")
ggplot(mdl.svmPoly, plotType = "level")

# nParams == 4
ggplot(mdl.gbm, plotType = "scatter")
ggplot(mdl.gbm, plotType = "level")

# nParams == 5
ggplot(mdl.dnn, plotType = "scatter")
ggplot(mdl.dnn, plotType = "level")

sessionInfo()

# To debug ggplot without going into debugger
# data = mdl.svmPoly; dnm <- c("RMSE", as.character(subset(mdls.df, model %in% c("svmPoly"))$parameter))
# params = names(data$bestTune)
# dat = data$results
# paramData = data$modelInfo$parameters
# p = length(params)
#
# if(p > 1 && is.numeric(dat[, dnm[3]])) dat[, dnm[3]] <- factor(format(dat[, dnm[3]]))
# resampText <- resampName(data, FALSE)
# bstRes <- data$results
# for (par in as.character(params))
#   bstRes <- bstRes[which(bstRes[, par] == data$bestTune[, par]), ]
# if (nrow(bstRes) > 1)
#   stop("problem in extracting model$bestTune row from model$results")
# print("bstRes:"); print(bstRes)
#
# bstRes[, dnm[4]] <- factor(bstRes[, dnm[4]],
#                            levels = as.character(unique(dat[, dnm[4]])))
#    dat[, dnm[4]] <- factor(   dat[, dnm[4]],
#                            levels = as.character(unique(dat[, dnm[4]])))
# out <- ggplot(dat, aes_string(x = dnm[2], y = dnm[1]))
# out <- out + ylab(resampText)
# out <- out + xlab(paramData$label[paramData$parameter == dnm[2]])
# leg_name <- paramData$label[paramData$parameter == dnm[3]]
# out <- out + geom_point(aes_string(color = dnm[3], shape = dnm[3]))
# # out <- out + geom_point(aes_string(color = dnm[3]))
# # out <- out + geom_point()
# out <- out + geom_line(aes_string(color = dnm[3], shape = dnm[3]))
# # out <- out + geom_line()
# out <- out + scale_colour_discrete(name = leg_name)
# out <- out + scale_shape_discrete(name = leg_name)
# out <- out + facet_wrap(as.formula(paste("~", dnm[4])))
# out <- out + geom_point(data = bstRes,
#                         x = as.numeric(bstRes[, dnm[2]]),
#                         y = as.numeric(bstRes[, dnm[1]]),
#                         size = 4, shape = 5)
# print(out)
