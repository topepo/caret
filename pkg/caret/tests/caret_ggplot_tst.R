rm(list = ls())
setwd("~/Documents/Work/PullRequests/caret/pkg/caret/tests")

devtools::install_github('topepo/caret/pkg/caret')
require(caret)
print(packageVersion("caret"))
require(grid)

library(mlbench)
data(BostonHousing)

set.seed(1)
# Create models to check plots with different number of tuning parameters
mdl.avNNet <- train(medv ~ ., data = BostonHousing, "avNNet",
                 trControl=trainControl(method = "repeatedcv"))
# mdl.bartMachine <- train(medv ~ ., data = BostonHousing, "bartMachine",
#                     trControl=trainControl(method = "repeatedcv"))
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
#ggplot(mdl.glm, plotType = "scatter")
#ggplot(mdl.glm, plotType = "level")

# nParams == 1
gp.sct.nohi.rpart <- ggplot(mdl.rpart, plotType = "scatter")
gp.sct.yshi.rpart <- ggplot(mdl.rpart, plotType = "scatter", highBestTune = TRUE)
#gp.lvl.rpart      <- ggplot(mdl.rpart, plotType = "level")

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(ggplot(mdl.rf.rcv),   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(ggplot(mdl.rf.rcv, highBestTune = TRUE),
                            vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(ggplot(mdl.rf.oob),   vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(ggplot(mdl.rf.oob, highBestTune = TRUE),
                            vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

# nParams == 2
gp.sct.nohi.glmnet <- ggplot(mdl.glmnet, plotType = "scatter")
gp.sct.yshi.glmnet <- ggplot(mdl.glmnet, plotType = "scatter", highBestTune = TRUE)
gp.lvl.glmnet      <- ggplot(mdl.glmnet, plotType = "level")

# nParams == 3
gp.sct.nohi.svmPoly <- ggplot(mdl.svmPoly, plotType = "scatter")
gp.sct.yshi.svmPoly <- ggplot(mdl.svmPoly, plotType = "scatter", highBestTune = TRUE)
gp.lvl.svmPoly      <- ggplot(mdl.svmPoly, plotType = "level")

# nParams == 4
gp.sct.nohi.gbm <- ggplot(mdl.gbm, plotType = "scatter")
gp.sct.yshi.gbm <- ggplot(mdl.gbm, plotType = "scatter", highBestTune = TRUE)
gp.lvl.gbm      <- ggplot(mdl.gbm, plotType = "level")

# nParams == 5
#ggplot(mdl.dnn, plotType = "scatter")
#ggplot(mdl.dnn, plotType = "level")

#png("scatter_nohighlight.png", width = 480 * 2, height = 480)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(gp.sct.nohi.rpart,   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(gp.sct.nohi.glmnet,  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(gp.sct.nohi.svmPoly, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(gp.sct.nohi.gbm,     vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
#dev.off()

#png("scatter_yshighlight.png", width = 480 * 2, height = 480)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(gp.sct.yshi.rpart,   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(gp.sct.yshi.glmnet,  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(gp.sct.yshi.svmPoly, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(gp.sct.yshi.gbm,     vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

print(gp.sct.yshi.svmPoly + scale_x_log10())
#dev.off()

#png("level.png", width = 480 * 2, height = 480)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
#print(gp.lvl.rpart,   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(gp.lvl.glmnet,  vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(gp.lvl.svmPoly, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(gp.lvl.gbm,     vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
#dev.off()

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
