### R code from vignette source 'caret.Rnw'

###################################################
### code chunk number 1: loadLibs
###################################################
library(MASS)
library(caret)
library(mlbench)
data(Sonar)
library(pROC)
library(pls)
options(useFancyQuotes = FALSE) 
getInfo <- function(what = "Suggests")
{
  text <- packageDescription("caret")[what][[1]]
  text <- gsub("\n", ", ", text, fixed = TRUE)
  text <- gsub(">=", "$\\\\ge$", text, fixed = TRUE)
  eachPkg <- strsplit(text, ", ", fixed = TRUE)[[1]]
  eachPkg <- gsub(",", "", eachPkg, fixed = TRUE)
  #out <- paste("\\\\pkg{", eachPkg[order(tolower(eachPkg))], "}", sep = "")
  #paste(out, collapse = ", ")
  length(eachPkg)
}


###################################################
### code chunk number 2: install (eval = FALSE)
###################################################
## install.packages("caret", dependencies = c("Depends", "Suggests"))


###################################################
### code chunk number 3: SonarSplit
###################################################
library(caret)
library(mlbench)
data(Sonar)

set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, 
                               ## the outcome data are needed
                               p = .75, 
                               ## The percentage of data in the 
                               ## training set
                               list = FALSE)
                               ## The format of the results

## The output is a set of integers for the rows of Sonar 
## that belong in the training set.
str(inTrain)


###################################################
### code chunk number 4: SonarDatasets
###################################################
training <- Sonar[ inTrain,]
testing  <- Sonar[-inTrain,]

nrow(training)
nrow(testing)


###################################################
### code chunk number 5: plsTune1 (eval = FALSE)
###################################################
## plsFit <- train(Class ~ ., 
##                 data = training,
##                 method = "pls",
##                 ## Center and scale the predictors for the training 
##                 ## set and all future samples.
##                 preProc = c("center", "scale"))


###################################################
### code chunk number 6: plsFit
###################################################
ctrl <- trainControl(method = "repeatedcv",
                    repeats = 3,
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)
                    
set.seed(123)                    
plsFit <- train(Class ~ ., 
                data = training,
                method = "pls",
                tuneLength = 15,
                trControl = ctrl,
                metric = "ROC",
                preProc = c("center", "scale"))


###################################################
### code chunk number 7: plsPrint
###################################################
plsFit


###################################################
### code chunk number 8: baPlot
###################################################
trellis.par.set(caretTheme())
print(plot(plsFit))


###################################################
### code chunk number 9: plsPred
###################################################
plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)
plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)


###################################################
### code chunk number 10: plsCM
###################################################
confusionMatrix(data = plsClasses, testing$Class)


###################################################
### code chunk number 11: rdaFit
###################################################
## To illustrate, a custom grid is used
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)                    
rdaFit <- train(Class ~ ., 
                data = training,
                method = "rda",
                tuneGrid = rdaGrid,
                trControl = ctrl,
                metric = "ROC")
rdaFit
rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)


###################################################
### code chunk number 12: rs
###################################################
resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)


###################################################
### code chunk number 13: diffs
###################################################
diffs <- diff(resamps)
summary(diffs)


###################################################
### code chunk number 14: plsPlot
###################################################

plotTheme <- caretTheme()
plotTheme$plot.symbol$col <- rgb(.2, .2, .2, .5)
plotTheme$plot.symbol$pch <- 16
trellis.par.set(plotTheme)
print(xyplot(resamps, what = "BlandAltman"))


