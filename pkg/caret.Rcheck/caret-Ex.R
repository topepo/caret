pkgname <- "caret"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('caret')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BoxCoxTrans")
### * BoxCoxTrans

flush(stderr()); flush(stdout())

### Name: BoxCoxTrans.default
### Title: Box-Cox and Exponential Transformations
### Aliases: BoxCoxTrans.default BoxCoxTrans predict.BoxCoxTrans
###   expoTrans.default expoTrans predict.expoTrans
### Keywords: utilities

### ** Examples

data(BloodBrain)

ratio <- exp(logBBB)
bc <- BoxCoxTrans(ratio)
bc

predict(bc, ratio[1:5])

ratio[5] <- NA
bc2 <- BoxCoxTrans(ratio, bbbDescr$tpsa, na.rm = TRUE)
bc2

manly <- expoTrans(ratio)
manly





cleanEx()
nameEx("as.table.confusionMatrix")
### * as.table.confusionMatrix

flush(stderr()); flush(stdout())

### Name: as.table.confusionMatrix
### Title: Save Confusion Table Results
### Aliases: as.table.confusionMatrix as.matrix.confusionMatrix
### Keywords: utilities

### ** Examples

###################
## 2 class example

lvs <- c("normal", "abnormal")
truth <- factor(rep(lvs, times = c(86, 258)),
                levels = rev(lvs))
pred <- factor(
               c(
                 rep(lvs, times = c(54, 32)),
                 rep(lvs, times = c(27, 231))),               
               levels = rev(lvs))

xtab <- table(pred, truth)

results <- confusionMatrix(xtab)
as.table(results)
as.matrix(results)
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")
  
###################
## 3 class example

xtab <- confusionMatrix(iris$Species, sample(iris$Species))
as.matrix(xtab)



cleanEx()
nameEx("avNNet")
### * avNNet

flush(stderr()); flush(stdout())

### Name: avNNet.default
### Title: Neural Networks Using Model Averaging
### Aliases: avNNet.default predict.avNNet avNNet.formula avNNet
### Keywords: neural

### ** Examples

data(BloodBrain)
## Not run: 
##D modelFit <- avNNet(bbbDescr, logBBB, size = 5, linout = TRUE, trace = FALSE)
##D modelFit
##D 
##D predict(modelFit, bbbDescr)
## End(Not run)



cleanEx()
nameEx("bag")
### * bag

flush(stderr()); flush(stdout())

### Name: bag.default
### Title: A General Framework For Bagging
### Aliases: bag.default bag bagControl predict.bag ldaBag plsBag nbBag
###   ctreeBag svmBag nnetBag
### Keywords: models

### ** Examples

## A simple example of bagging conditional inference regression trees:
data(BloodBrain)

## treebag <- bag(bbbDescr, logBBB, B = 10,
##                bagControl = bagControl(fit = ctreeBag$fit,
##                                        predict = ctreeBag$pred,
##                                        aggregate = ctreeBag$aggregate))




## An example of pooling posterior probabilities to generate class predictions
data(mdrr)

## remove some zero variance predictors and linear dependencies
mdrrDescr <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .95)]

## basicLDA <- train(mdrrDescr, mdrrClass, "lda")

## bagLDA2 <- train(mdrrDescr, mdrrClass, 
##                  "bag", 
##                  B = 10, 
##                  bagControl = bagControl(fit = ldaBag$fit,
##                                          predict = ldaBag$pred,
##                                          aggregate = ldaBag$aggregate),
##                  tuneGrid = data.frame(vars = c((1:10)*10 , ncol(mdrrDescr))))



cleanEx()
nameEx("bagEarth")
### * bagEarth

flush(stderr()); flush(stdout())

### Name: bagEarth
### Title: Bagged Earth
### Aliases: bagEarth print.bagEarth bagEarth.default bagEarth.formula
### Keywords: regression

### ** Examples

## Not run: 
##D library(mda)
##D library(earth)
##D data(trees)
##D fit1 <- earth(trees[,-3], trees[,3])
##D fit2 <- bagEarth(trees[,-3], trees[,3], B = 10)
## End(Not run)



cleanEx()
nameEx("bagFDA")
### * bagFDA

flush(stderr()); flush(stdout())

### Name: bagFDA
### Title: Bagged FDA
### Aliases: bagFDA print.bagFDA bagFDA.default bagFDA.formula
### Keywords: regression

### ** Examples

library(mlbench)
library(earth)
data(Glass)

set.seed(36)
inTrain <- sample(1:dim(Glass)[1], 150)

trainData <- Glass[ inTrain, ]
testData  <- Glass[-inTrain, ]


baggedFit <- bagFDA(Type ~ ., trainData)
confusionMatrix(predict(baggedFit, testData[, -10]),
                testData[, 10])




cleanEx()
nameEx("calibration")
### * calibration

flush(stderr()); flush(stdout())

### Name: calibration
### Title: Probability Calibration Plot
### Aliases: calibration calibration.formula calibration.default
###   xyplot.calibration panel.calibration
### Keywords: hplot

### ** Examples

## Not run: 
##D data(mdrr)
##D mdrrDescr <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
##D mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .5)]
##D 
##D 
##D inTrain <- createDataPartition(mdrrClass)
##D trainX <- mdrrDescr[inTrain[[1]], ]
##D trainY <- mdrrClass[inTrain[[1]]]
##D testX <- mdrrDescr[-inTrain[[1]], ]
##D testY <- mdrrClass[-inTrain[[1]]]
##D 
##D library(MASS)
##D 
##D ldaFit <- lda(trainX, trainY)
##D qdaFit <- qda(trainX, trainY)
##D 
##D testProbs <- data.frame(obs = testY,
##D                         lda = predict(ldaFit, testX)$posterior[,1],
##D                         qda = predict(qdaFit, testX)$posterior[,1])
##D 
##D calibration(obs ~ lda + qda, data = testProbs)
##D 
##D calPlotData <- calibration(obs ~ lda + qda, data = testProbs)
##D calPlotData
##D 
##D xyplot(calPlotData, auto.key = list(columns = 2))
## End(Not run)



cleanEx()
nameEx("classDist")
### * classDist

flush(stderr()); flush(stdout())

### Name: classDist
### Title: Compute and predict the distances to class centroids
### Aliases: classDist.default classDist predict.classDist
### Keywords: manip

### ** Examples

trainSet <- sample(1:150, 100)

distData <- classDist(iris[trainSet, 1:4], 
                      iris$Species[trainSet])

newDist <- predict(distData,
                   iris[-trainSet, 1:4])

splom(newDist, groups = iris$Species[-trainSet])



cleanEx()
nameEx("confusionMatrix")
### * confusionMatrix

flush(stderr()); flush(stdout())

### Name: confusionMatrix
### Title: Create a confusion matrix
### Aliases: confusionMatrix.table confusionMatrix.default confusionMatrix
### Keywords: utilities

### ** Examples

###################
## 2 class example

lvs <- c("normal", "abnormal")
truth <- factor(rep(lvs, times = c(86, 258)),
                levels = rev(lvs))
pred <- factor(
               c(
                 rep(lvs, times = c(54, 32)),
                 rep(lvs, times = c(27, 231))),               
               levels = rev(lvs))

xtab <- table(pred, truth)

confusionMatrix(xtab)
confusionMatrix(pred, truth)
confusionMatrix(xtab, prevalence = 0.25)   

###################
## 3 class example

confusionMatrix(iris$Species, sample(iris$Species))

newPrior <- c(.05, .8, .15)
names(newPrior) <- levels(iris$Species)

confusionMatrix(iris$Species, sample(iris$Species))




cleanEx()
nameEx("confusionMatrix.train")
### * confusionMatrix.train

flush(stderr()); flush(stdout())

### Name: confusionMatrix.train
### Title: Estimate a Resampled Confusion Matrix
### Aliases: confusionMatrix.train confusionMatrix.rfe confusionMatrix.sbf
### Keywords: utilities

### ** Examples


data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit <- train(TrainData, TrainClasses,
                method = "knn",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
confusionMatrix(knnFit)
confusionMatrix(knnFit, "average")
confusionMatrix(knnFit, "none")




cleanEx()
nameEx("createDataPartition")
### * createDataPartition

flush(stderr()); flush(stdout())

### Name: createDataPartition
### Title: Data Splitting functions
### Aliases: createDataPartition createResample createFolds
###   createMultiFolds createTimeSlices
### Keywords: utilities

### ** Examples

data(oil)
createDataPartition(oilType, 2)

x <- rgamma(50, 3, .5)
inA <- createDataPartition(x, list = FALSE)

plot(density(x[inA]))
rug(x[inA])

points(density(x[-inA]), type = "l", col = 4)
rug(x[-inA], col = 4)

createResample(oilType, 2)

createFolds(oilType, 10)
createFolds(oilType, 5, FALSE)

createFolds(rnorm(21))

createTimeSlices(1:9, 5, 1, fixedWindow = FALSE)
createTimeSlices(1:9, 5, 1, fixedWindow = TRUE)
createTimeSlices(1:9, 5, 3, fixedWindow = TRUE)
createTimeSlices(1:9, 5, 3, fixedWindow = FALSE)

createTimeSlices(1:15, 5, 3)
createTimeSlices(1:15, 5, 3, skip = 2)
createTimeSlices(1:15, 5, 3, skip = 3)



cleanEx()
nameEx("diff.resamples")
### * diff.resamples

flush(stderr()); flush(stdout())

### Name: diff.resamples
### Title: Inferential Assessments About Model Performance
### Aliases: diff.resamples summary.diff.resamples compare_models
### Keywords: models

### ** Examples

## Not run: 
##D #load(url("http://topepo.github.io/caret/exampleModels.RData"))
##D 
##D resamps <- resamples(list(CART = rpartFit,
##D                           CondInfTree = ctreeFit,
##D                           MARS = earthFit))
##D 
##D difs <- diff(resamps)
##D 
##D difs
##D 
##D summary(difs)
##D 
##D compare_models(rpartFit, ctreeFit)
## End(Not run)



cleanEx()
nameEx("dotplot.varImp.train")
### * dotplot.varImp.train

flush(stderr()); flush(stdout())

### Name: dotPlot
### Title: Create a dotplot of variable importance values
### Aliases: dotPlot
### Keywords: hplot

### ** Examples


data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit <- train(TrainData, TrainClasses, "knn")

knnImp <- varImp(knnFit)

dotPlot(knnImp)




cleanEx()
nameEx("downSample")
### * downSample

flush(stderr()); flush(stdout())

### Name: downSample
### Title: Down- and Up-Sampling Imbalanced Data
### Aliases: downSample upSample
### Keywords: utilities

### ** Examples

## A ridiculous example...
data(oil)
table(oilType)
downSample(fattyAcids, oilType)

upSample(fattyAcids, oilType)




cleanEx()
nameEx("dummyVars")
### * dummyVars

flush(stderr()); flush(stdout())

### Name: dummyVars
### Title: Create A Full Set of Dummy Variables
### Aliases: dummyVars dummyVars.default predict.dummyVars contr.dummy
###   contr.ltfr class2ind
### Keywords: models

### ** Examples


when <- data.frame(time = c("afternoon", "night", "afternoon",
                            "morning", "morning", "morning",
                            "morning", "afternoon", "afternoon"),
                   day = c("Mon", "Mon", "Mon",
                           "Wed", "Wed", "Fri",
                           "Sat", "Sat", "Fri"))

levels(when$time) <- list(morning="morning",
                          afternoon="afternoon",
                          night="night")
levels(when$day) <- list(Mon="Mon", Tue="Tue", Wed="Wed", Thu="Thu",
                         Fri="Fri", Sat="Sat", Sun="Sun")

## Default behavior:
model.matrix(~day, when)

mainEffects <- dummyVars(~ day + time, data = when)
mainEffects
predict(mainEffects, when[1:3,])

when2 <- when
when2[1, 1] <- NA
predict(mainEffects, when2[1:3,])
predict(mainEffects, when2[1:3,], na.action = na.omit)


interactionModel <- dummyVars(~ day + time + day:time,
                              data = when,
                              sep = ".")
predict(interactionModel, when[1:3,])

noNames <- dummyVars(~ day + time + day:time,
                     data = when,
                     levelsOnly = TRUE)
predict(noNames, when)



cleanEx()
nameEx("extractPrediction")
### * extractPrediction

flush(stderr()); flush(stdout())

### Name: predict.train
### Title: Extract predictions and class probabilities from train objects
### Aliases: predict.list predict.train extractPrediction extractProb
### Keywords: manip

### ** Examples

   ## Not run: 
##D 
##D knnFit <- train(Species ~ ., data = iris, method = "knn", 
##D                 trControl = trainControl(method = "cv"))
##D 
##D rdaFit <- train(Species ~ ., data = iris, method = "rda", 
##D                 trControl = trainControl(method = "cv"))
##D 
##D predict(knnFit)
##D predict(knnFit, type = "prob")
##D 
##D bothModels <- list(knn = knnFit,
##D                    tree = rdaFit)
##D 
##D predict(bothModels)
##D 
##D extractPrediction(bothModels, testX = iris[1:10, -5])
##D extractProb(bothModels, testX = iris[1:10, -5])
##D   
## End(Not run)



cleanEx()
nameEx("featurePlot")
### * featurePlot

flush(stderr()); flush(stdout())

### Name: featurePlot
### Title: Wrapper for Lattice Plotting of Predictor Variables
### Aliases: featurePlot
### Keywords: hplot

### ** Examples

x <- matrix(rnorm(50*5),ncol=5)
y <- factor(rep(c("A", "B"),  25))

trellis.par.set(theme = col.whitebg(), warn = FALSE)
featurePlot(x, y, "ellipse")
featurePlot(x, y, "strip", jitter = TRUE)
featurePlot(x, y, "box")
featurePlot(x, y, "pairs")



cleanEx()
nameEx("filterVarImp")
### * filterVarImp

flush(stderr()); flush(stdout())

### Name: filterVarImp
### Title: Calculation of filter-based variable importance
### Aliases: filterVarImp
### Keywords: models

### ** Examples

data(mdrr)
filterVarImp(mdrrDescr[, 1:5], mdrrClass)

data(BloodBrain)

filterVarImp(bbbDescr[, 1:5], logBBB, nonpara = FALSE)
apply(bbbDescr[, 1:5],
      2,
      function(x, y) summary(lm(y~x))$coefficients[2,3],
      y = logBBB)

filterVarImp(bbbDescr[, 1:5], logBBB, nonpara = TRUE)



cleanEx()
nameEx("findCorrelation")
### * findCorrelation

flush(stderr()); flush(stdout())

### Name: findCorrelation
### Title: Determine highly correlated variables
### Aliases: findCorrelation
### Keywords: manip

### ** Examples

R1 <- structure(c(1, 0.86, 0.56, 0.32, 0.85, 0.86, 1, 0.01, 0.74, 0.32, 
                  0.56, 0.01, 1, 0.65, 0.91, 0.32, 0.74, 0.65, 1, 0.36,
                  0.85, 0.32, 0.91, 0.36, 1), 
                .Dim = c(5L, 5L))
colnames(R1) <- rownames(R1) <- paste0("x", 1:ncol(R1))
R1

findCorrelation(R1, cutoff = .6, exact = FALSE)
findCorrelation(R1, cutoff = .6, exact = TRUE)
findCorrelation(R1, cutoff = .6, exact = TRUE, names = FALSE)


R2 <- diag(rep(1, 5))
R2[2, 3] <- R2[3, 2] <- .7
R2[5, 3] <- R2[3, 5] <- -.7
R2[4, 1] <- R2[1, 4] <- -.67

corrDF <- expand.grid(row = 1:5, col = 1:5)
corrDF$correlation <- as.vector(R2)
levelplot(correlation ~ row + col, corrDF)

findCorrelation(R2, cutoff = .65, verbose = TRUE)

findCorrelation(R2, cutoff = .99, verbose = TRUE)



cleanEx()
nameEx("findLinearCombos")
### * findLinearCombos

flush(stderr()); flush(stdout())

### Name: findLinearCombos
### Title: Determine linear combinations in a matrix
### Aliases: findLinearCombos
### Keywords: manip

### ** Examples

testData1 <- matrix(0, nrow=20, ncol=8)
testData1[,1] <- 1
testData1[,2] <- round(rnorm(20), 1)
testData1[,3] <- round(rnorm(20), 1)
testData1[,4] <- round(rnorm(20), 1)
testData1[,5] <- 0.5 * testData1[,2] - 0.25 * testData1[,3] - 0.25 * testData1[,4]
testData1[1:4,6] <- 1
testData1[5:10,7] <- 1
testData1[11:20,8] <- 1

findLinearCombos(testData1)

testData2 <- matrix(0, nrow=6, ncol=6)
testData2[,1] <- c(1, 1, 1, 1, 1, 1)
testData2[,2] <- c(1, 1, 1, 0, 0, 0)
testData2[,3] <- c(0, 0, 0, 1, 1, 1)
testData2[,4] <- c(1, 0, 0, 1, 0, 0)
testData2[,5] <- c(0, 1, 0, 0, 1, 0)
testData2[,6] <- c(0, 0, 1, 0, 0, 1)

findLinearCombos(testData2)



cleanEx()
nameEx("format.bagEarth")
### * format.bagEarth

flush(stderr()); flush(stdout())

### Name: format.bagEarth
### Title: Format 'bagEarth' objects
### Aliases: format.bagEarth
### Keywords: models

### ** Examples

a <- bagEarth(Volume ~ ., data = trees, B= 3)
format(a)

# yields:
# (
#   31.61075 
#   +  6.587273 * pmax(0,  Girth -   14.2) 
#   -  3.229363 * pmax(0,   14.2 -  Girth) 
#   - 0.3167140 * pmax(0,     79 - Height) 
#   +
#    22.80225 
#   +  5.309866 * pmax(0,  Girth -     12) 
#   -  2.378658 * pmax(0,     12 -  Girth) 
#   +  0.793045 * pmax(0, Height -     80) 
#   - 0.3411915 * pmax(0,     80 - Height) 
#   +
#    31.39772 
#   +   6.18193 * pmax(0,  Girth -   14.2) 
#   -  3.660456 * pmax(0,   14.2 -  Girth) 
#   + 0.6489774 * pmax(0, Height -     80) 
# )/3




cleanEx()
nameEx("ga_functions")
### * ga_functions

flush(stderr()); flush(stdout())

### Name: gafs_initial
### Title: Ancillary genetic algorithm functions
### Aliases: gafs_initial gafs_lrSelection gafs_rwSelection
###   gafs_tourSelection gafs_uCrossover gafs_spCrossover gafs_raMutation
###   caretGA rfGA treebagGA

### ** Examples

pop <- gafs_initial(vars = 10, popSize = 10)
pop

gafs_lrSelection(population = pop, fitness = 1:10)

gafs_spCrossover(population = pop, fitness = 1:10, parents = 1:2)


## Not run: 
##D ## Hypothetical examples
##D lda_ga <- gafs(x = predictors,
##D                y = classes,
##D                gafsControl = gafsControl(functions = caretGA),
##D                ## now pass arguments to `train`
##D                method = "lda",
##D                metric = "Accuracy"
##D                trControl = trainControl(method = "cv", classProbs = TRUE))
##D 
##D rf_ga <- gafs(x = predictors,
##D               y = classes,
##D               gafsControl = gafsControl(functions = rfGA),
##D               ## these are arguments to `randomForest`
##D               ntree = 1000,
##D               importance = TRUE)
##D 	
## End(Not run)




cleanEx()
nameEx("gafs.default")
### * gafs.default

flush(stderr()); flush(stdout())

### Name: gafs.default
### Title: Genetic algorithm feature selection
### Aliases: gafs.default gafs
### Keywords: models

### ** Examples

## Not run: 
##D set.seed(1)
##D train_data <- twoClassSim(100, noiseVars = 10)
##D test_data  <- twoClassSim(10,  noiseVars = 10)
##D 
##D ## A short example 
##D ctrl <- gafsControl(functions = rfGA, 
##D                     method = "cv",
##D                     number = 3)
##D 
##D rf_search <- gafs(x = train_data[, -ncol(train_data)],
##D                   y = train_data$Class,
##D                   iters = 3,
##D                   gafsControl = ctrl)
##D 
##D rf_search  
##D   
## End(Not run)



cleanEx()
nameEx("icr")
### * icr

flush(stderr()); flush(stdout())

### Name: icr.formula
### Title: Independent Component Regression
### Aliases: icr.formula icr.default icr predict.icr
### Keywords: multivariate

### ** Examples

data(BloodBrain)

icrFit <- icr(bbbDescr, logBBB, n.comp = 5)

icrFit

predict(icrFit, bbbDescr[1:5,])






cleanEx()
nameEx("index2vec")
### * index2vec

flush(stderr()); flush(stdout())

### Name: index2vec
### Title: Convert indicies to a binary vector
### Aliases: index2vec

### ** Examples

index2vec(x = 1:2, vars = 5)
index2vec(x = 1:2, vars = 5, sign = TRUE)



cleanEx()
nameEx("knn3")
### * knn3

flush(stderr()); flush(stdout())

### Name: knn3
### Title: k-Nearest Neighbour Classification
### Aliases: knn3 knn3.formula knn3.matrix knn3.data.frame knn3Train
### Keywords: multivariate

### ** Examples

irisFit1 <- knn3(Species ~ ., iris)

irisFit2 <- knn3(as.matrix(iris[, -5]), iris[,5])

data(iris3)
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
knn3Train(train, test, cl, k = 5, prob = TRUE) 



cleanEx()
nameEx("knnreg")
### * knnreg

flush(stderr()); flush(stdout())

### Name: knnreg
### Title: k-Nearest Neighbour Regression
### Aliases: knnreg knnregTrain knnreg.formula knnreg.default knnreg.matrix
###   knnreg.data.frame knnreg
### Keywords: multivariate

### ** Examples

data(BloodBrain)

inTrain <- createDataPartition(logBBB, p = .8)[[1]]

trainX <- bbbDescr[inTrain,]
trainY <- logBBB[inTrain]

testX <- bbbDescr[-inTrain,]
testY <- logBBB[-inTrain]

fit <- knnreg(trainX, trainY, k = 3)

plot(testY, predict(fit, testX))   



cleanEx()
nameEx("lattice")
### * lattice

flush(stderr()); flush(stdout())

### Name: histogram.train
### Title: Lattice functions for plotting resampling results
### Aliases: stripplot.train xyplot.train densityplot.train histogram.train
### Keywords: hplot

### ** Examples

## Not run: 
##D 
##D library(mlbench)
##D data(BostonHousing)
##D 
##D library(rpart)
##D rpartFit <- train(medv ~ .,
##D                   data = BostonHousing,
##D                   "rpart", 
##D                   tuneLength = 9,
##D                   trControl = trainControl(
##D                     method = "boot", 
##D                     returnResamp = "all"))
##D 
##D densityplot(rpartFit,
##D             adjust = 1.25)
##D 
##D xyplot(rpartFit,
##D        metric = "Rsquared",
##D        type = c("p", "a"))
##D 
##D stripplot(rpartFit,
##D           horizontal = FALSE,
##D           jitter = TRUE)
##D 
## End(Not run)



cleanEx()
nameEx("lattice.diff.resamples")
### * lattice.diff.resamples

flush(stderr()); flush(stdout())

### Name: dotplot.diff.resamples
### Title: Lattice Functions for Visualizing Resampling Differences
### Aliases: levelplot.diff.resamples densityplot.diff.resamples
###   bwplot.diff.resamples dotplot.diff.resamples
### Keywords: hplot

### ** Examples

## Not run: 
##D #load(url("http://topepo.github.io/caret/exampleModels.RData"))
##D 
##D resamps <- resamples(list(CART = rpartFit,
##D                           CondInfTree = ctreeFit,
##D                           MARS = earthFit))
##D difs <- diff(resamps)
##D 
##D dotplot(difs)
##D 
##D densityplot(difs,
##D             metric = "RMSE",
##D             auto.key = TRUE,
##D             pch = "|")
##D 
##D bwplot(difs,
##D        metric = "RMSE")
##D 
##D levelplot(difs, what = "differences")
##D 
## End(Not run)



cleanEx()
nameEx("lattice.resamples")
### * lattice.resamples

flush(stderr()); flush(stdout())

### Name: xyplot.resamples
### Title: Lattice Functions for Visualizing Resampling Results
### Aliases: xyplot.resamples densityplot.resamples bwplot.resamples
###   splom.resamples parallelplot.resamples dotplot.resamples
### Keywords: hplot

### ** Examples

## Not run: 
##D #load(url("http://topepo.github.io/caret/exampleModels.RData"))
##D 
##D resamps <- resamples(list(CART = rpartFit,
##D                           CondInfTree = ctreeFit,
##D                           MARS = earthFit))
##D 
##D dotplot(resamps, 
##D         scales =list(x = list(relation = "free")), 
##D         between = list(x = 2))
##D 
##D bwplot(resamps,
##D        metric = "RMSE")
##D 
##D densityplot(resamps,
##D             auto.key = list(columns = 3),
##D             pch = "|")
##D 
##D xyplot(resamps,
##D        models = c("CART", "MARS"),
##D        metric = "RMSE")
##D 
##D splom(resamps, metric = "RMSE")
##D splom(resamps, variables = "metrics")
##D 
##D parallelplot(resamps, metric = "RMSE")
##D 
##D 
## End(Not run)



cleanEx()
nameEx("lattice.rfe")
### * lattice.rfe

flush(stderr()); flush(stdout())

### Name: lattice.rfe
### Title: Lattice functions for plotting resampling results of recursive
###   feature selection
### Aliases: xyplot.rfe stripplot.rfe densityplot.rfe histogram.rfe
### Keywords: hplot

### ** Examples

## Not run: 
##D library(mlbench)
##D n <- 100
##D p <- 40
##D sigma <- 1
##D set.seed(1)
##D sim <- mlbench.friedman1(n, sd = sigma)
##D x <- cbind(sim$x,  matrix(rnorm(n * p), nrow = n))
##D y <- sim$y
##D colnames(x) <- paste("var", 1:ncol(x), sep = "")
##D 
##D normalization <- preProcess(x)
##D x <- predict(normalization, x)
##D x <- as.data.frame(x)
##D subsets <- c(10, 15, 20, 25)
##D 
##D ctrl <- rfeControl(
##D                    functions = lmFuncs,
##D                    method = "cv",
##D                    verbose = FALSE,
##D                    returnResamp = "all")
##D 
##D lmProfile <- rfe(x, y,
##D                  sizes = subsets,
##D                  rfeControl = ctrl)
##D xyplot(lmProfile)
##D stripplot(lmProfile)
##D 
##D histogram(lmProfile)
##D densityplot(lmProfile)
## End(Not run)



cleanEx()
nameEx("learning_curve")
### * learning_curve

flush(stderr()); flush(stdout())

### Name: learing_curve_dat
### Title: Create Data to Plot a Learning Curve
### Aliases: learing_curve_dat
### Keywords: models

### ** Examples

## Not run: 
##D set.seed(1412)
##D class_dat <- twoClassSim(1000)
##D 
##D set.seed(29510)
##D lda_data <- learing_curve_dat(dat = class_dat, 
##D                               outcome = "Class",
##D                               test_prop = 1/4, 
##D                               ## `train` arguments:
##D                               method = "lda", 
##D                               metric = "ROC",
##D                               trControl = trainControl(classProbs = TRUE, 
##D                                                        summaryFunction = twoClassSummary))
##D 
##D 
##D 
##D ggplot(lda_data, aes(x = Training_Size, y = ROC, color = Data)) + 
##D   geom_smooth(method = loess, span = .8) + 
##D   theme_bw()
##D  
## End(Not run)



cleanEx()
nameEx("lift")
### * lift

flush(stderr()); flush(stdout())

### Name: lift
### Title: Lift Plot
### Aliases: lift lift.formula lift.default xyplot.lift
### Keywords: hplot

### ** Examples

set.seed(1)
simulated <- data.frame(obs = factor(rep(letters[1:2], each = 100)),
                        perfect = sort(runif(200), decreasing = TRUE),
                        random = runif(200))

lift1 <- lift(obs ~ random, data = simulated)
lift1
xyplot(lift1)

lift2 <- lift(obs ~ random + perfect, data = simulated)
lift2
xyplot(lift2, auto.key = list(columns = 2))

xyplot(lift2, auto.key = list(columns = 2), value = c(10, 30))

xyplot(lift2, plot = "lift", auto.key = list(columns = 2))



cleanEx()
nameEx("maxDissim")
### * maxDissim

flush(stderr()); flush(stdout())

### Name: maxDissim
### Title: Maximum Dissimilarity Sampling
### Aliases: maxDissim minDiss sumDiss
### Keywords: utilities

### ** Examples


example <- function(pct = 1, obj = minDiss, ...)
{
  tmp <- matrix(rnorm(200 * 2), nrow = 200)

  ## start with 15 data points
  start <- sample(1:dim(tmp)[1], 15)
  base <- tmp[start,]
  pool <- tmp[-start,]
  
  ## select 9 for addition
  newSamp <- maxDissim(
                       base, pool, 
                       n = 9, 
                       randomFrac = pct, obj = obj, ...)
  
  allSamp <- c(start, newSamp)
  
  plot(
       tmp[-newSamp,], 
       xlim = extendrange(tmp[,1]), ylim = extendrange(tmp[,2]), 
       col = "darkgrey", 
       xlab = "variable 1", ylab = "variable 2")
  points(base, pch = 16, cex = .7)
  
  for(i in seq(along = newSamp))
    points(
           pool[newSamp[i],1], 
           pool[newSamp[i],2], 
           pch = paste(i), col = "darkred") 
}

par(mfrow=c(2,2))

set.seed(414)
example(1, minDiss)
title("No Random Sampling, Min Score")

set.seed(414)
example(.1, minDiss)
title("10 Pct Random Sampling, Min Score")

set.seed(414)
example(1, sumDiss)
title("No Random Sampling, Sum Score")

set.seed(414)
example(.1, sumDiss)
title("10 Pct Random Sampling, Sum Score")



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("modelLookup")
### * modelLookup

flush(stderr()); flush(stdout())

### Name: modelLookup
### Title: Tools for Models Available in 'train'
### Aliases: modelLookup getModelInfo checkInstall
### Keywords: utilities

### ** Examples

modelLookup()
modelLookup("gbm")

getModelInfo("pls")
getModelInfo("^pls")
getModelInfo("pls", regex = FALSE)

## Not run: 
##D checkInstall(getModelInfo("pls")$library)
## End(Not run)



cleanEx()
nameEx("nearZeroVar")
### * nearZeroVar

flush(stderr()); flush(stdout())

### Name: nearZeroVar
### Title: Identification of near zero variance predictors
### Aliases: nearZeroVar nzv checkResamples checkConditionalX
### Keywords: utilities

### ** Examples

nearZeroVar(iris[, -5], saveMetrics = TRUE)

data(BloodBrain)
nearZeroVar(bbbDescr)


set.seed(1)
classes <- factor(rep(letters[1:3], each = 30))
x <- data.frame(x1 = rep(c(0, 1), 45),
                x2 = c(rep(0, 10), rep(1, 80)))

lapply(x, table, y = classes)
checkConditionalX(x, classes)

folds <- createFolds(classes, k = 3, returnTrain = TRUE)
x$x3 <- x$x1
x$x3[folds[[1]]] <- 0

checkResamples(folds, x, classes)





cleanEx()
nameEx("nullModel")
### * nullModel

flush(stderr()); flush(stdout())

### Name: nullModel
### Title: Fit a simple, non-informative model
### Aliases: nullModel nullModel.default predict.nullModel
### Keywords: models

### ** Examples

outcome <- factor(sample(letters[1:2], 
                         size = 100, 
                         prob = c(.1, .9), 
                         replace = TRUE))
useless <- nullModel(y = outcome)
useless
predict(useless, matrix(NA, nrow = 10))




cleanEx()
nameEx("panel.lift")
### * panel.lift

flush(stderr()); flush(stdout())

### Name: panel.lift2
### Title: Lattice Panel Functions for Lift Plots
### Aliases: panel.lift panel.lift2
### Keywords: hplot

### ** Examples

set.seed(1)
simulated <- data.frame(obs = factor(rep(letters[1:2], each = 100)),
                        perfect = sort(runif(200), decreasing = TRUE),
                        random = runif(200))

regionInfo <- trellis.par.get("reference.line")
regionInfo$col <- "lightblue"
trellis.par.set("reference.line", regionInfo)

lift2 <- lift(obs ~ random + perfect, data = simulated)
lift2
xyplot(lift2, auto.key = list(columns = 2))

## use a different panel function
xyplot(lift2, panel = panel.lift)



cleanEx()
nameEx("pcaNNet")
### * pcaNNet

flush(stderr()); flush(stdout())

### Name: pcaNNet.default
### Title: Neural Networks with a Principal Component Step
### Aliases: pcaNNet.default predict.pcaNNet pcaNNet.formula pcaNNet
### Keywords: neural

### ** Examples

data(BloodBrain)
modelFit <- pcaNNet(bbbDescr[, 1:10], logBBB, size = 5, linout = TRUE, trace = FALSE)
modelFit

predict(modelFit, bbbDescr[, 1:10])



cleanEx()
nameEx("plot.gafs")
### * plot.gafs

flush(stderr()); flush(stdout())

### Name: plot.gafs
### Title: Plot Method for the gafs and safs Classes
### Aliases: plot.safs plot.gafs
### Keywords: hplot

### ** Examples

## Not run: 
##D set.seed(1)
##D train_data <- twoClassSim(100, noiseVars = 10)
##D test_data  <- twoClassSim(10,  noiseVars = 10)
##D 
##D ## A short example 
##D ctrl <- safsControl(functions = rfSA, 
##D                     method = "cv",
##D                     number = 3)
##D 
##D rf_search <- safs(x = train_data[, -ncol(train_data)],
##D                   y = train_data$Class,
##D                   iters = 50,
##D                   safsControl = ctrl)
##D 
##D plot(rf_search)
##D plot(rf_search, 
##D 	 output = "lattice", 
##D 	 auto.key = list(columns = 2))
##D 
##D plot_data <- plot(rf_search, output = "data")   
##D summary(plot_data)                 
##D     
## End(Not run)



cleanEx()
nameEx("plot.rfe")
### * plot.rfe

flush(stderr()); flush(stdout())

### Name: plot.rfe
### Title: Plot RFE Performance Profiles
### Aliases: plot.rfe ggplot.rfe
### Keywords: hplot

### ** Examples

## Not run: 
##D data(BloodBrain)
##D 
##D x <- scale(bbbDescr[,-nearZeroVar(bbbDescr)])
##D x <- x[, -findCorrelation(cor(x), .8)]
##D x <- as.data.frame(x)
##D 
##D set.seed(1)
##D lmProfile <- rfe(x, logBBB,
##D                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
##D                  rfeControl = rfeControl(functions = lmFuncs, 
##D                                          number = 200))
##D plot(lmProfile)
##D plot(lmProfile, metric = "Rsquared")
##D ggplot(lmProfile)
## End(Not run)



cleanEx()
nameEx("plot.train")
### * plot.train

flush(stderr()); flush(stdout())

### Name: plot.train
### Title: Plot Method for the train Class
### Aliases: plot.train ggplot.train
### Keywords: hplot

### ** Examples


## Not run: 
##D library(klaR)
##D rdaFit <- train(Species ~ .,
##D                 data = iris, 
##D                 method = "rda", 
##D                 control = trainControl(method = "cv"))
##D plot(rdaFit)
##D plot(rdaFit, plotType = "level")
##D 
##D ggplot(rdaFit) + theme_bw()
##D 
## End(Not run)
 


cleanEx()
nameEx("plotClassProbs")
### * plotClassProbs

flush(stderr()); flush(stdout())

### Name: plotClassProbs
### Title: Plot Predicted Probabilities in Classification Models
### Aliases: plotClassProbs
### Keywords: hplot

### ** Examples

## Not run: 
##D data(mdrr)
##D set.seed(90)
##D inTrain <- createDataPartition(mdrrClass, p = .5)[[1]]
##D 
##D trainData <- mdrrDescr[inTrain,1:20]
##D testData <- mdrrDescr[-inTrain,1:20]
##D 
##D trainY <- mdrrClass[inTrain]
##D testY <- mdrrClass[-inTrain]
##D 
##D ctrl <- trainControl(method = "cv")
##D 
##D nbFit1 <- train(trainData, trainY, "nb",
##D                 trControl = ctrl,
##D                 tuneGrid = data.frame(usekernel = TRUE, fL = 0))
##D 
##D nbFit2 <- train(trainData, trainY, "nb",
##D                 trControl = ctrl,
##D                 tuneGrid = data.frame(usekernel = FALSE, fL = 0))
##D 
##D 
##D models <- list(para = nbFit2, nonpara = nbFit1)
##D 
##D predProbs <- extractProb(models, testX = testData,  testY = testY)
##D 
##D plotClassProbs(predProbs, useObjects = TRUE)
##D plotClassProbs(predProbs,
##D                subset = object == "para" & dataType == "Test")
##D plotClassProbs(predProbs,
##D                useObjects = TRUE,
##D                plotType = "densityplot",
##D                auto.key = list(columns = 2))
## End(Not run)





cleanEx()
nameEx("plotObsVsPred")
### * plotObsVsPred

flush(stderr()); flush(stdout())

### Name: plotObsVsPred
### Title: Plot Observed versus Predicted Results in Regression and
###   Classification Models
### Aliases: plotObsVsPred
### Keywords: hplot

### ** Examples

## Not run: 
##D # regression example
##D data(BostonHousing)
##D rpartFit <- train(BostonHousing[1:100, -c(4, 14)], 
##D                   BostonHousing$medv[1:100], 
##D                   "rpart", tuneLength = 9)
##D plsFit <- train(BostonHousing[1:100, -c(4, 14)], 
##D                 BostonHousing$medv[1:100], 
##D                 "pls")
##D 
##D predVals <- extractPrediction(list(rpartFit, plsFit), 
##D                               testX = BostonHousing[101:200, -c(4, 14)], 
##D                               testY = BostonHousing$medv[101:200], 
##D                               unkX = BostonHousing[201:300, -c(4, 14)])
##D 
##D plotObsVsPred(predVals)
##D 
##D 
##D #classification example
##D data(Satellite)
##D numSamples <- dim(Satellite)[1]
##D set.seed(716)
##D 
##D varIndex <- 1:numSamples
##D 
##D trainSamples <- sample(varIndex, 150)
##D 
##D varIndex <- (1:numSamples)[-trainSamples]
##D testSamples <- sample(varIndex, 100)
##D 
##D varIndex <- (1:numSamples)[-c(testSamples, trainSamples)]
##D unkSamples <- sample(varIndex, 50)
##D 
##D trainX <- Satellite[trainSamples, -37]
##D trainY <- Satellite[trainSamples, 37]
##D 
##D testX <- Satellite[testSamples, -37]
##D testY <- Satellite[testSamples, 37]
##D 
##D unkX <- Satellite[unkSamples, -37]
##D 
##D knnFit  <- train(trainX, trainY, "knn")
##D rpartFit <- train(trainX, trainY, "rpart")
##D 
##D predTargets <- extractPrediction(list(knnFit, rpartFit), 
##D                                  testX = testX, 
##D                                  testY = testY, 
##D                                  unkX = unkX)
##D 
##D plotObsVsPred(predTargets)
## End(Not run)



cleanEx()
nameEx("plsda")
### * plsda

flush(stderr()); flush(stdout())

### Name: plsda
### Title: Partial Least Squares and Sparse Partial Least Squares
###   Discriminant Analysis
### Aliases: plsda.default predict.plsda plsda splsda.default
###   predict.splsda splsda
### Keywords: models

### ** Examples

## Not run: 
##D data(mdrr)
##D set.seed(1)
##D inTrain <- sample(seq(along = mdrrClass), 450)
##D  
##D nzv <- nearZeroVar(mdrrDescr)
##D filteredDescr <- mdrrDescr[, -nzv]
##D 
##D training <- filteredDescr[inTrain,]
##D test <- filteredDescr[-inTrain,]
##D trainMDRR <- mdrrClass[inTrain]
##D testMDRR <- mdrrClass[-inTrain]
##D  
##D preProcValues <- preProcess(training)
##D 
##D trainDescr <- predict(preProcValues, training)
##D testDescr <- predict(preProcValues, test)
##D 
##D useBayes   <- plsda(trainDescr, trainMDRR, ncomp = 5,
##D                     probMethod = "Bayes")
##D useSoftmax <- plsda(trainDescr, trainMDRR, ncomp = 5)
##D 
##D confusionMatrix(predict(useBayes, testDescr),
##D                 testMDRR)
##D 
##D confusionMatrix(predict(useSoftmax, testDescr),
##D                 testMDRR)
##D 
##D histogram(~predict(useBayes, testDescr, type = "prob")[,"Active",]
##D           | testMDRR, xlab = "Active Prob", xlim = c(-.1,1.1))
##D histogram(~predict(useSoftmax, testDescr, type = "prob")[,"Active",]
##D           | testMDRR, xlab = "Active Prob", xlim = c(-.1,1.1))
##D 
##D 
##D ## different sized objects are returned
##D length(predict(useBayes, testDescr))
##D dim(predict(useBayes, testDescr, ncomp = 1:3))
##D dim(predict(useBayes, testDescr, type = "prob"))
##D dim(predict(useBayes, testDescr, type = "prob", ncomp = 1:3))
##D 
##D ## Using spls:
##D ## (As of 11/09, the spls package now has a similar function with
##D ## the same mane. To avoid conflicts, use caret:::splsda to 
##D ## get this version)
##D 
##D splsFit <- caret:::splsda(trainDescr, trainMDRR, 
##D                           K = 5, eta = .9,
##D                           probMethod = "Bayes")
##D 
##D confusionMatrix(caret:::predict.splsda(splsFit, testDescr),
##D                 testMDRR)
## End(Not run)



cleanEx()
nameEx("postResample")
### * postResample

flush(stderr()); flush(stdout())

### Name: postResample
### Title: Calculates performance across resamples
### Aliases: postResample defaultSummary twoClassSummary getTrainPerf
###   mnLogLoss R2 RMSE multiClassSummary
### Keywords: utilities

### ** Examples

predicted <-  matrix(rnorm(50), ncol = 5)
observed <- rnorm(10)
apply(predicted, 2, postResample, obs = observed)

classes <- c("class1", "class2")
set.seed(1)
dat <- data.frame(obs =  factor(sample(classes, 50, replace = TRUE)),
                  pred = factor(sample(classes, 50, replace = TRUE)),
                  class1 = runif(50), class2 = runif(50))

defaultSummary(dat, lev = classes)
twoClassSummary(dat, lev = classes)
mnLogLoss(dat, lev = classes)







cleanEx()
nameEx("prcomp.resamples")
### * prcomp.resamples

flush(stderr()); flush(stdout())

### Name: prcomp.resamples
### Title: Principal Components Analysis of Resampling Results
### Aliases: prcomp.resamples cluster.resamples cluster
###   plot.prcomp.resamples
### Keywords: hplot

### ** Examples

## Not run: 
##D #load(url("http://topepo.github.io/caret/exampleModels.RData"))
##D 
##D resamps <- resamples(list(CART = rpartFit,
##D                           CondInfTree = ctreeFit,
##D                           MARS = earthFit))
##D resampPCA <- prcomp(resamps)
##D 
##D resampPCA
##D 
##D plot(resampPCA, what = "scree")
##D 
##D plot(resampPCA, what = "components")
##D 
##D plot(resampPCA, what = "components", dims = 2, auto.key = list(columns = 3))
##D 
##D clustered <- cluster(resamps)
##D plot(clustered)
##D 
## End(Not run)



cleanEx()
nameEx("preProcess")
### * preProcess

flush(stderr()); flush(stdout())

### Name: preProcess
### Title: Pre-Processing of Predictors
### Aliases: preProcess preProcess.default predict.preProcess
### Keywords: utilities

### ** Examples

data(BloodBrain)
# one variable has one unique value
## Not run: 
##D preProc <- preProcess(bbbDescr)
##D 
##D preProc  <- preProcess(bbbDescr[1:100,-3])
##D training <- predict(preProc, bbbDescr[1:100,-3])
##D test     <- predict(preProc, bbbDescr[101:208,-3])
## End(Not run)



cleanEx()
nameEx("predict.bagEarth")
### * predict.bagEarth

flush(stderr()); flush(stdout())

### Name: predict.bagEarth
### Title: Predicted values based on bagged Earth and FDA models
### Aliases: predict.bagEarth predict.bagFDA
### Keywords: regression

### ** Examples

## Not run: 
##D data(trees)
##D ## out of bag predictions vs just re-predicting the training set
##D fit1 <- bagEarth(Volume ~ ., data = trees, keepX = TRUE)
##D fit2 <- bagEarth(Volume ~ ., data = trees, keepX = FALSE)
##D hist(predict(fit1) - predict(fit2))
## End(Not run)



cleanEx()
nameEx("predict.gafs")
### * predict.gafs

flush(stderr()); flush(stdout())

### Name: predict.gafs
### Title: Predict new samples
### Aliases: predict.gafs predict.safs
### Keywords: multivariate

### ** Examples

## Not run: 
##D 
##D set.seed(1)
##D train_data <- twoClassSim(100, noiseVars = 10)
##D test_data  <- twoClassSim(10,  noiseVars = 10)
##D 
##D ## A short example 
##D ctrl <- safsControl(functions = rfSA, 
##D                     method = "cv",
##D                     number = 3)
##D 
##D rf_search <- safs(x = train_data[, -ncol(train_data)],
##D                   y = train_data$Class,
##D                   iters = 3,
##D                   safsControl = ctrl)
##D 
##D rf_search
##D 
##D predict(rf_search, train_data)  
## End(Not run)



cleanEx()
nameEx("print.train")
### * print.train

flush(stderr()); flush(stdout())

### Name: print.train
### Title: Print Method for the train Class
### Aliases: print.train
### Keywords: print

### ** Examples

## Not run: 
##D data(iris)
##D TrainData <- iris[,1:4]
##D TrainClasses <- iris[,5]
##D 
##D library(klaR)
##D rdaFit <- train(TrainData, TrainClasses, method = "rda",
##D                 control = trainControl(method = "cv"))
##D print(rdaFit)
## End(Not run)



cleanEx()
nameEx("resampleHist")
### * resampleHist

flush(stderr()); flush(stdout())

### Name: resampleHist
### Title: Plot the resampling distribution of the model statistics
### Aliases: resampleHist
### Keywords: hplot

### ** Examples


## Not run: 
##D data(iris)
##D TrainData <- iris[,1:4]
##D TrainClasses <- iris[,5]
##D 
##D knnFit <- train(TrainData, TrainClasses, "knn")
##D 
##D resampleHist(knnFit)
## End(Not run)



cleanEx()
nameEx("resampleSummary")
### * resampleSummary

flush(stderr()); flush(stdout())

### Name: resampleSummary
### Title: Summary of resampled performance estimates
### Aliases: resampleSummary
### Keywords: utilities

### ** Examples

resampleSummary(rnorm(10), matrix(rnorm(50), ncol = 5))



cleanEx()
nameEx("resamples")
### * resamples

flush(stderr()); flush(stdout())

### Name: resamples
### Title: Collation and Visualization of Resampling Results
### Aliases: resamples.default resamples summary.resamples sort.resamples
###   as.matrix.resamples as.data.frame.resamples modelCor
### Keywords: models

### ** Examples


data(BloodBrain)
set.seed(1)

## tmp <- createDataPartition(logBBB,
##                            p = .8,
##                            times = 100)

## rpartFit <- train(bbbDescr, logBBB,
##                   "rpart", 
##                   tuneLength = 16,
##                   trControl = trainControl(
##                     method = "LGOCV", index = tmp))

## ctreeFit <- train(bbbDescr, logBBB,
##                   "ctree", 
##                   trControl = trainControl(
##                     method = "LGOCV", index = tmp))

## earthFit <- train(bbbDescr, logBBB,
##                   "earth",
##                   tuneLength = 20,
##                   trControl = trainControl(
##                     method = "LGOCV", index = tmp))

## or load pre-calculated results using:
## load(url("http://caret.r-forge.r-project.org/exampleModels.RData"))

## resamps <- resamples(list(CART = rpartFit,
##                           CondInfTree = ctreeFit,
##                           MARS = earthFit))

## resamps
## summary(resamps)



cleanEx()
nameEx("rf_seq")
### * rf_seq

flush(stderr()); flush(stdout())

### Name: var_seq
### Title: Sequences of Variables for Tuning
### Aliases: var_seq
### Keywords: models

### ** Examples

var_seq(p = 100, len = 10)
var_seq(p = 600, len = 10)



cleanEx()
nameEx("rfe")
### * rfe

flush(stderr()); flush(stdout())

### Name: rfe
### Title: Backwards Feature Selection
### Aliases: rfe rfe.default rfeIter predict.rfe update.rfe
### Keywords: models

### ** Examples

## Not run: 
##D data(BloodBrain)
##D 
##D x <- scale(bbbDescr[,-nearZeroVar(bbbDescr)])
##D x <- x[, -findCorrelation(cor(x), .8)]
##D x <- as.data.frame(x)
##D 
##D set.seed(1)
##D lmProfile <- rfe(x, logBBB,
##D                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
##D                  rfeControl = rfeControl(functions = lmFuncs, 
##D                                          number = 200))
##D set.seed(1)
##D lmProfile2 <- rfe(x, logBBB,
##D                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
##D                  rfeControl = rfeControl(functions = lmFuncs, 
##D                                          rerank = TRUE, 
##D                                          number = 200))
##D 
##D xyplot(lmProfile$results$RMSE + lmProfile2$results$RMSE  ~ 
##D        lmProfile$results$Variables, 
##D        type = c("g", "p", "l"), 
##D        auto.key = TRUE)
##D 
##D rfProfile <- rfe(x, logBBB,
##D                  sizes = c(2, 5, 10, 20),
##D                  rfeControl = rfeControl(functions = rfFuncs))
##D 
##D bagProfile <- rfe(x, logBBB,
##D                   sizes = c(2, 5, 10, 20),
##D                   rfeControl = rfeControl(functions = treebagFuncs))
##D 
##D set.seed(1)
##D svmProfile <- rfe(x, logBBB,
##D                   sizes = c(2, 5, 10, 20),
##D                   rfeControl = rfeControl(functions = caretFuncs, 
##D                                           number = 200),
##D                   ## pass options to train()
##D                   method = "svmRadial")
##D 
##D ## classification 
##D 
##D data(mdrr)
##D mdrrDescr <- mdrrDescr[,-nearZeroVar(mdrrDescr)]
##D mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .8)]
##D 
##D set.seed(1)
##D inTrain <- createDataPartition(mdrrClass, p = .75, list = FALSE)[,1]
##D 
##D train <- mdrrDescr[ inTrain, ]
##D test  <- mdrrDescr[-inTrain, ]
##D trainClass <- mdrrClass[ inTrain]
##D testClass  <- mdrrClass[-inTrain]
##D 
##D set.seed(2)
##D ldaProfile <- rfe(train, trainClass,
##D                   sizes = c(1:10, 15, 30),
##D                   rfeControl = rfeControl(functions = ldaFuncs, method = "cv"))
##D plot(ldaProfile, type = c("o", "g"))
##D 
##D postResample(predict(ldaProfile, test), testClass)
##D 
## End(Not run)

#######################################
## Parallel Processing Example via multicore

## Not run: 
##D library(doMC)
##D 
##D ## Note: if the underlying model also uses foreach, the
##D ## number of cores specified above will double (along with
##D ## the memory requirements)
##D registerDoMC(cores = 2)
##D 
##D set.seed(1)
##D lmProfile <- rfe(x, logBBB,
##D                  sizes = c(2:25, 30, 35, 40, 45, 50, 55, 60, 65),
##D                  rfeControl = rfeControl(functions = lmFuncs, 
##D                                          number = 200))
##D 
##D 
## End(Not run)




cleanEx()
nameEx("rfeControl")
### * rfeControl

flush(stderr()); flush(stdout())

### Name: rfeControl
### Title: Controlling the Feature Selection Algorithms
### Aliases: rfeControl
### Keywords: utilities

### ** Examples

  ## Not run: 
##D subsetSizes <- c(2, 4, 6, 8)
##D set.seed(123)
##D seeds <- vector(mode = "list", length = 51)
##D for(i in 1:50) seeds[[i]] <- sample.int(1000, length(subsetSizes) + 1)
##D seeds[[51]] <- sample.int(1000, 1)
##D 
##D set.seed(1)
##D rfMod <- rfe(bbbDescr, logBBB,
##D              sizes = subsetSizes,
##D              rfeControl = rfeControl(functions = rfFuncs, 
##D                                      seeds = seeds,
##D                                      number = 50))
##D   
## End(Not run)



cleanEx()
nameEx("rfeFunctions")
### * rfeFunctions

flush(stderr()); flush(stdout())

### Name: caretFuncs
### Title: Backwards Feature Selection Helper Functions
### Aliases: caretFuncs lmFuncs rfFuncs gamFuncs treebagFuncs ldaFuncs
###   nbFuncs lrFuncs pickSizeBest pickSizeTolerance pickVars
### Keywords: models

### ** Examples

## For picking subset sizes:
## Minimize the RMSE
example <- data.frame(RMSE = c(1.2, 1.1, 1.05, 1.01, 1.01, 1.03, 1.00),
                      Variables = 1:7)
## Percent Loss in performance (positive)
example$PctLoss <- (example$RMSE - min(example$RMSE))/min(example$RMSE)*100

xyplot(RMSE ~ Variables, data= example)
xyplot(PctLoss ~ Variables, data= example)

absoluteBest <- pickSizeBest(example, metric = "RMSE", maximize = FALSE)
within5Pct <- pickSizeTolerance(example, metric = "RMSE", maximize = FALSE)

cat("numerically optimal:",
    example$RMSE[absoluteBest],
    "RMSE in position",
    absoluteBest, "\n")
cat("Accepting a 1.5 pct loss:",
    example$RMSE[within5Pct],
    "RMSE in position",
    within5Pct, "\n")

## Example where we would like to maximize
example2 <- data.frame(Rsquared = c(0.4, 0.6, 0.94, 0.95, 0.95, 0.95, 0.95),
                      Variables = 1:7)
## Percent Loss in performance (positive)
example2$PctLoss <- (max(example2$Rsquared) - example2$Rsquared)/max(example2$Rsquared)*100

xyplot(Rsquared ~ Variables, data= example2)
xyplot(PctLoss ~ Variables, data= example2)

absoluteBest2 <- pickSizeBest(example2, metric = "Rsquared", maximize = TRUE)
within5Pct2 <- pickSizeTolerance(example2, metric = "Rsquared", maximize = TRUE)

cat("numerically optimal:",
    example2$Rsquared[absoluteBest2],
    "R^2 in position",
    absoluteBest2, "\n")
cat("Accepting a 1.5 pct loss:",
    example2$Rsquared[within5Pct2],
    "R^2 in position",
    within5Pct2, "\n")



cleanEx()
nameEx("sa_functions")
### * sa_functions

flush(stderr()); flush(stdout())

### Name: safs_initial
### Title: Ancillary simulated annealing functions
### Aliases: safs_initial safs_perturb safs_prob caretSA rfSA treebagSA

### ** Examples

selected_vars <- safs_initial(vars = 10 , prob = 0.2)
selected_vars

###

safs_perturb(selected_vars, vars = 10, number = 1)

###

safs_prob(old = .8, new = .9, iteration = 1)
safs_prob(old = .5, new = .6, iteration = 1)

grid <- expand.grid(old = c(4, 3.5),
                    new = c(4.5, 4, 3.5) + 1,
                    iter = 1:40)
grid <- subset(grid, old < new)

grid$prob <- apply(grid, 1, 
                   function(x) 
                     safs_prob(new = x["new"], 
                               old= x["old"], 
                               iteration = x["iter"]))

grid$Difference <- factor(grid$new - grid$old)
grid$Group <- factor(paste("Current Value", grid$old))

ggplot(grid, aes(x = iter, y = prob, color = Difference)) + 
  geom_line() + facet_wrap(~Group) + theme_bw() +
  ylab("Probability") + xlab("Iteration")

## Not run: 
##D ###
##D ## Hypothetical examples
##D lda_sa <- safs(x = predictors,
##D                y = classes,
##D                safsControl = safsControl(functions = caretSA),
##D                ## now pass arguments to `train`
##D                method = "lda",
##D                metric = "Accuracy"
##D                trControl = trainControl(method = "cv", classProbs = TRUE))
##D 
##D rf_sa <- safs(x = predictors,
##D               y = classes,
##D               safsControl = safsControl(functions = rfSA),
##D               ## these are arguments to `randomForest`
##D               ntree = 1000,
##D               importance = TRUE)
##D 	
## End(Not run)





cleanEx()
nameEx("safs.default")
### * safs.default

flush(stderr()); flush(stdout())

### Name: safs.default
### Title: Simulated annealing feature selection
### Aliases: safs.default safs
### Keywords: models

### ** Examples

## Not run: 
##D 
##D set.seed(1)
##D train_data <- twoClassSim(100, noiseVars = 10)
##D test_data  <- twoClassSim(10,  noiseVars = 10)
##D 
##D ## A short example 
##D ctrl <- safsControl(functions = rfSA, 
##D                     method = "cv",
##D                     number = 3)
##D 
##D rf_search <- safs(x = train_data[, -ncol(train_data)],
##D                   y = train_data$Class,
##D                   iters = 3,
##D                   safsControl = ctrl)
##D 
##D rf_search 
## End(Not run)



cleanEx()
nameEx("sbf")
### * sbf

flush(stderr()); flush(stdout())

### Name: sbf
### Title: Selection By Filtering (SBF)
### Aliases: sbf sbf.default sbf.formula predict.sbf
### Keywords: models

### ** Examples

## Not run: 
##D data(BloodBrain)
##D 
##D ## Use a GAM is the filter, then fit a random forest model
##D RFwithGAM <- sbf(bbbDescr, logBBB,
##D                  sbfControl = sbfControl(functions = rfSBF,
##D                                          verbose = FALSE, 
##D                                          method = "cv"))
##D RFwithGAM
##D 
##D predict(RFwithGAM, bbbDescr[1:10,])
##D 
##D ## classification example with parallel processing
##D 
##D ## library(doMC)
##D 
##D ## Note: if the underlying model also uses foreach, the
##D ## number of cores specified above will double (along with
##D ## the memory requirements)
##D ## registerDoMC(cores = 2)
##D 
##D data(mdrr)
##D mdrrDescr <- mdrrDescr[,-nearZeroVar(mdrrDescr)]
##D mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .8)]
##D 
##D set.seed(1)
##D filteredNB <- sbf(mdrrDescr, mdrrClass,
##D                  sbfControl = sbfControl(functions = nbSBF,
##D                                          verbose = FALSE, 
##D                                          method = "repeatedcv",
##D                                          repeats = 5))
##D confusionMatrix(filteredNB)
## End(Not run)




cleanEx()
nameEx("sbfControl")
### * sbfControl

flush(stderr()); flush(stdout())

### Name: sbfControl
### Title: Control Object for Selection By Filtering (SBF)
### Aliases: sbfControl
### Keywords: utilities

### ** Examples

## Not run: 
##D data(BloodBrain)
##D 
##D ## Use a GAM is the filter, then fit a random forest model
##D set.seed(1)
##D RFwithGAM <- sbf(bbbDescr, logBBB,
##D                  sbfControl = sbfControl(functions = rfSBF,
##D                                          verbose = FALSE, 
##D                                          seeds = sample.int(100000, 11),
##D                                          method = "cv"))
##D RFwithGAM
##D 
##D 
##D ## A simple example for multivariate scoring
##D rfSBF2 <- rfSBF
##D rfSBF2$score <- function(x, y) apply(x, 2, rfSBF$score, y = y)
##D 
##D set.seed(1)
##D RFwithGAM2 <- sbf(bbbDescr, logBBB,
##D                   sbfControl = sbfControl(functions = rfSBF2,
##D                                           verbose = FALSE, 
##D                                           seeds = sample.int(100000, 11),
##D                                           method = "cv",
##D                                           multivariate = TRUE))
##D RFwithGAM2
##D 
##D 
## End(Not run)


cleanEx()
nameEx("selection")
### * selection

flush(stderr()); flush(stdout())

### Name: oneSE
### Title: Selecting tuning Parameters
### Aliases: oneSE best tolerance
### Keywords: manip

### ** Examples

## Not run: 
##D # simulate a PLS regression model
##D test <- data.frame(ncomp = 1:5,
##D                    RMSE = c(3, 1.1, 1.02, 1, 2),
##D                    RMSESD = .4)
##D 
##D best(test, "RMSE", maximize = FALSE)
##D oneSE(test, "RMSE", maximize = FALSE, num = 10)
##D tolerance(test, "RMSE", tol = 3, maximize = FALSE)
##D 
##D ### usage example
##D 
##D data(BloodBrain)
##D 
##D marsGrid <- data.frame(degree = 1, nprune = (1:10) * 3)
##D 
##D set.seed(1)
##D marsFit <- train(bbbDescr, logBBB,
##D                  method = "earth",
##D                  tuneGrid = marsGrid,
##D                  trControl = trainControl(method = "cv",
##D                                           number = 10,
##D                                           selectionFunction = "tolerance"))
##D 
##D # around 18 terms should yield the smallest CV RMSE     
## End(Not run)




cleanEx()
nameEx("sensitivity")
### * sensitivity

flush(stderr()); flush(stdout())

### Name: sensitivity
### Title: Calculate sensitivity, specificity and predictive values
### Aliases: sensitivity sensitivity.default sensitivity.table
###   sensitivity.matrix specificity specificity.default specificity.table
###   specificity.matrix posPredValue posPredValue.default
###   posPredValue.table posPredValue.matrix negPredValue
###   negPredValue.default negPredValue.table negPredValue.matrix
### Keywords: manip

### ** Examples

## Not run: 
##D ###################
##D ## 2 class example
##D 
##D lvs <- c("normal", "abnormal")
##D truth <- factor(rep(lvs, times = c(86, 258)),
##D                 levels = rev(lvs))
##D pred <- factor(
##D                c(
##D                  rep(lvs, times = c(54, 32)),
##D                  rep(lvs, times = c(27, 231))),               
##D                levels = rev(lvs))
##D 
##D xtab <- table(pred, truth)
##D 
##D sensitivity(pred, truth)
##D sensitivity(xtab)
##D posPredValue(pred, truth)
##D posPredValue(pred, truth, prevalence = 0.25)
##D 
##D specificity(pred, truth)
##D negPredValue(pred, truth)
##D negPredValue(xtab)
##D negPredValue(pred, truth, prevalence = 0.25)
##D 
##D 
##D prev <- seq(0.001, .99, length = 20)
##D npvVals <- ppvVals <- prev  * NA
##D for(i in seq(along = prev))
##D   {
##D     ppvVals[i] <- posPredValue(pred, truth, prevalence = prev[i])
##D     npvVals[i] <- negPredValue(pred, truth, prevalence = prev[i])
##D   }
##D 
##D plot(prev, ppvVals,
##D      ylim = c(0, 1),
##D      type = "l",
##D      ylab = "",
##D      xlab = "Prevalence (i.e. prior)")
##D points(prev, npvVals, type = "l", col = "red")
##D abline(h=sensitivity(pred, truth), lty = 2)
##D abline(h=specificity(pred, truth), lty = 2, col = "red")
##D legend(.5, .5,
##D        c("ppv", "npv", "sens", "spec"),
##D        col = c("black", "red", "black", "red"),
##D        lty = c(1, 1, 2, 2))
##D 
##D ###################
##D ## 3 class example
##D 
##D library(MASS)
##D 
##D fit <- lda(Species ~ ., data = iris)
##D model <- predict(fit)$class
##D 
##D irisTabs <- table(model, iris$Species)
##D 
##D ## When passing factors, an error occurs with more
##D ## than two levels
##D sensitivity(model, iris$Species)
##D 
##D ## When passing a table, more than two levels can
##D ## be used
##D sensitivity(irisTabs, "versicolor")
##D specificity(irisTabs, c("setosa", "virginica"))
## End(Not run)



cleanEx()
nameEx("spatialSign")
### * spatialSign

flush(stderr()); flush(stdout())

### Name: spatialSign
### Title: Compute the multivariate spatial sign
### Aliases: spatialSign spatialSign.default spatialSign.matrix
###   spatialSign.data.frame
### Keywords: manip

### ** Examples

spatialSign(rnorm(5))

spatialSign(matrix(rnorm(12), ncol = 3))

# should fail since the fifth column is a factor
try(spatialSign(iris), silent = TRUE)

spatialSign(iris[,-5])

trellis.par.set(caretTheme())
featurePlot(iris[,-5], iris[,5], "pairs")
featurePlot(spatialSign(scale(iris[,-5])), iris[,5], "pairs")



cleanEx()
nameEx("summary.bagEarth")
### * summary.bagEarth

flush(stderr()); flush(stdout())

### Name: summary.bagEarth
### Title: Summarize a bagged earth or FDA fit
### Aliases: summary.bagEarth summary.bagFDA
### Keywords: manip

### ** Examples

## Not run: 
##D data(trees)
##D fit <- bagEarth(trees[,-3], trees[3])
##D summary(fit)
## End(Not run)



cleanEx()
nameEx("tecator")
### * tecator

flush(stderr()); flush(stdout())

### Name: tecator
### Title: Fat, Water and Protein Content of Meat Samples
### Aliases: tecator absorp endpoints
### Keywords: datasets

### ** Examples

data(tecator)

splom(~endpoints)

# plot 10 random spectra
set.seed(1)
inSubset <- sample(1:dim(endpoints)[1], 10)

absorpSubset <- absorp[inSubset,]
endpointSubset <- endpoints[inSubset, 3]

newOrder <- order(absorpSubset[,1])
absorpSubset <- absorpSubset[newOrder,]
endpointSubset <- endpointSubset[newOrder]

plotColors <- rainbow(10)

plot(absorpSubset[1,], 
     type = "n", 
     ylim = range(absorpSubset), 
     xlim = c(0, 105),
     xlab = "Wavelength Index", 
     ylab = "Absorption")
   
for(i in 1:10)
{
   points(absorpSubset[i,], type = "l", col = plotColors[i], lwd = 2)
   text(105, absorpSubset[i,100], endpointSubset[i], col = plotColors[i])
}
title("Predictor Profiles for 10 Random Samples")



cleanEx()
nameEx("train")
### * train

flush(stderr()); flush(stdout())

### Name: train
### Title: Fit Predictive Models over Different Tuning Parameters
### Aliases: train train.default train.formula
### Keywords: models

### ** Examples

## Not run: 
##D 
##D #######################################
##D ## Classification Example
##D 
##D data(iris)
##D TrainData <- iris[,1:4]
##D TrainClasses <- iris[,5]
##D 
##D knnFit1 <- train(TrainData, TrainClasses,
##D                  method = "knn",
##D                  preProcess = c("center", "scale"),
##D                  tuneLength = 10,
##D                  trControl = trainControl(method = "cv"))
##D 
##D knnFit2 <- train(TrainData, TrainClasses,
##D                  method = "knn",
##D                  preProcess = c("center", "scale"),
##D                  tuneLength = 10, 
##D                  trControl = trainControl(method = "boot"))
##D 
##D 
##D library(MASS)
##D nnetFit <- train(TrainData, TrainClasses,
##D                  method = "nnet",
##D                  preProcess = "range", 
##D                  tuneLength = 2,
##D                  trace = FALSE,
##D                  maxit = 100)
##D 
##D #######################################
##D ## Regression Example
##D 
##D library(mlbench)
##D data(BostonHousing)
##D 
##D lmFit <- train(medv ~ . + rm:lstat,
##D                data = BostonHousing, 
##D                method = "lm")
##D 
##D library(rpart)
##D rpartFit <- train(medv ~ .,
##D                   data = BostonHousing,
##D                   method = "rpart",
##D                   tuneLength = 9)
##D 
##D #######################################
##D ## Example with a custom metric
##D 
##D madSummary <- function (data,
##D                         lev = NULL,
##D                         model = NULL) {
##D   out <- mad(data$obs - data$pred, 
##D              na.rm = TRUE)  
##D   names(out) <- "MAD"
##D   out
##D }
##D 
##D robustControl <- trainControl(summaryFunction = madSummary)
##D marsGrid <- expand.grid(degree = 1, nprune = (1:10) * 2)
##D 
##D earthFit <- train(medv ~ .,
##D                   data = BostonHousing, 
##D                   method = "earth",
##D                   tuneGrid = marsGrid,
##D                   metric = "MAD",
##D                   maximize = FALSE,
##D                   trControl = robustControl)
##D 
##D #######################################
##D ## Parallel Processing Example via multicore package
##D 
##D ## library(doMC)
##D ## registerDoMC(2)
##D 
##D ## NOTE: don't run models form RWeka when using
##D ### multicore. The session will crash.
##D 
##D ## The code for train() does not change:
##D set.seed(1)
##D usingMC <-  train(medv ~ .,
##D                   data = BostonHousing, 
##D                   method = "glmboost")
##D 
##D ## or use:
##D ## library(doMPI) or 
##D ## library(doParallel) or 
##D ## library(doSMP) and so on
##D 
## End(Not run)




cleanEx()
nameEx("trainControl")
### * trainControl

flush(stderr()); flush(stdout())

### Name: trainControl
### Title: Control parameters for train
### Aliases: trainControl
### Keywords: utilities

### ** Examples

## Not run: 
##D 
##D ## Do 5 repeats of 10-Fold CV for the iris data. We will fit
##D ## a KNN model that evaluates 12 values of k and set the seed
##D ## at each iteration.
##D 
##D set.seed(123)
##D seeds <- vector(mode = "list", length = 51)
##D for(i in 1:50) seeds[[i]] <- sample.int(1000, 22)
##D 
##D ## For the last model:
##D seeds[[51]] <- sample.int(1000, 1)
##D 
##D ctrl <- trainControl(method = "repeatedcv",
##D                      repeats = 5,
##D                      seeds = seeds)
##D 
##D set.seed(1)
##D mod <- train(Species ~ ., data = iris,
##D              method = "knn",
##D              tuneLength = 12,
##D              trControl = ctrl)
##D 
##D 
##D ctrl2 <- trainControl(method = "adaptive_cv",
##D                       repeats = 5,
##D                       verboseIter = TRUE,
##D                       seeds = seeds)
##D 
##D set.seed(1)
##D mod2 <- train(Species ~ ., data = iris,
##D               method = "knn",
##D               tuneLength = 12,
##D               trControl = ctrl2)
##D 
## End(Not run)



cleanEx()
nameEx("twoClassSim")
### * twoClassSim

flush(stderr()); flush(stdout())

### Name: twoClassSim
### Title: Simulation Functions
### Aliases: twoClassSim SLC14_1 SLC14_2 LPH07_1 LPH07_2
### Keywords: models

### ** Examples

example <- twoClassSim(100, linearVars = 1)
splom(~example[, 1:6], groups = example$Class)



cleanEx()
nameEx("update.safs")
### * update.safs

flush(stderr()); flush(stdout())

### Name: update.safs
### Title: Update or Re-fit a SA or GA Model
### Aliases: update.safs update.gafs
### Keywords: models

### ** Examples

## Not run: 
##D set.seed(1)
##D train_data <- twoClassSim(100, noiseVars = 10)
##D test_data  <- twoClassSim(10,  noiseVars = 10)
##D 
##D ## A short example 
##D ctrl <- safsControl(functions = rfSA, 
##D                     method = "cv",
##D                     number = 3)
##D 
##D rf_search <- safs(x = train_data[, -ncol(train_data)],
##D                   y = train_data$Class,
##D                   iters = 3,
##D                   safsControl = ctrl)
##D 
##D rf_search2 <- update(rf_search, 
##D 	                 iter = 1,
##D 	                 x = train_data[, -ncol(train_data)],
##D                      y = train_data$Class)
##D rf_search2
## End(Not run)



cleanEx()
nameEx("update.train")
### * update.train

flush(stderr()); flush(stdout())

### Name: update.train
### Title: Update or Re-fit a Model
### Aliases: update.train
### Keywords: models

### ** Examples

## Not run: 
##D data(iris)
##D TrainData <- iris[,1:4]
##D TrainClasses <- iris[,5]
##D 
##D knnFit1 <- train(TrainData, TrainClasses,
##D                  method = "knn",
##D                  preProcess = c("center", "scale"),
##D                  tuneLength = 10,
##D                  trControl = trainControl(method = "cv"))
##D 
##D update(knnFit1, list(.k = 3))
## End(Not run)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
