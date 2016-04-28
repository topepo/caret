modelInfo <- list(label = "Random Forest",
                  library = "Rborist",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = "predProb",
                                          class = "numeric",
                                          label = "Probability of Selection"),
                  grid = function(x, y, len = NULL, search = "grid") {
                    out <- if(search == "grid") 
                      data.frame(predProb = seq(0, 1, length = len)) else
                      data.frame(predProb = runif(len))
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    Rborist(x, y, predProb = param$predProb, ...),
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)$yPred
                    if(modelFit$problemType == "Classification") out <- modelFit$obsLevels[out]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, newdata)$census
                    out <- apply(out, 1, function(x) x/sum(x))
                    out
                  },
                  predictors = function(x, ...) x$xNames[x$training$info != 0],
                  varImp = function(object, ...){
                    out <- data.frame(Overall = object$training$info)
                    rownames(out) <- object$xNames
                    out
                  },
                  levels = function(x) colnames(x$validation$confusion),
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),],
                  oob = function(x) {
                    out <- switch(x$problemType ,
                                  Regression =   c(sqrt(x$validation$mse), x$validation$rsq),
                                  Classification =  c(1 - x$err.rate[x$ntree, "OOB"],
                                                      e1071::classAgreement(x$validation$confusion)[["kappa"]]))
                    names(out) <- if(x$problemType == "Regression") c("RMSE", "Rsquared") else c("Accuracy", "Kappa")
                    out
                  })

