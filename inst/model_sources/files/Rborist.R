modelInfo <- list(label = "Random Forest",
                  library = "Rborist",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("predFixed","minNode"),
                                          class = c("numeric","numeric"),
                                          label = c("#Randomly Selected Predictors","Minimal Node Size")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(predFixed = caret::var_seq(p = ncol(x),
                                                                   classification = is.factor(y),
                                                                   len = len),
                                        minNode = ifelse(is.factor(y), 2, 3))
                    } else {
                      out <- data.frame(predFixed = sample(1:ncol(x), size = len, replace = TRUE), #removed unique
                                        minNode = sample(1:(min(20,nrow(x))), size = len, replace = TRUE)) # might cause warning for very small samples < 20
                    }
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...){
                    Rborist::Rborist(x, y, predFixed = param$predFixed, minNode = param$minNode, ...)
                  } ,
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)$yPred
                    if(modelFit$problemType == "Classification") out <- modelFit$obsLevels[out]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, newdata)$census
                    out <- t(apply(out, 1, function(x) x/sum(x)))
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
                                  Classification =  c(sum(diag(x$validation$confusion))/sum(x$validation$confusion),
                                                      e1071::classAgreement(x$validation$confusion)[["kappa"]]))
                    names(out) <- if(x$problemType == "Regression") c("RMSE", "Rsquared") else c("Accuracy", "Kappa")
                    out
                  })
