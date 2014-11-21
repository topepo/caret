modelInfo <- list(label = "Bagged MARS using gCV Pruning", 
                  library = "earth",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('degree'),
                                          class = c("numeric"),
                                          label = c('Product Degree')),
                  grid = function(x, y, len = NULL)  data.frame(degree = 1),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    if(is.factor(y)){
                      mod <- bagEarth(x, y, degree = param$degree, 
                                      glm = list(family=binomial, maxit=100),
                                      ...)
                    } else {
                      mod <- bagEarth(x, y, degree = param$degree, ...)
                    }
                    mod
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(modelFit$problemType == "Classification")
                    {
                      out <- predict(modelFit, newdata,  type = "class")
                    } else {
                      out <- predict(modelFit, newdata)
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, type= "response")
                    out <- cbind(1-out, out)
                    colnames(out) <-  modelFit$obsLevels
                    out
                  },
                  predictors = function(x, ...) {
                    predEarth <- function(x) {
                      vi <- varImp(x)
                      notZero <- sort(unique(unlist(lapply(vi, function(x) which(x > 0)))))
                      if(length(notZero) > 0) rownames(vi)[notZero] else NULL
                    }
                    eachFit <- lapply(x$fit, predEarth)
                    unique(unlist(eachFit))
                  },
                  varImp = function(object, ...) {
                    allImp <- lapply(object$fit, varImp, ...)
                    impDF <- as.data.frame(allImp)
                    meanImp <- apply(impDF, 1, mean)
                    out <- data.frame(Overall = meanImp)
                    rownames(out) <- names(meanImp)
                    out
                  },
                  levels = function(x) x$levels,
                  tags = c("Multivariate Adaptive Regression Splines", "Ensemble Model", 
                           "Implicit Feature Selection", "Bagging"),
                  sort = function(x) x[order(x$degree),])
