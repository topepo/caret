modelInfo <- list(label = "Bagged MARS using gCV Pruning",
                  library = "earth",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('degree'),
                                          class = c("numeric"),
                                          label = c('Product Degree')),
                  grid = function(x, y, len = NULL, search = "grid")  data.frame(degree = 1),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                  	require(earth)
                    theDots <- list(...)
                    theDots$keepxy <- TRUE

                    ## pass in any model weights
                    if (!is.null(wts))
                      theDots$weights <- wts

                    modelArgs <- c(list(x = x, y = y, degree = param$degree), theDots)

                    if (is.factor(y) & !any(names(theDots) == "glm")) {
                      modelArgs$glm <- list(family = binomial, maxit = 100)
                    }

                    tmp <- do.call(getFromNamespace("bagEarth.default", "caret"), modelArgs)

                    tmp$call["degree"] <-  param$degree
                    tmp
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if (modelFit$problemType == "Classification") {
                      out <- predict(modelFit, newdata,  type = "class")
                    } else {
                      out <- predict(modelFit, newdata)
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata, type= "prob")
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
                    allImp <- lapply(allImp,
                                     function (x) {
                                       x$var <- rownames(x)
                                       x
                                     },
                                     ...)
                    allImp <- do.call("rbind", allImp)

                    impDF <- plyr::ddply(allImp, .(var), function(x) c(Overall = mean(x$Overall, rm.na = TRUE)))
                    out <- data.frame(Overall = impDF$Overall)
                    rownames(out) <- impDF$var
                    out
                  },
                  levels = function(x) x$levels,
                  tags = c("Multivariate Adaptive Regression Splines", "Ensemble Model",
                           "Implicit Feature Selection", "Bagging", "Accepts Case Weights"),
                  notes = paste(
                    "Unlike other packages used by `train`, the `earth`",
                    "package is fully loaded when this model is used."
                  ),
                  sort = function(x) x[order(x$degree),],
                  oob = function(x) apply(x$oob, 2, function(x) quantile(x, probs = .5)))
