modelInfo <- list(label = "Random Forest",
                  library = c("e1071", "ranger"),
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = "mtry",
                                          class = "numeric",
                                          label = "#Randomly Selected Predictors"),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(mtry = caret::var_seq(p = ncol(x), 
                                                              classification = is.factor(y), 
                                                              len = len))
                    } else {
                      out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, replace = TRUE)))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    if(!is.data.frame(x)) x <- as.data.frame(x)
                    x$.outcome <- y
                    if(!is.null(wts)) {
                      out <- ranger(.outcome ~ ., data = x, mtry = param$mtry, write.forest = TRUE, 
                                    probability = classProbs, case.weights = wts, ...)
                    } else {
                      out <- ranger(.outcome ~ ., data = x, mtry = param$mtry, write.forest = TRUE, 
                                    probability = classProbs, ...)
                    }
                    ## in case the resampling method is "oob"
                    if(!last) out$y <- y
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    out <- predict(modelFit, newdata)$predictions
                    if(!is.null(modelFit$obsLevels) & modelFit$treetype == "Probability estimation") {
                      out <- colnames(out)[apply(out, 1, which.max)]
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata)$predictions
                  },
                  predictors = function(x, ...) {
                    var_index <- sort(unique(unlist(lapply(x$forest$split.varIDs, function(x) x))))
                    var_index <-var_index[var_index > 0]
                    x$forest$independent.variable.names[var_index] 
                  },
                  varImp = function(object, ...){
                    if(length(object$variable.importance) == 0)
                      stop("No importance values available")
                    imps <- importance(object)
                    out <- data.frame(Overall = as.vector(imps))
                    rownames(out) <- names(imps)
                    out
                  },
                  levels = function(x) {
                    if(x$treetype == "Probability estimation") {
                      out <- colnames(x$predictions)
                    } else {
                      if(x$treetype == "Classification") {
                        out <- levels(x$predictions)
                      } else out <- NULL
                    } 
                    out
                  },
                  oob = function(x) {
                    postResample(x$predictions, x$y)
                  },
                  tags = c("Random Forest", "Ensemble Model", "Bagging", 
                           "Implicit Feature Selection", "Accepts Case Weights"),
                  sort = function(x) x[order(x[,1]),])
