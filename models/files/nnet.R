modelInfo <- list(label = "Neural Network",
                  library = "nnet",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c('size', 'decay'),
                                          class = rep("numeric", 2),
                                          label = c('#Hidden Units', 'Weight Decay')),
                  grid = function(x, y, len = NULL) expand.grid(size = ((1:len) * 2) - 1, 
                                                                decay = c(0, 10 ^ seq(-1, -4, length = len - 1))),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    if(!is.null(wts))
                    {
                      out <- nnet(.outcome ~ .,
                                  data = dat,
                                  weights = wts,                                       
                                  size = param$size,
                                  decay = param$decay,
                                  ...)
                    } else out <- nnet(.outcome ~ .,
                                       data = dat,
                                       size = param$size,
                                       decay = param$decay,
                                       ...)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL)
                  {
                    if(modelFit$problemType == "Classification")
                    {
                      out <- predict(modelFit, newdata, type="class")
                    } else {
                      out  <- predict(modelFit, newdata, type="raw")
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, newdata)
                    if(ncol(as.data.frame(out)) == 1)
                    {
                      out <- cbind(out, 1-out)
                      dimnames(out)[[2]] <-  rev(modelFit$obsLevels)
                    }
                    out
                  },
                  varImp = function(object, ...) {
                    imp <- caret:::GarsonWeights(object, ...)
                    if(ncol(imp) > 1) {
                      imp <- cbind(apply(imp, 1, mean), imp)
                      colnames(imp)[1] <- "Overall"
                    } else {
                      imp <- as.data.frame(imp)
                      names(imp) <- "Overall"
                    }
                    if(!is.null(object$xNames)) rownames(imp) <- object$xNames
                    imp
                  },
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else NA,
                  tags = c("Neural Network", "L2 Regularization"),
                  levels = function(x) x$lev,
                  sort = function(x) x[order(x$size, -x$decay),])
