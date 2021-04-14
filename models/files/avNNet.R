modelInfo <- list(label = "Model Averaged Neural Network", 
                  library = "nnet",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c('size', 'decay', 'bag'),
                                          class = c(rep("numeric", 2), "logical"),
                                          label = c('#Hidden Units', 'Weight Decay', 'Bagging')),
                  grid =  function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(size = ((1:len) * 2) - 1, 
                                         decay = c(0, 10 ^ seq(-1, -4, length = len - 1)),
                                         bag = FALSE)
                    } else {
                      out <- data.frame(size = sample(1:20, size = len, replace = TRUE), 
                                        decay = 10^runif(len, min = -5, 1),
                                        bag = sample(c(TRUE, FALSE), size = len, replace = TRUE))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    if(!is.null(wts)) {
                      out <- caret::avNNet(.outcome ~ .,
                                    data = dat,
                                    weights = wts,                                       
                                    size = param$size,
                                    decay = param$decay,
                                    bag = param$bag,
                                    ...)
                    } else out <- caret::avNNet(.outcome ~ .,
                                         data = dat,
                                         size = param$size,
                                         decay = param$decay,
                                         bag = param$bag,
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
                    out <- predict(modelFit, newdata, type = "prob")
                    if(ncol(as.data.frame(out, stringsAsFactors = TRUE)) == 1)
                    {
                      out <- cbind(out, 1-out)
                      dimnames(out)[[2]] <-  rev(modelFit$obsLevels)
                    }
                    out
                  },
                  predictors = function(x, ...) x$names,
                  levels = function(x) x$model[[1]]$lev,
                  tags = c("Neural Network", "Ensemble Model", "Bagging", "L2 Regularization", "Accepts Case Weights"),
                  sort = function(x) x[order(x$size, -x$decay),])
