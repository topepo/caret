modelInfo <- list(label = "Ridge Regression",
                  library = "elasticnet",
                  type = "Regression",
                  parameters = data.frame(parameter = c('lambda'),
                                          class = c("numeric"),
                                          label = c('Weight Decay')),
                  grid = function(x, y, len = NULL, search = "grid")  {
                    if(search == "grid") {
                      out <- expand.grid(lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
                    } else {
                      out <- data.frame(lambda = 10^runif(len, min = -5, 1))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    elasticnet::enet(as.matrix(x), y, lambda = param$lambda)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    elasticnet::predict.enet(
                      modelFit, newdata, 
                      s = 1, 
                      mode = "fraction")$fit
                  },
                  predictors = function(x, s = NULL, ...) {
                    if(is.null(s)) {
                      if(!is.null(x$tuneValue)) {
                        s <- x$tuneValue$.fraction
                      } else stop("must supply a vaue of s")
                      out <- elasticnet::predict.enet(
                        x, s = s,
                        type = "coefficients",
                        mode = "fraction")$coefficients
                    } else {
                      out <- elasticnet::predict.enet(x, s = s)$coefficients
                      
                    }
                    names(out)[out != 0]
                  },
                  tags = c("Linear Regression", "L2 Regularization"),
                  prob = NULL,
                  sort = function(x) x[order(-x$lambda),])
