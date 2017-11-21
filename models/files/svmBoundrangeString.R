modelInfo <- list(label = "Support Vector Machines with Boundrange String Kernel",
                  library = "kernlab",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('length', 'C'),
                                          class = c("numeric", "numeric"),
                                          label = c('length', "Cost")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(length = 2:(len+1),
                                         C = 2 ^((1:len) - 3))
                    } else {
                      out <- data.frame(length = sample(1:20, size = len, replace = TRUE),
                                        C = 2^runif(len, min = -5, max = 10))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    if(any(names(list(...)) == "prob.model") | is.numeric(y))
                    {
                      out <- kernlab::ksvm(x = x[,1], y = y,
                                           kernel = "stringdot",
                                           kpar = list(type = "boundrange",
                                                       length = param$length),
                                           C = param$C, ...)
                    } else {
                      out <- kernlab::ksvm(x = x[,1], y = y,
                                           kernel = "stringdot",
                                           kpar = list(type = "boundrange",
                                                       length = param$length),
                                           C = param$C,
                                           prob.model = classProbs,
                                           ...)
                    }
                    
                    out            
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    svmPred <- function(obj, x)
                    {
                      hasPM <- !is.null(unlist(obj@prob.model))
                      if(hasPM) {
                        pred <- kernlab::lev(obj)[apply(kernlab::predict(obj, x, type = "probabilities"),
                                               1, which.max)]
                      } else pred <- kernlab::predict(obj, x)
                      pred
                    }
                    out <- try(svmPred(modelFit, newdata[,1]), silent = TRUE)
                    if(is.character(kernlab::lev(modelFit)))
                    {
                      if(class(out)[1] == "try-error")
                      {
                        warning("kernlab class prediction calculations failed; returning NAs")
                        out <- rep("", nrow(newdata))
                        out[seq(along = out)] <- NA
                      }
                    } else {
                      if(class(out)[1] == "try-error")
                      {
                        warning("kernlab prediction calculations failed; returning NAs")
                        out <- rep(NA, nrow(newdata))
                      } 
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- try(kernlab::predict(modelFit, newdata[,1], type="probabilities"),
                               silent = TRUE)
                    if(class(out)[1] != "try-error")
                    {
                      ## There are times when the SVM probability model will
                      ## produce negative class probabilities, so we
                      ## induce vlaues between 0 and 1
                      if(any(out < 0))
                      {
                        out[out < 0] <- 0
                        out <- t(apply(out, 1, function(x) x/sum(x)))
                      }
                      out <- out[, kernlab::lev(modelFit), drop = FALSE]
                    } else {
                      warning("kernlab class probability calculations failed; returning NAs")
                      out <- matrix(NA, nrow(newdata) * length(kernlab::lev(modelFit)), ncol = length(kernlab::lev(modelFit)))
                      colnames(out) <- kernlab::lev(modelFit)
                    }
                    out
                  },
                  predictors = function(x, ...){
                    iNA
                  },
                  tags = c("Kernel Method", "Support Vector Machines", "String Kernel",
                           "Robust Methods", "Text Mining"),
                  levels = function(x) kernlab::lev(x),
                  sort = function(x) {
                    # If the cost is high, the decision boundary will work hard to
                    # adapt. Also, if C is fixed, smaller values of sigma yeild more
                    # complex boundaries
                    x[order(x$C, -x$length),]
                  })
