modelInfo <- list(label = "Support Vector Machines with Linear Kernel",
                  library = "e1071",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('cost'),
                                          class = c("numeric"),
                                          label = c("Cost")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(cost = 2 ^((1:len) - 3))
                    } else {
                      out <- data.frame(cost = 2^runif(len, min = -5, max = 10))
                    }
                    out
                  }, 
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(any(names(list(...)) == "probability") | is.numeric(y))
                    {
                      out <- e1071::svm(x = as.matrix(x), y = y,
                                        kernel = "linear",
                                        cost = param$cost,
                                        ...)
                    } else {
                      out <- e1071::svm(x = as.matrix(x), y = y,
                                        kernel = "linear",
                                        cost = param$cost,
                                        probability = classProbs,
                                        ...)
                    }

                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata)
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, probability = TRUE)
                    attr(out, "probabilities")
                  },
                  predictors = function(x, ...){
                    out <- if(!is.null(x$terms)) predictors.terms(x$terms) else x$xNames
                    if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
                    if(is.null(out)) out <-NA
                    out
                  },
                  tags = c("Kernel Method", "Support Vector Machines","Linear Regression", "Linear Classifier",
                           "Robust Methods"),
                  levels = function(x) x$levels,
                  sort = function(x) {
                    # If the cost is high, the decision boundary will work hard to
                    # adapt. Also, if C is fixed, smaller values of sigma yeild more
                    # complex boundaries
                    x[order(x$cost),]
                  })
