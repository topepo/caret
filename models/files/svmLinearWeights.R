modelInfo <- list(label = "Linear Support Vector Machines with Class Weights",
                  library = "e1071",
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('cost','weight'),
                                          class = c("numeric",'numeric'),
                                          label = c("Cost",'Class Weight')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(cost = 2 ^((1:len) - 3), weight = 1:len)
                    } else {
                      out <- data.frame(cost = 2^runif(len, min = -5, max = 10),
                                        weight = runif(len, min = 1, max = 25))
                    }
                    out
                  }, 
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(length(levels(y)) != 2)
                      stop("Currently implemented for 2-class problems")
                    cwts <- c(1, param$weight)
                    names(cwts) <- levels(y)
                    out <- e1071::svm(x = as.matrix(x), y = y,
                                      kernel = "linear",
                                      cost = param$cost,
                                      probability = classProbs,
                                      class.weights = cwts,
                                      ...)

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
                  tags = c("Kernel Method", "Support Vector Machines","Linear Classifier",
                           "Robust Methods", "Cost Sensitive Learning", "Two Class Only"),
                  levels = function(x) x$levels,
                  sort = function(x) {
                    # If the cost is high, the decision boundary will work hard to
                    # adapt. Also, if C is fixed, smaller values of sigma yeild more
                    # complex boundaries
                    x[order(x$cost, x$weight),]
                  })
