modelInfo <- list(label = "Sparse Distance Weighted Discrimination",
                  library = "sdwd",
                  type = "Classification",
                  parameters = data.frame(parameter = c('lambda', 'lambda2'),
                                          class = c("numeric", "numeric"),
                                          label = c('L1 Penalty', 'L2 Penalty')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    lev <- levels(y)
                    y <- ifelse(y == lev[1], 1, -1)
                    init <- sdwd::sdwd(as.matrix(x), y,
                                       nlambda = len + 2,
                                       lambda2 = 0)
                    lambda <- unique(init$lambda)
                    lambda <- lambda[-c(1, length(lambda))]

                    if(search == "grid") {
                      lambda <- lambda[1:min(length(lambda), len)]
                      out <- expand.grid(lambda = lambda,
                                         lambda2 = seq(0.1, 1, length = len))
                    } else {
                      out <- data.frame(lambda = runif(len, min = min(lambda), max(lambda)),
                                        lambda2 = 10^runif(len, min = -5, 0))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    y <- ifelse(y == lev[1], 1, -1)
                    sdwd::sdwd(as.matrix(x), y = y,
                               lambda = param$lambda,
                               lambda2 = param$lambda2, 
                               ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- predict(modelFit, newx = newdata, type = "class")
                    ifelse(out == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- predict(modelFit, newx = newdata, type = "link")
                    out <- binomial()$linkinv(out)
                    out <- data.frame(c1 = out, c2 = 1 - out)
                    colnames(out) <- modelFit$obsLevels
                    out
                  },
                  predictors = function(x, ...) {
                    out <- apply(x$beta, 1, function(x) any(x != 0))
                    names(out)[out]
                  },
                  varImp = function(object, lambda = NULL, ...) {
                    out <- as.data.frame(as.matrix(abs(object$beta)), stringsAsFactors = TRUE)
                    colnames(out) <- "Overall"
                    out
                  },
                  levels = function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL,
                  tags = c("Discriminant Analysis Models", "Implicit Feature Selection", 
                           "L1 Regularization", "L2 Regularization", "Linear Classifier",
                           "Distance Weighted Discrimination"),
                  sort = function(x) x[order(-x$lambda, -x$lambda2),],
                  trim = function(x) {
                    x$call <- NULL
                    x
                  })
