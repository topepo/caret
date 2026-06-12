modelInfo <- list(label = "Distance Weighted Discrimination with Polynomial Kernel",
                  library = "kerndwd",
                  type = "Classification",
                  parameters = data.frame(parameter = c('lambda', "qval", 'degree', 'scale'),
                                          class = rep("numeric", 4),
                                          label = c('Regularization Parameter', 'q', 'Polynomial Degree','Scale')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(length(levels(y)) != 2) stop("Two class problems only")
                    if(search == "grid") {
                      out <-  expand.grid(lambda = 10^seq(-5, 1, length = len),
                                          qval = 1,
                                          degree = seq(1, min(len, 3)),      
                                          scale = 10 ^((1:len) - 4))
                    } else {
                      out <- data.frame(lambda = 10^runif(len, min = -5, 1),
                                        qval = runif(len, min = 0, 3),
                                        degree = sample(1:3, size = len, replace = TRUE),
                                        scale = 10^runif(len, min = -5, 0))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.matrix(x)) x <- as.matrix(x)
                    kobj <- kernlab::polydot(degree = param$degree, scale = param$scale, offset = 1)
                    out <- kerndwd::kerndwd(x = x,
                                            y = ifelse(y == lev[1], 1, -1),
                                            qval = param$qval,
                                            lambda = param$lambda,
                                            kern = kobj,
                                            ...)
                    out$kern <- kobj
                    out$x <- x
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- predict(object = modelFit,
                                   newx = newdata,
                                   kern = modelFit$kern,
                                   x = modelFit$x)[,1]
                    ifelse(out == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])        
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- predict(object = modelFit,
                                   newx = newdata,
                                   kern = modelFit$kern,
                                   x = modelFit$x, type = "link")[,1]
                    out <- binomial()$linkinv(out)
                    out <- cbind(out, 1 - out)
                    colnames(out) <- modelFit$obsLevels
                    out
                  },
                  levels = function(x) x$obsLevels,
                  predictors = function(x, s = NULL, ...) x$xNames,
                  tags = c("Discriminant Analysis", "L2 Regularization", 
                           "Kernel Method", "Polynomial Model",
                           "Distance Weighted Discrimination", "Two Class Only"),
                  sort = function(x) x[order(x[,1]),])
