modelInfo <- list(label = "Monotone Multi-Layer Perceptron Neural Network",
                  library = "monmlp",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c('hidden1', 'n.ensemble'),
                                          class = rep('numeric', 2),
                                          label = c('#Hidden Units', "#Models")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(hidden1 = ((1:len) * 2) - 1, 
                                         n.ensemble = 1)
                    } else {
                      out <- data.frame(hidden1 = sample(2:20, replace = TRUE, size = len),
                                        n.ensemble = sample(1:10, replace = TRUE, size = len))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.matrix(x)) x <- as.matrix(x)
                    y <- if(is.numeric(y) )  
                      matrix(y, ncol = 1) else
                        class2ind(y)
                    out <- monmlp::monmlp.fit(y = y,
                                              x = x,
                                              hidden1 = param$hidden1,
                                              n.ensemble = param$n.ensemble,
                                              ...)
                    list(model = out)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- monmlp::monmlp.predict(newdata, modelFit$model)
                    if(modelFit$problemType == "Classification") {
                      out <- modelFit$obsLevels[apply(out, 1, which.max)]
                    } else out <- out[, 1]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- monmlp::monmlp.predict(newdata, modelFit$model)
                    out <- t(apply(out, 1, function(x) exp(x)/sum(exp(x))))
                    colnames(out) <- modelFit$obsLevels
                    out
                  },
                  predictors = function(x, ...) {
                    colnames(attr(x$model, "x"))
                  },
                  varImp = NULL,
                  levels = function(x) {
                    y <- attr(x$model, "y")
                    if(is.matrix(y) && ncol >= 2) 
                      colnames(colnames) else NULL
                  },
                  oob = NULL,
                  tags = "Neural Network",
                  sort = function(x) x[order(x[,1]),])
