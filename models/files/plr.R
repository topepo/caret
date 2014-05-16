modelInfo <- list(label = "Penalized Logistic Regression",
                  library = "stepPlr",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('lambda', 'cp'),
                                          class = c('numeric', 'character'),
                                          label = c('L2 Penalty', 'Complexity Parameter')),
                  grid = function(x, y, len = NULL) expand.grid(cp = "bic", 
                                                                lambda = c(0, 10 ^ seq(-1, -4, length = len - 1))),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...){
                    y <- ifelse(y == levels(y)[1], 1, 0)
                    plr(x, y,
                        lambda = param$lambda,
                        cp = as.character(param$cp),
                        weights = if(!is.null(wts)) wts else rep(1,length(y)), 
                        ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL)  {
                    ifelse(predict(modelFit, as.matrix(newdata), type = "class") == 1,
                           modelFit$obsLevels[1],
                           modelFit$obsLevels[2])
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, type = "response")
                    out <- cbind(out, 1-out)
                    dimnames(out)[[2]] <-  modelFit$obsLevels
                    out
                  },
                  tags = c("L2 Regularization", "Logistic Regression", "Linear Classifier"),
                  sort = function(x) x[order(-x$lambda),])
