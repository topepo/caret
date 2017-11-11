modelInfo <- list(label = "Penalized Logistic Regression",
                  library = "stepPlr",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('lambda', 'cp'),
                                          class = c('numeric', 'character'),
                                          label = c('L2 Penalty', 'Complexity Parameter')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <-  expand.grid(cp = "bic", 
                                          lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)))
                    } else {
                      out <- data.frame(cp = sample(c("aic", "bic"), size = len, replace = TRUE), 
                                        lambda = 10^runif(len, min = -5, 1))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...){
                    if(!is.matrix(x)) 
                      x <- as.matrix(x)
                    y <- ifelse(y == levels(y)[1], 1, 0)
                    stepPlr::plr(x, y,
                                 lambda = param$lambda,
                                 cp = as.character(param$cp),
                                 weights = if(!is.null(wts)) wts else rep(1,length(y)), 
                                 ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL)  {
                    ifelse(stepPlr::predict.plr(modelFit, as.matrix(newdata), type = "class") == 1,
                           modelFit$obsLevels[1],
                           modelFit$obsLevels[2])
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- stepPlr::predict.plr(modelFit, as.matrix(newdata), type = "response")
                    out <- cbind(out, 1-out)
                    dimnames(out)[[2]] <-  modelFit$obsLevels
                    out
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("L2 Regularization", "Logistic Regression", "Linear Classifier"),
                  sort = function(x) x[order(-x$lambda),])
