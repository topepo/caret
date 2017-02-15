modelInfo <- list(label = "Regularized Logistic Regression",
                  library = "LiblineaR",
                  type = c ("Classification"),
                  parameters = data.frame(parameter = c('cost', "loss", 'epsilon'),
                                          class = c("numeric", "character", "numeric"),
                                          label = c("Cost", "Loss Function", "Tolerance" )),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(cost = 2 ^((1:len) - 3),
                                         loss = c("L1", "L2"),
                                         epsilon = 0.001 * (10^(1:len)) )
                    } else {
                      out <- data.frame(cost = 2^runif(len, min = -10, max = 10),
                                        loss = sample(c("L1", "L2"), size = len, replace = TRUE),
                                        epsilon = 1^runif(len, min=-10, max= 10))
                    }
                    out
                  }, 
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    model_type = ifelse(param$loss == "L2", 0, ifelse(param$loss == "L1", 6, 
                                                                      stop("Loss function is not recognised.")))
                   
                  if(!is.factor(y)) {
                      stop('y is not recognised as a factor')
                  }
                    out <- LiblineaR(data = as.matrix(x), target = y,
                                     cost = param$cost,
                                     type = model_type,
                                     ...)

                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata)$predictions
                  },
                  prob = NULL,
                  predictors = function(x, ...) { 
                    out <- colnames(x$W)
                    out[out != "Bias"]
                    },
                  tags = c("Linear Regression", "Linear Classifier", "Robust Methods"),
                  levels = function(x) x$levels,
                  sort = function(x) {
                    x[order(x$cost),]
                  })
