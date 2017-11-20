modelInfo <- list(label = "Regularized Logistic Regression",
                  library = "LiblineaR",
                  type = c ("Classification"),
                  parameters = data.frame(parameter = c('cost', "loss", 'epsilon'),
                                          class = c("numeric", "character", "numeric"),
                                          label = c("Cost", "Loss Function", "Tolerance" )),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(cost = 2 ^((1:len)- ceiling(len*0.5)),
                                         loss = c("L1", "L2_dual", "L2_primal"),
                                         epsilon = signif(0.01 * (10^((1:len) - ceiling(len*0.5))), 2) )
                    } else {
                      out <- data.frame(cost = 2^runif(len, min = -10, max = 10),
                                        loss = sample(c("L1", "L2_dual", "L2_primal"), size = len, replace = TRUE),
                                        epsilon = 1^runif(len, min=-10, max= 10))
                    }
                    out
                  }, 
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    
                    if( !(param$loss %in% c( "L1", "L2_dual", "L2_primal")) ) {
                      stop("Loss function is not recognised.", call. = FALSE)
                    }
                    if(!is.factor(y)) {
                      stop('y is not recognised as a factor', call. = FALSE)
                    }
                    model_type <-
                      ifelse(param$loss == "L1", 6, ifelse(param$loss == "L2_primal", 0, 7))
                    out <- LiblineaR::LiblineaR(
                      data = as.matrix(x), 
                      target = y,
                      cost = param$cost, 
                      epsilon = param$epsilon,
                      type = model_type,
                      ...
                    )
                    
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    LiblineaR:::predict.LiblineaR(modelFit, newdata)$predictions
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    LiblineaR:::predict.LiblineaR(modelFit,newdata, proba = TRUE)$probabilities
                  },
                  predictors = function(x, ...) { 
                    out <- colnames(x$W)
                    out[out != "Bias"]
                  },
                  tags = c("Linear Classifier", "Robust Methods", "L1 Regularization", "L2 Regularization"),
                  levels = function(x) x$levels,
                  sort = function(x) {
                    x[order(x$cost),]
                  })
