modelInfo <- list(label = "L2 Regularized Linear Support Vector Machines with Class Weights",
                  library = "LiblineaR",
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('cost', "Loss", "weight"),
                                          class = c("numeric", "character", "numeric"),
                                          label = c("Cost", "Loss Function", 'Class Weight')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(cost = 2 ^((1:len) - 3),
                                         Loss = c("L1", "L2"),
                                         weight = 1:len)
                    } else {
                      out <- data.frame(cost = 2^runif(len, min = -10, max = 10),
                                        Loss = sample(c("L1", "L2"), size = len, replace = TRUE),
                                        weight = runif(len, min = 1, max = 25))
                    }
                    out
                  }, 
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    # LiblineaR lists the following models 
                    # 
                    # 2 - L2-regularized L2-loss support vector classification (primal) 
                    # 3 - L2-regularized L1-loss support vector classification (dual) 
                    model_type <- if(param$Loss == "L2") 2 else 3
                    if(length(levels(y)) != 2)
                      stop("Currently implemented for 2-class problems")
                    cwts <- c(1, param$weight)
                    names(cwts) <- levels(y)

                    out <- LiblineaR::LiblineaR(data = as.matrix(x), target = y,
                                                cost = param$cost,
                                                type = model_type,
                                                wi = cwts,
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
                  tags = c("Kernel Method", "Support Vector Machines","Linear Classifier",
                           "Robust Methods", "Cost Sensitive Learning", "Two Class Only"),
                  levels = function(x) x$levels,
                  sort = function(x) {
                    x[order(x$cost),]
                  })
