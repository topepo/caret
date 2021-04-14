modelInfo <- list(label = "Multi-Step Adaptive MCP-Net",
                  library = "msaenet",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('alphas', 'nsteps', "scale"),
                                          class = c("numeric", "numeric", "numeric"),
                                          label = c('Alpha', '#Adaptive Estimation Steps',
                                                    "Adaptive Weight Scaling Factor")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(alphas = seq(0.05, 0.95, length = len),
                                         nsteps = 2:(len+1),
                                         scale  = 2:(len+1))
                    } else {
                      out <- data.frame(alphas = runif(len, min = 0.05, max = 0.95),
                                        nsteps = sample(2:10, size = len, replace = TRUE),
                                        scale = runif(len, min = .25, max = 4))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(length(levels(y)) > 2)
                      stop("Only two class problems are supported by `msamnet`", call. = FALSE)

                    theDots <- list(...)
                    
                    if(all(names(theDots) != "family")) {
                      if(length(levels(y)) > 0) {
                        fam <- "binomial"
                      } else fam <- "gaussian"    
                      theDots$family <- fam   
                    }
                    # we will only send a single alpha/nsteps combo to the 
                    # function so avoid an extra meaningless cv loop
                    if(all(names(theDots) != "tune")) 
                      theDots$tune <- "aic"
                    if(all(names(theDots) != "tune.nsteps")) 
                      theDots$tune.nsteps <- "aic"                                    
                    
                    if(!is.matrix(x))
                      x <- as.matrix(x)
                    
                    modelArgs <- c(list(x = x,
                                        y = y,
                                        alphas = param$alphas,
                                        nsteps = param$nsteps,
                                        scale = param$scale),
                                   theDots)
                    
                    do.call(getFromNamespace("msamnet", "msaenet"), modelArgs) 
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- msaenet:::predict.msaenet(modelFit, newdata, type = "response")
                    if(modelFit$model$family == "binomial") {
                      out <- ifelse(out > .4, modelFit$obsLevels[2], modelFit$obsLevels[1])
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- msaenet:::predict.msaenet(modelFit, newdata, type = "response")
                    out <- as.data.frame(cbind(1-out, out), stringsAsFactors = TRUE)
                    colnames(out) <- modelFit$obsLevels
                    out
                  },
                  predictors = function(x, ...) {
                    coefs <- msaenet:::predict.msaenet(x, newx = NULL, type = "coefficients")
                    coefs <- rownames(coefs)[coefs != 0]
                    coefs <- coefs[coefs != "(Intercept)"]
                    coefs
                  },
                  varImp = function(object, ...) {
                    coefs <- msaenet:::predict.msaenet(object, newx = NULL, type = "coefficients")
                    coefs <- abs(coefs[rownames(coefs) != "(Intercept)",,drop = FALSE])
                    colnames(coefs) <- "Overall"
                    coefs
                  },
                  levels = function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL,
                  tags = c("Generalized Linear Model", "Implicit Feature Selection", 
                           "L1 Regularization", "L2 Regularization", "Linear Classifier",
                           "Linear Regression"),
                  sort = function(x) x[order(x$alphas, x$nsteps),],
                  trim = function(x) {
                    x$call <- NULL
                    x
                  })
