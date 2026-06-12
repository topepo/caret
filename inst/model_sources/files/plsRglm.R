modelInfo <- list(label = "Partial Least Squares Generalized Linear Models ",
                  library = "plsRglm",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c('nt', 'alpha.pvals.expli'),
                                          class = c("numeric", "numeric"),
                                          label = c('#PLS Components', 'p-Value threshold')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <-  expand.grid(nt = 1:len, 
                                          alpha.pvals.expli = 10^(c(-2:(len-3), 0)))
                    } else {
                      out <- data.frame(nt = sample(1:ncol(x), size = len, replace = TRUE), 
                                        alpha.pvals.expli = runif(len, min = 0, .2))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    require(plsRglm)
                    if(is.factor(y)) {
                      lv <- levels(y)
                      y <- as.numeric(y)  - 1
                      dst <- "pls-glm-logistic"
                    } else {
                      lv <- NULL
                      dst <- "pls-glm-gaussian"
                    }
                    
                    ## Intercept parameters if needed
                    theDots <- list(...)
                    if(any(names(theDots) == "modele")) {
                      mod <- plsrRglm::plsRglm(y, x, 
                                     	       nt = param$nt, 
                                               pvals.expli = param$alpha.pvals.expli < 1,
                                               sparse  = param$alpha.pvals.expli < 1,
                                               alpha.pvals.expli = param$alpha.pvals.expli,
                                               ...)
                    } else {
                      mod <- plsRglm::plsRglm(y, x, 
                                              nt = param$nt, 
                                              modele = dst, 
                                              pvals.expli = param$alpha.pvals.expli < 1,
                                              sparse  = param$alpha.pvals.expli < 1,
                                              alpha.pvals.expli = param$alpha.pvals.expli,
                                              ...)
                    }
                    mod
                  },
                  predict = function(modelFit, newdata, submodels = NULL)
                  {
                    out <- predict(modelFit, newdata, type="response")
                    if(modelFit$problemType == "Classification")
                    {
                      out <- factor(ifelse(out >= .5, 
                                           modelFit$obsLevels[2], 
                                           modelFit$obsLevels[1]))
                    } 
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, newdata, type="response")
                    out <- cbind(1 - out, out)
                    dimnames(out)[[2]] <-  rev(modelFit$obsLevels)
                    out
                  },
                  varImp = NULL,
                  predictors = function(x, ...) {
                    vars <- names(which(coef(x)[[2]][,1] != 0))
                    vars[vars != "Intercept"]
                  },
                  notes = paste(
                    "Unlike other packages used by `train`, the `plsRglm`",
                    "package is fully loaded when this model is used."
                  ),
                  tags = c("Generalized Linear Models", 
                           "Partial Least Squares", "Two Class Only"),
                  levels = function(x) x$lev,
                  sort = function(x) x[order(-x$alpha.pvals.expli, x$nt),])
