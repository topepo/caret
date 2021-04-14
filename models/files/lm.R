modelInfo <- list(label = "Linear Regression",
                  library = NULL,
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = "intercept",
                                          class = "logical",
                                          label = "intercept"),
                  grid = function(x, y, len = NULL, search = "grid") 
                    data.frame(intercept = TRUE),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    if(!is.null(wts))
                    {
                      if (param$intercept)
                        out <- lm(.outcome ~ ., data = dat, weights = wts, ...)
                      else
                        out <- lm(.outcome ~ 0 + ., data = dat, weights = wts, ...)
                    } else 
                    {
                      if (param$intercept)
                        out <- lm(.outcome ~ ., data = dat, ...)
                      else
                        out <- lm(.outcome ~ 0 + ., data = dat, ...)
                    }
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)
                  },
                  prob = NULL,
                  predictors = function(x, ...) predictors(x$terms),
                  tags = c("Linear Regression", "Accepts Case Weights"),
                  varImp = function(object, ...) {
                    values <- summary(object)$coef
                    varImps <-  abs(values[ !grepl( rownames(values), pattern = 'Intercept' ), 
                                            grep("value$", colnames(values)), drop = FALSE]) 
                    out <- data.frame(varImps)
                    colnames(out) <- "Overall"
                    if(!is.null(names(varImps))) rownames(out) <- names(varImps)
                    out   
                  },
                  sort = function(x) x)
