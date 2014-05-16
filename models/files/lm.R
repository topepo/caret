modelInfo <- list(label = "Linear Regression",
                  library = NULL,
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    if(!is.null(wts))
                    {
                      out <- lm(.outcome ~ ., data = dat, weights = wts, ...)
                    } else out <- lm(.outcome ~ ., data = dat, ...)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) predict(modelFit, newdata),
                  prob = NULL,
                  predictors = function(x, ...) predictors(x$terms),
                  tags = "Linear Regression",
                  varImp = function(object, ...) {
                    values <- summary(object)$coef
                    varImps <-  abs(values[-1, grep("value$", colnames(values))])
                    out <- data.frame(varImps)
                    colnames(out) <- "Overall"
                    if(!is.null(names(varImps))) rownames(out) <- names(varImps)
                    out   
                  },
                  sort = function(x) x)
