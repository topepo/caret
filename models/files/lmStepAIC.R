modelInfo <- list(label = "Linear Regression with Stepwise Selection",
                  library = "MASS",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    if(!is.null(wts))
                    {
                      out <- MASS::stepAIC(lm(.outcome ~ ., data = dat, weights = wts), ...)
                    } else out <- MASS::stepAIC(lm(.outcome ~ ., data = dat), ...)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)
                    },
                  prob = NULL,
                  tags = c("Linear Regression", "Feature Selection Wrapper", "Accepts Case Weights"),
                  sort = function(x) x)
