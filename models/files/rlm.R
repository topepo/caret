modelInfo <- list(label = "Robust Linear Model",
                  library = "MASS",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x)
                    dat$.outcome <- y
                    if(!is.null(wts)) {
                      out <- rlm(.outcome ~ ., data = dat, weights = wts, ...)
                    } else out <- rlm(.outcome ~ ., data = dat, ...)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata)
                    },
                  prob = NULL,
                  tags = c("Linear Regression", "Robust Model", "Accepts Case Weights"),
                  sort = function(x) x)
