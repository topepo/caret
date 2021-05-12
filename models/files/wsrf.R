modelInfo <- list(label = "Weighted Subspace Random Forest",
                  library = "wsrf",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = "mtry",
                                          class = "numeric",
                                          label = "#Randomly Selected Predictors"),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(mtry = caret::var_seq(p = ncol(x),
                                                              classification = is.factor(y),
                                                              len = len))
                    } else {
                      out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, replace = TRUE)))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    wsrf::wsrf(.outcome ~ ., data = dat, mtry = min(param$mtry, ncol(x)), ...)
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)$class
                    },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata, type = "prob")$prob
                    },
                  predictors = function(x, ...) x$xNames,
                  varImp = NULL,
                  levels = function(x) x$obsLevels,
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),])
