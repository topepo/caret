modelInfo <- list(label = "k-Nearest Neighbors",
                  library = NULL,
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = "k",
                                          class = "numeric",
                                          label = "#Neighbors"),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- data.frame(k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0])
                    } else {
                      by_val <- if(is.factor(y)) length(levels(y)) else 1
                      out <- data.frame(k = sample(seq(1, floor(nrow(x)/3), by = by_val), size = len, replace = TRUE))
                    }
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(is.factor(y))
                    {
                      knn3(as.matrix(x), y, k = param$k, ...)
                    } else {
                      knnreg(as.matrix(x), y, k = param$k, ...)
                    }
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(modelFit$problemType == "Classification")
                    {
                      out <- predict(modelFit, newdata,  type = "class")
                    } else {
                      out <- predict(modelFit, newdata)
                    }
                    out
                  },
                  predictors = function(x, ...) colnames(x$learn$X),
                  tags = "Prototype Models",
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "prob"),
                  levels = function(x) levels(x$learn$y),
                  sort = function(x) x[order(-x[,1]),])
