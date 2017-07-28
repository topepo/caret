modelInfo <- list(label = "Optimal Weighted Nearest Neighbor Classifier",
                  library = "snn",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = "K",
                                          class = "numeric",
                                          label = "#Neighbors"),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- data.frame(K = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0])
                    } else {
                      by_val <- if(is.factor(y)) length(levels(y)) else 1
                      out <- data.frame(K = sample(seq(1, floor(nrow(x)/3), by = by_val), size = len, replace = TRUE))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.matrix(x)) x <- as.matrix(x)
                    if(!(class(x[1,1]) %in% c("integer", "numeric")))
                      stop("predictors should be all numeric")
                    x <- cbind(x, as.numeric(y))
                    colnames(x)[ncol(x)] <- ".outcome"
                    list(dat = x, K = param$K)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- snn::myownn(train = modelFit$dat,
                                       test = newdata,
                                       K = modelFit$K)
                    modelFit$obsLevels[out]
                  },
                  predictors = function(x, ...) x$xNames,
                  tags = "Prototype Models",
                  prob = NULL,
                  levels = function(x) x$obsLevels,
                  sort = function(x) x[order(-x[,1]),])
