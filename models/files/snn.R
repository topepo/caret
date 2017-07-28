modelInfo <- list(label = "Stabilized Nearest Neighbor Classifier",
                  library = "snn",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = "lambda",
                                          class = "numeric",
                                          label = "Stabilization Parameter"),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(lambda = c(0, 2 ^ seq(-5, 5, length = len - 1)))
                    } else {
                      out <- data.frame(lambda = 2^runif(len, min = -5, 5))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.matrix(x)) x <- as.matrix(x)
                    if(!(class(x[1,1]) %in% c("integer", "numeric")))
                      stop("predictors should be all numeric")
                    x <- cbind(x, as.numeric(y))
                    colnames(x)[ncol(x)] <- ".outcome"
                    list(dat = x, lambda = param$lambda)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- snn::mysnn(train = modelFit$dat,
                                      test = newdata,
                                      lambda = modelFit$lambda)
                    modelFit$obsLevels[out]
                  },
                  predictors = function(x, ...) x$xNames,
                  tags = "Prototype Models",
                  prob = NULL,
                  levels = function(x) x$obsLevels,
                  sort = function(x) x[order(-x[,1]),])
