modelInfo <- list(label = "Non-Negative Least Squares",
                  library = "nnls",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.matrix(x)) x <- as.matrix(x)
                    out <- nnls::nnls(x, y)
                    names(out$x) <- colnames(x)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- newdata %*% modelFit$x
                    out[,1]
                    },
                  prob = NULL,
                  predictors = function(x, ...) names(x$x)[x$x != 0],
                  tags = "Linear Regression",
                  varImp = function(object, ...) {
                    out <- data.frame(Overall = object$x)
                    rownames(out) <- names(object$x)
                    out
                    out   
                  },
                  sort = function(x) x)
