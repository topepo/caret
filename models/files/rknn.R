modelInfo <- list(label = "Random k-Nearest Neighbors",
                  library = "rknn",
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("k", "mtry"),
                                          class = rep("numeric", 2),
                                          label = c("#Neighbors", "#Randomly Selected Predictors")),
                  grid = function(x, y, len = NULL) {
                    expand.grid(mtry = caret::var_seq(p = ncol(x), 
                                               classification = is.factor(y), 
                                               len = len),
                                k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0])
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    out <- list(data = x, y = y, mtry = param$mtry, k = param$k)
                    theDots = list(...)
                    if(length(theDots) > 0) out <- c(out, theDots)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    modelFit$xNames <- NULL
                    modelFit$problemType <- NULL
                    modelFit$tuneValue <- NULL
                    modelFit$obsLevels <- NULL
                    modelFit$newdata <- newdata
                    if(!is.factor(modelFit$y)) {
                      out <- do.call("rknnReg", modelFit)$pred
                    } else {
                      out <- as.character(do.call("rknn", modelFit)$pred)
                    } 
                    out
                  },
                  levels = function(x) x$obsLevels,
                  prob = NULL,
                  predictors = function(x, s = NULL, ...) {
                    modelFit$xNames
                  },
                  tags = "Prototype Models",
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
