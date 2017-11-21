modelInfo <- list(label = "Random k-Nearest Neighbors",
                  library = "rknn",
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("k", "mtry"),
                                          class = rep("numeric", 2),
                                          label = c("#Neighbors", "#Randomly Selected Predictors")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(mtry = caret::var_seq(p = ncol(x), 
                                                               classification = is.factor(y), 
                                                               len = len),
                                         k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0])
                    } else {
                      by_val <- if(is.factor(y)) length(levels(y)) else 1
                      out <- data.frame(mtry = sample(1:ncol(x), size = len, replace = TRUE),
                                        k = sample(seq(1, floor(nrow(x)/3), by = by_val), size = len, replace = TRUE))
                    }
                    out
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
                      out <- do.call(rknn::rknnReg, modelFit)$pred
                    } else {
                      out <- as.character(do.call(rknn::rknn, modelFit)$pred)
                    }
                    out
                  },
                  levels = function(x) x$obsLevels,
                  prob = NULL,
                  predictors = function(x, s = NULL, ...) {
                    modelFit$xNames
                  },
                  tags = c("Prototype Models", "Two Class Only"),
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
