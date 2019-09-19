modelInfo <- list(label = "Random k-Nearest Neighbors with Feature Selection",
                  library = c("rknn", "plyr"),
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("k", "mtry", "d"),
                                          class = rep("numeric", 3),
                                          label = c("#Neighbors",
                                                    "#Randomly Selected Predictors",
                                                    "#Features Dropped")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      grid <- expand.grid(k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0],
                                          d = seq(.2, .8, length = len))
                    } else {
                      by_val <- if(is.factor(y)) length(levels(y)) else 1
                      grid <- data.frame(d = runif(len, min = 0, max = 1),
                                         k = sample(seq(1, floor(nrow(x)/3), by = by_val), size = len, replace = TRUE))
                    }
                    grid$d <- floor(grid$d*ncol(x))
                    grid$d[grid$d == 0] <- 1
                    newp <- ncol(x) - grid$d
                    grid$mtry <- if (!is.null(y) && !is.factor(y))
                      pmax(floor(newp/3), 1) else floor(sqrt(newp))
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(param$d >= ncol(x)) {
                      warning(paste("'d' was set at ", param$d,
                                    " but there are", ncol(x),
                                    "predictors. 'd' was reset to ",
                                    ncol(x) - 1, sep = ""))
                      param$d <- ncol(x) - 1
                    }
                    out <- list(data = x, y = y,
                                mtry = min(param$mtry, ncol(x) - param$d),
                                k = param$k,
                                d = param$d)
                    if(out$mtry != param$mtry) {
                      warning(paste("'mtry' was set at ", param$mtry,
                                    " with only ", ncol(x) - param$d,
                                     "columns after selection.",
                                    " 'mtry' was reset to ",
                                    out$mtry, sep = ""))
                    }
                    theDots = list(...)
                    if(length(theDots) > 0) out <- c(out, theDots)
                    mod <- do.call(rknn::rknnBel, out)
                    out$vars <- bestset(mod, criterion = "mean_accuracy")
                    out$model <- mod
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    modelFit$xNames <- NULL
                    modelFit$problemType <- NULL
                    modelFit$tuneValue <- NULL
                    modelFit$obsLevels <- NULL
                    modelFit$newdata <- newdata[,modelFit$vars,drop = FALSE]
                    modelFit$data <- modelFit$data[,modelFit$vars,drop = FALSE]
                    modelFit$vars <- NULL
                    modelFit$model <- NULL
                    modelFit$d <- NULL
                    if(!is.factor(modelFit$y)) {
                      out <- do.call(rknn::rknnReg, modelFit)$pred
                    } else {
                      out <- as.character(do.call(rknn::rknn, modelFit)$pred)
                    }
                    out
                  },
                  prob = NULL,
                  levels = function(x) x$obsLevels,
                  predictors = function(x, s = NULL, ...) {
                    bestset(x$mod, criterion = "mean_accuracy")
                  },
                  varImp = function(object, ...){
                    allVars <- object$xNames
                    imp <- object$mod$vars[[which.max(object$mod$mean_accuracy)]]
                    imp <- data.frame(Overall = imp)
                    used <- rownames(imp)
                    unused <- allVars[!(allVars %in% used)]
                    if(length(unused) > 0) {
                      imp0 <- data.frame(Overall = rep(0, length(unused)))
                      rownames(imp0) <- unused
                      imp <- rbind(imp, imp0)
                    }
                    imp
                  },
                  tags = c("Prototype Models", "Feature Selection Wrapper", "Two Class Only"),
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
