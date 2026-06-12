modelInfo <- list(label = "Nearest Shrunken Centroids",
                  library = "pamr",
                  type = "Classification",
                  parameters = data.frame(parameter = 'threshold',
                                          class = "numeric",
                                          label = 'Shrinkage Threshold'),
                  grid = function(x, y, len = NULL, search = "grid") {
                    cc <- complete.cases(x) & complete.cases(y)
                    x <- x[cc,,drop = FALSE]
                    y <- y[cc]
                    initialThresh <- pamr::pamr.train(list(x=t(x), y=y))$threshold
                    initialThresh <- initialThresh[-c(1, length(initialThresh))]
                    if(search == "grid") {
                      out <- data.frame(threshold = 
                                          seq(from = min(initialThresh),
                                              to = max(initialThresh), 
                                              length = len))
                    } else {
                      out <- data.frame(threshold = 
                                          runif(len, 
                                                min = min(initialThresh),
                                                max = max(initialThresh)))
                    }
                    out
                    
                  },
                  loop = function(grid) {   
                    grid <- grid[order(grid$threshold, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])       
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    res <- pamr::pamr.train(list(x = t(x), y = y), 
                                            threshold = param$threshold, ...)
                    if (last) {
                      res$xData <- x
                      res$yData <- y
                    }
                    res
                  }, 
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- pamr::pamr.predict(modelFit,
                                              t(newdata),
                                              threshold = modelFit$tuneValue$threshold)
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      for(j in seq(along = submodels$threshold)) {
                        tmp[[j+1]] <- pamr::pamr.predict(modelFit,
                                                         t(newdata),
                                                         threshold = submodels$threshold[j])
                      }
                      out <- tmp
                    }
                    out         
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- pamr::pamr.predict(modelFit, t(newdata),
                                              threshold = modelFit$tuneValue$threshold,
                                              type= "posterior")
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$threshold)) {
                        tmpProb <-  pamr::pamr.predict(modelFit, t(newdata),
                                                       threshold =  submodels$threshold[j],
                                                       type= "posterior")
                        tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, drop = FALSE], stringsAsFactors = TRUE)
                      }
                      out <- tmp
                    }   
                    out
                  },
                  predictors = function(x, newdata = NULL, threshold = NULL,  ...) {
                    if (is.null(newdata)) {
                      if (!is.null(x$xData))
                        newdata <- x$xData
                      else
                        stop("must supply newdata")
                    }
                    if (is.null(threshold)) {
                      if (!is.null(x$threshold))
                        threshold <-
                          x$threshold
                      else
                        stop("must supply threshold")
                    }
                    varIndex <- pamr::pamr.predict(x, 
                                                   newx = newdata, 
                                                   threshold = threshold, 
                                                   type = "nonzero")
                    colnames(newdata)[varIndex]
                  },
                  varImp = function (object, threshold = NULL, data = NULL, ...) {
                    if(is.null(data)) 
                      data <- object$xData
                    if(is.null(threshold)) 
                      threshold <- object$tuneValue$threshold
                    if( dim(object$centroids)[1] != dim(data)[2]) 
                      stop("the number of columns (=variables) is not consistent with the pamr object")
                    
                    if(is.null(dimnames(data)))  {
                      featureNames <- paste("Feature", seq(along = data[1,]), sep = "")
                      colnames(data) <- featureNames
                    } else featureNames <- dimnames(data)[[2]]
                    
                    x <- t(data)
                    retainedX <- x[object$gene.subset, object$sample.subset, drop = F]
                    centroids <- pamr::pamr.predict(object, x, threshold = threshold, type = "cent")
                    standCentroids <- (centroids - object$centroid.overall)/object$sd
                    rownames(standCentroids) <- featureNames
                    colnames(standCentroids) <- names(object$prior)
                    as.data.frame(standCentroids, stringsAsFactors = TRUE)
                  },
                  levels = function(x) names(x$prior),
                  tags = c("Prototype Models", "Implicit Feature Selection", "Linear Classifier"),
                  sort = function(x) x[order(x[,1]),])
