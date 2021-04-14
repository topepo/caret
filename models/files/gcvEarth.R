modelInfo <- list(label = "Multivariate Adaptive Regression Splines",
                  library = "earth",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('degree'),
                                          class = c("numeric"),
                                          label = c('Product Degree')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    data.frame(degree = 1)
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    require(earth)
                    theDots <- list(...)
                    theDots$keepxy <- TRUE 
                    
                    ## pass in any model weights
                    if (!is.null(wts)) theDots$weights <- wts
                    
                    modelArgs <- c(list(x = x, y = y, degree = param$degree), theDots)
                    
                    if (is.factor(y) & !any(names(theDots) == "glm")) {
                      modelArgs$glm <- list(family = binomial, maxit = 100)
                    }
                    
                    tmp <- do.call(earth::earth, modelArgs)
                    
                    tmp$call["degree"] <-  param$degree
                    tmp 
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if (modelFit$problemType == "Classification") {
                      out <- earth:::predict.earth(modelFit, newdata, type = "class")
                    } else {
                      out <- earth:::predict.earth(modelFit, newdata)
                    }
                    as.vector(out)            
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- earth:::predict.earth(modelFit, newdata, type = "response")
                    if (ncol(out) > 1) {
                      out <- t(apply(out, 1, function(x) x / sum(x)))
                    } else {
                      out <- cbind(1 - out[, 1], out[, 1])
                      colnames(out) <- modelFit$obsLevels
                    }
                    as.data.frame(out, stringsAsFactors = TRUE)
                  },
                  predictors = function(x, ...) {
                    vi <- varImp(x)
                    notZero <- sort(unique(unlist(lapply(vi, function(x) which(x > 0)))))
                    if (length(notZero) > 0) rownames(vi)[notZero] else NULL
                  },
                  varImp = function(object, value = "gcv", ...) {
                    earthImp <- earth::evimp(object)
                    if (!is.matrix(earthImp)) earthImp <- t(as.matrix(earthImp))
                    
                    # get other variable names and padd with zeros
                    
                    out <- earthImp
                    perfCol <- which(colnames(out) == value)
                    
                    increaseInd <- out[,perfCol + 1]
                    out <- as.data.frame(out[,perfCol, drop = FALSE], stringsAsFactors = TRUE)  
                    colnames(out) <- "Overall"
                    
                    # At this point, we still may have some variables
                    # that are not in the model but have non-zero
                    # importance. We'll set those to zero
                    if (any(earthImp[,"used"] == 0)) {
                      dropList <- grep("-unused", rownames(earthImp), value = TRUE)
                      out$Overall[rownames(out) %in% dropList] <- 0
                    }
                    rownames(out) <- gsub("-unused", "", rownames(out))                
                    out <- as.data.frame(out, stringsAsFactors = TRUE)
                    # fill in zeros for any variabels not  in out
                    
                    xNames <- object$namesx.org
                    if (any(!(xNames %in% rownames(out)))) {
                      xNames <- xNames[!(xNames %in% rownames(out))]
                      others <- data.frame(
                        Overall = rep(0, length(xNames)),
                        row.names = xNames)
                      out <- rbind(out, others)
                    }
                    out
                  },
                  levels = function(x) x$levels,
                  tags = c("Multivariate Adaptive Regression Splines", 
                           "Implicit Feature Selection", 
                           "Accepts Case Weights"),
                  notes = paste(
                    "Unlike other packages used by `train`, the `earth`",
                    "package is fully loaded when this model is used."
                  ),                  
                  sort = function(x) x[order(x$degree),])
