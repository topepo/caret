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
                    theDots <- list(...)
                    theDots$keepxy <- TRUE 
                    
                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts
                    
                    modelArgs <- c(list(x = x, y = y,
                                        degree = param$degree),
                                   theDots)
                    if(is.factor(y)) modelArgs$glm <- list(family=binomial)
                    
                    tmp <- do.call("earth", modelArgs)
                    
                    tmp$call["degree"] <-  param$degree
                    tmp 
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(modelFit$problemType == "Classification")
                    {
                      out <- predict(modelFit, newdata,  type = "class")
                    } else {
                      out <- predict(modelFit, newdata)
                    }
                    as.vector(out)            
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, type= "response")
                    out <- cbind(1-out, out)
                    colnames(out) <-  modelFit$obsLevels
                    out
                  },
                  predictors = function(x, ...) {
                    vi <- varImp(x)
                    notZero <- sort(unique(unlist(lapply(vi, function(x) which(x > 0)))))
                    if(length(notZero) > 0) rownames(vi)[notZero] else NULL
                  },
                  varImp = function(object, value = "gcv", ...) {
                    earthImp <- evimp(object)
                    if(!is.matrix(earthImp)) earthImp <- t(as.matrix(earthImp))
                    
                    # get other variable names and padd with zeros
                    
                    out <- earthImp
                    perfCol <- which(colnames(out) == value)
                    
                    increaseInd <- out[,perfCol + 1]
                    out <- as.data.frame(out[,perfCol, drop = FALSE])  
                    colnames(out) <- "Overall"
                    
                    # At this point, we still may have some variables
                    # that are not in the model but have non-zero
                    # importance. We'll set those to zero
                    if(any(earthImp[,"used"] == 0)) {
                      dropList <- grep("-unused", rownames(earthImp), value = TRUE)
                      out$Overall[rownames(out) %in% dropList] <- 0
                    }
                    rownames(out) <- gsub("-unused", "", rownames(out))                
                    out <- as.data.frame(out)
                    # fill in zeros for any variabels not  in out
                    
                    xNames <- object$namesx.org
                    if(any(!(xNames %in% rownames(out)))) {
                      xNames <- xNames[!(xNames %in% rownames(out))]
                      others <- data.frame(
                        Overall = rep(0, length(xNames)),
                        row.names = xNames)
                      out <- rbind(out, others)
                    }
                    out
                  },
                  levels = function(x) x$levels,
                  tags = c("Multivariate Adaptive Regression Splines", "Implicit Feature Selection", 
                           "Accepts Case Weights"),
                  sort = function(x) x[order(x$degree),])
