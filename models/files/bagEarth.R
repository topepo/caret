modelInfo <- list(label = "Bagged MARS", 
                  library = "earth",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('nprune', 'degree'),
                                          class = c("numeric", "numeric"),
                                          label = c('#Terms', 'Product Degree')),
                  grid = function(x, y, len = NULL) {
                    dat <- x
                    dat$.outcome <- y
                    
                    mod <- earth( .outcome~., data = dat, pmethod = "none")
                    maxTerms <- nrow(mod$dirs)
                    
                    maxTerms <- min(200, floor(maxTerms * .75) + 2)
                    data.frame(nprune = unique(floor(seq(2, to = maxTerms, length = len))),
                               degree = 1)
                  },
                  loop = function(grid) {     
                    deg <- unique(grid$degree)
                    
                    loop <- data.frame(degree = deg)
                    loop$nprune <- NA
                    
                    submodels <- vector(mode = "list", length = length(deg))
                    for(i in seq(along = deg))
                    {
                      np <- grid[grid$degree == deg[i],"nprune"]
                      loop$nprune[loop$degree == deg[i]] <- np[which.max(np)]
                      submodels[[i]] <- data.frame(nprune = np[-which.max(np)])
                    }  
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    theDots <- list(...)
                    theDots$keepxy <- TRUE 
                    
                    modelArgs <- c(list(x = x, y = y,
                                        degree = param$degree,
                                        nprune = param$nprune),
                                   theDots)
                    if(is.factor(y)) modelArgs$glm <- list(family=binomial)
                    
                    tmp <- do.call("bagEarth", modelArgs)
                    
                    tmp$call["nprune"] <-  param$nprune
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
                    
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- if(is.matrix(out)) out[,1] else out
                      
                      for(j in seq(along = submodels$nprune))
                      {
                        prunedFit <- update(modelFit, nprune = submodels$nprune[j])
                        if(modelFit$problemType == "Classification")
                        {
                          tmp[[j+1]]  <-  predict(prunedFit, newdata,  type = "class")
                        } else {
                          tmp[[j+1]]  <-  predict(prunedFit, newdata)
                        }
                      }
                      
                      out <- tmp
                    }
                    out            
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, type= "response")
                    out <- cbind(1-out, out)
                    colnames(out) <-  modelFit$obsLevels
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$nprune))
                      {
                        prunedFit <- update(modelFit, nprune = submodels$nprune[j])
                        tmp2 <- predict(prunedFit, newdata, type= "response")
                        tmp2 <- cbind(1-tmp2, tmp2)
                        colnames(tmp2) <-  modelFit$obsLevels
                        tmp[[j+1]] <- tmp2
                      }
                      out <- tmp
                    }
                    out
                  },
                  predictors = function(x, ...) {
                    predEarth <- function(x) {
                      vi <- varImp(x)
                      notZero <- sort(unique(unlist(lapply(vi, function(x) which(x > 0)))))
                      if(length(notZero) > 0) rownames(vi)[notZero] else NULL
                    }
                    eachFit <- lapply(x$fit, predEarth)
                    unique(unlist(eachFit))
                  },
                  varImp = function(object, ...) {
                    allImp <- lapply(object$fit, varImp, ...)
                    impDF <- as.data.frame(allImp)
                    meanImp <- apply(impDF, 1, mean)
                    out <- data.frame(Overall = meanImp)
                    rownames(out) <- names(meanImp)
                    out
                  },
                  levels = function(x) x$levels,
                  tags = c("Multivariate Adaptive Regression Splines", "Ensemble Model", 
                           "Implicit Feature Selection", "Bagging"),
                  sort = function(x) x[order(x$degree, x$nprune),])
