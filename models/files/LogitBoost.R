modelInfo <- list(label = "Boosted Logistic Regression",
                  library = "caTools",
                  loop = function(grid) {            
                    ## Get the largest value of ncomp to fit the "full" model
                    loop <- grid[which.max(grid$nIter),,drop = FALSE]
                    
                    submodels <- grid[-which.max(grid$nIter),,drop = FALSE]
                    
                    ## This needs to be excased in a list in case there are more
                    ## than one tuning parameter
                    submodels <- list(submodels)  
                    list(loop = loop, submodels = submodels)
                  },
                  type = "Classification",
                  parameters = data.frame(parameter = 'nIter',
                                          class = 'numeric',
                                          label = '# Boosting Iterations'),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(nIter = 1 + ((1:len)*10))
                    } else {
                      out <- data.frame(nIter = unique(sample(1:100, size = len, replace = TRUE)))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    ## There is another package with a function called `LogitBoost`
                    ## so we call using the namespace
                    caTools::LogitBoost(as.matrix(x), y, nIter = param$nIter)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    ## This model was fit with the maximum value of nIter
                    out <- caTools::predict.LogitBoost(modelFit, newdata, type="class")
                    ## submodels contains one of the elements of 'submodels'. In this 
                    ## case, 'submodels' is a data frame with the other values of
                    ## nIter. We loop over these to get the other predictions.
                    if(!is.null(submodels))
                    {                   
                      ## Save _all_ the predictions in a list
                      tmp <- out
                      out <- vector(mode = "list", length = nrow(submodels) + 1)
                      out[[1]] <- tmp
                      
                      for(j in seq(along = submodels$nIter))
                      {
                        out[[j+1]] <- caTools::predict.LogitBoost(modelFit,
                                                                  newdata,
                                                                  nIter = submodels$nIter[j])
                      }
                    }
                    out                   
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- caTools::predict.LogitBoost(modelFit, newdata, type = "raw")
                    ## I've seen them not be on [0, 1]
                    out <- t(apply(out, 1, function(x) x/sum(x)))
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$nIter))
                      {                           
                        tmpProb <- caTools::predict.LogitBoost(modelFit,
                                                               newdata,
                                                               type = "raw",
                                                               nIter = submodels$nIter[j])
                        tmpProb <- out <- t(apply(tmpProb, 1, function(x) x/sum(x)))
                        tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, drop = FALSE], stringsAsFactors = TRUE)           
                      }
                      out <- tmp
                    }                       
                    out
                  },
                  predictors = function(x, ...) {                    
                    if(!is.null(x$xNames))
                    {
                      out <- unique(x$xNames[x$Stump[, "feature"]])
                    } else out <- NA
                    
                    out
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Ensemble Model", "Boosting", "Implicit Feature Selection",
                           "Tree-Based Model", "Logistic Regression"),
                  sort = function(x) x[order(x[,1]),])
