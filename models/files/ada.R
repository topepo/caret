modelInfo <- list(label = "Boosted Classification Trees",
                  library = c("ada", "plyr"),
                  loop = function(grid) {     
                    loop <- ddply(grid, c("nu", "maxdepth"),
                                  function(x) c(iter = max(x$iter)))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$iter)) {
                      index <- which(grid$maxdepth == loop$maxdepth[i] & 
                                       grid$nu == loop$nu[i])
                      trees <- grid[index, "iter"] 
                      submodels[[i]] <- data.frame(iter = trees[trees != loop$iter[i]])
                    }    
                    list(loop = loop, submodels = submodels)
                  },
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('iter', 'maxdepth', 'nu'),
                                          class = rep("numeric", 3),
                                          label = c('#Trees', 'Max Tree Depth', 'Learning Rate')),
                  grid = function(x, y, len = NULL) expand.grid(iter = floor((1:len) * 50),
                                                                maxdepth = seq(1, len),         
                                                                nu = .1),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control")) {
                      theDots$control$maxdepth <- param$maxdepth 
                      ctl <- theDots$control
                      theDots$control <- NULL
                      
                    } else ctl <- rpart.control(maxdepth = param$maxdepth,
                                                cp=-1,minsplit=0,xval=0) 
                    
                    modelArgs <- c(list(x = x,
                                        y = y,
                                        iter = param$iter,
                                        nu = param$nu,              
                                        control = ctl),
                                   theDots)
                    out <- do.call("ada", modelArgs)                    
                    out     
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    out <- predict(modelFit, newdata, n.iter = modelFit$tuneValue$iter)
                    
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = length(submodels$iter)+1)
                      tmp[[1]] <- out
                      for(i in seq(along = submodels$iter)) {
                        tmp[[i+1]] <- predict(modelFit, newdata, n.iter = submodels$iter[[i]])
                      }
                      out <- lapply(tmp, as.character)
                    }
                    out  
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    out <- predict(modelFit, newdata, type = "prob", 
                                   n.iter = modelFit$tuneValue$iter)
                    colnames(out) <- modelFit$obsLevels
                    
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = length(submodels$iter)+1)
                      tmp[[1]] <- out
                      for(i in seq(along = submodels$iter)) {
                        tmp[[i+1]] <- predict(modelFit, newdata, type = "prob", 
                                              n.iter = submodels$iter[[i]])
                        colnames(tmp[[i+1]]) <- modelFit$obsLevels
                      }
                      out <- lapply(tmp, as.data.frame)
                    }
                    out 
                  },
                  tags = c("Tree-Based Model", "Ensemble Model", "Boosting", 
                           "Implicit Feature Selection"),
                  sort = function(x) x[order(x$iter, x$maxdepth, x$nu),])


