modelInfo <- list(label = "Bagged AdaBoost",
                  library = c("adabag", "plyr"),
                  loop = function(grid) {     
                    loop <- plyr::ddply(grid, c("maxdepth"),
                                  function(x) c(mfinal = max(x$mfinal)))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$mfinal)) {
                      index <- which(grid$maxdepth == loop$maxdepth[i])
                      trees <- grid[index, "mfinal",drop = FALSE] 
                      submodels[[i]] <- data.frame(mfinal = trees[trees != loop$mfinal[i]])
                    }    
                    list(loop = loop, submodels = submodels)
                  },
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('mfinal', 'maxdepth'),
                                          class = c("numeric", "numeric"),
                                          label = c('#Trees', 'Max Tree Depth')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(mfinal = floor((1:len) * 50),
                                         maxdepth = seq(1, len))
                    } else {
                      out <- data.frame(mfinal = sample(1:100, replace = TRUE, size = len),
                                        maxdepth = sample(1:30, replace = TRUE, size = len))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control")) {
                      theDots$control$maxdepth <- param$maxdepth 
                      ctl <- theDots$control
                      theDots$control <- NULL
                      
                    } else ctl <- rpart::rpart.control(maxdepth = param$maxdepth,
                                                cp=-1,minsplit=0,xval=0) 
                    
                    if (!is.data.frame(x) | inherits(x, "tbl_df"))
                      x <- as.data.frame(x, stringsAsFactors = TRUE)
                    
                    modelArgs <- c(list(formula = as.formula(.outcome ~ .),
                                        data = x,
                                        mfinal = param$mfinal,            
                                        control = ctl),
                                   theDots)
                    modelArgs$data$.outcome <- y
                    out <- do.call(adabag::bagging, modelArgs)                    
                    out     
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if (!is.data.frame(newdata) | inherits(newdata, "tbl_df"))
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    ## The predict function requires the outcome! Trick it by
                    ## adding bogus data
                    newdata$.outcome <- factor(rep(modelFit$obsLevels[1], nrow(newdata)), 
                                               levels = modelFit$obsLevels)
                    out <- predict(modelFit, newdata, 
                                   newmfinal = modelFit$tuneValue$mfinal)$class
                    
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = length(submodels$mfinal)+1)
                      tmp[[1]] <- out
                      for(i in seq(along = submodels$mfinal)) {
                        tmp[[i+1]] <- predict(modelFit, newdata, 
                                              newmfinal = submodels$mfinal[[i]])$class
                      }
                      out <- tmp
                    }       
                    out  
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    if (!is.data.frame(newdata) | inherits(newdata, "tbl_df"))
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    ## The predict function requires the outcome! Trick it by
                    ## adding bogus data
                    newdata$.outcome <- factor(rep(modelFit$obsLevels[1], nrow(newdata)), 
                                               levels = modelFit$obsLevels)
                    out <- predict(modelFit, newdata)$prob
                    colnames(out) <- modelFit$obsLevels
                    
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = length(submodels$mfinal)+1)
                      tmp[[1]] <- out
                      for(i in seq(along = submodels$mfinal)) {
                        tmp[[i+1]] <- predict(modelFit, newdata,  
                                              newmfinal = submodels$mfinal[[i]])$prob
                        colnames(tmp[[i+1]]) <- modelFit$obsLevels
                      }
                      out <- lapply(tmp, as.data.frame)
                    }
                    
                    out 
                  },
                  varImp = function(object, ...){
                    imps <- data.frame(Overall = object$importance)
                    rownames(imps) <- names(object$importance)
                    imps
                  },
                  levels = function(x) x$obsLevels,
                  predictors = function(x, ...) names(x$importance)[x$importance != 0],
                  tags = c("Tree-Based Model", "Ensemble Model", "Boosting", "Bagging",
                           "Implicit Feature Selection", "Handle Missing Predictor Data"),
                  sort = function(x) x[order(x$mfinal, x$maxdepth),])



