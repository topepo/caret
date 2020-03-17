modelInfo <- list(label = "Boosted Smoothing Spline", 
                  library = c("bst", "plyr"),
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('mstop', 'nu'),
                                          class = c("numeric", "numeric"),
                                          label = c('# Boosting Iterations', 'Shrinkage')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(mstop = floor((1:len) * 50), nu = .1)
                    } else {
                      out <- data.frame(mstop = sample(1:500, replace = TRUE, size = len),        
                                        nu = runif(len, min = .001, max = .6))
                    }
                    out
                  },
                  loop = function(grid) {
                    loop <- plyr::ddply(grid, plyr::`.`(nu), function(x) c(mstop = max(x$mstop)))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$mstop))
                    {
                      index <- which(grid$nu == loop$nu[i])
                      subTrees <- grid[index, "mstop"] 
                      submodels[[i]] <- data.frame(mstop = subTrees[subTrees != loop$mstop[i]])
                    }      
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    if(!is.data.frame(x) | inherits(x, "tbl_df")) 
                      x <- as.data.frame(x, stringsAsFactors = TRUE)
                    
                    theDots <- list(...)
                    modDist <- if(is.factor(y)) "hinge" else "gaussian"
                    
                    y <- if(is.factor(y)) ifelse(y == lev[1], 1, -1) else y
                    
                    if(any(names(theDots) == "ctrl")) {
                      theDots$ctrl$mstop <- param$mstop
                      theDots$ctrl$nu <- param$nu
                    } else {
                      theDots$ctrl <- bst::bst_control(mstop = param$mstop, nu = param$nu)
                    }
                    
                    modArgs <- list(x = x, y = y, family = modDist, learner = "sm")
                    modArgs <- c(modArgs, theDots)
                    
                    out <- do.call(bst::bst, modArgs)
                    out$call <- quote(redacted)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata) | inherits(newdata, "tbl_df")) 
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    if(modelFit$problemType == "Classification") {
                      out <- predict(modelFit, newdata, type = "class", mstop = modelFit$submodels$mstop)
                      out <- ifelse(out == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
                    } else {
                      out <- predict(modelFit, newdata, type = "response", mstop = modelFit$submodels$mstop)
                    }
                    
                    if(!is.null(submodels))  {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$mstop))  {
                        if(modelFit$problemType == "Classification") {
                          bstPred <- predict(modelFit, newdata, type = "class", mstop = submodels$mstop[j])
                          tmp[[j+1]] <- ifelse(bstPred == 1, modelFit$obsLevels[1], modelFit$obsLevels[2])
                        } else {
                          tmp[[j+1]]  <- predict(modelFit, newdata, type = "response", mstop = submodels$mstop[j])
                        }
                      }
                      out <- tmp
                    }
                    out         
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Ensemble Model", "Boosting", "Implicit Feature Selection"),
                  prob = NULL,
                  sort = function(x) x[order(x$mstop, x$nu),] )
