modelInfo <- list(label = "Boosted Tree", 
                  library = c("party", "mboost", "plyr", "partykit"),
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('mstop', 'maxdepth'),
                                          class = c("numeric", "numeric"),
                                          label = c('#Trees', 'Max Tree Depth')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(maxdepth  = seq(1, len),
                                         mstop = floor((1:len) * 50))
                    } else {
                      out <- data.frame(mstop = sample(1:1000, replace = TRUE, size = len),
                                        maxdepth = sample(1:10, replace = TRUE, size = len))
                    }
                    out
                  },
                  loop = function(grid) {
                    loop <- plyr::ddply(grid, plyr::`.`(maxdepth), function(x) c(mstop = max(x$mstop)))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$mstop))  {
                      index <- which(grid$maxdepth == loop$maxdepth[i])
                      subStops <- grid[index, "mstop"] 
                      submodels[[i]] <- data.frame(mstop = subStops[subStops != loop$mstop[i]])
                    }     
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    theDots <- list(...)
                    
                    if(length(levels(y)) > 2) 
                      stop("Two-class outcomes only. See ?mboost::Multinomial",
                           call. = FALSE)
                    
                    if(any(names(theDots) == "tree_controls")) {
                      theDots$tree_controls$maxdepth <- param$maxdepth 
                      treeCtl <- theDots$tree_controls
                      theDots$tree_controls <- NULL

                    } else treeCtl <- partykit::ctree_control(maxdepth = param$maxdepth)

                    if(any(names(theDots) == "control")) {
                      theDots$control$mstop <- param$mstop 
                      ctl <- theDots$control
                      theDots$control <- NULL

                    } else ctl <- mboost::boost_control(mstop = param$mstop)

                    if(!any(names(theDots) == "family")) {
                      if(is.factor(y)) {
                        theDots$family <- if(length(lev) == 2) mboost::Binomial() else mboost::Multinomial()
                        } else theDots$family <- mboost::GaussReg()
                    }

                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = if(!is.data.frame(x)) as.data.frame(x, stringsAsFactors = TRUE) else x,
                                        control = ctl,
                                        tree_controls = treeCtl),
                                   theDots)  
                    modelArgs$data$.outcome <- y

                    out <- do.call(mboost::blackboost, modelArgs)
                    out$call["data"] <- "data"
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predType <- ifelse(modelFit$problemType == "Classification", "class", "response")
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, newdata, type = predType)
                    
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- as.vector(out)
                      
                      for(j in seq(along = submodels$mstop)) {
                        tmp[[j+1]]  <- as.vector(predict(modelFit[submodels$mstop[j]],
                                                         newdata,
                                                         type = predType))
                      }
                      
                      out <- tmp
                    }
                    
                    out  
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    probs <- predict(modelFit, newdata, type = "response")
                    out <- cbind(1 - probs, probs)
                    colnames(out) <- modelFit$obsLevels
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$mstop)) {                           
                        tmpProb <- predict(modelFit[submodels$mstop[j]], newdata, type = "response")
                        tmpProb <- cbind(1 - tmpProb, tmpProb)
                        colnames(tmpProb) <- modelFit$obsLevels
                        tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, drop = FALSE], stringsAsFactors = TRUE)           
                      }
                      out <- tmp
                    }                        
                    out
                  },
                  predictors = function(x, ...) {
                    strsplit(variable.names(x), ", ")[[1]]
                  },
                  levels = function(x) levels(x$response),
                  tags = c("Tree-Based Model", "Ensemble Model", "Boosting", "Accepts Case Weights"),
                  sort = function(x) x[order(x$mstop, x$maxdepth),])
