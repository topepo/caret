modelInfo <- list(label = "C5.0", 
                  library = c("C50", "plyr"),
                  loop = function(grid) {     
                    loop <- ddply(grid, c("model", "winnow"),
                                  function(x) c(trials = max(x$trials)))                 
                    
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$trials))
                    {
                      index <- which(grid$model == loop$model[i] & 
                                       grid$winnow == loop$winnow[i])
                      trials <- grid[index, "trials"] 
                      submodels[[i]] <- data.frame(trials = trials[trials != loop$trials[i]])
                    }     
                    list(loop = loop, submodels = submodels)
                  },
                  type = "Classification",
                  parameters = data.frame(parameter = c('trials', 'model', 'winnow'),
                                          class = c("numeric", "character", "logical"),
                                          label = c('# Boosting Iterations', 'Model Type', 'Winnow')),
                  grid = function(x, y, len = NULL) {
                    c5seq <- if(len == 1)  1 else  c(1, 10*((2:min(len, 11)) - 1))
                    expand.grid(trials = c5seq, model = c("tree", "rules"), winnow = c(TRUE, FALSE))
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control"))
                    {                           
                      theDots$control$winnow <- param$winnow
                    } else theDots$control <- C5.0Control(winnow = param$winnow)
                    argList <- list(x = x, y = y, weights = wts, trials = param$trials,
                                    rules = param$model == "rules")
                    argList <- c(argList, theDots)
                    do.call("C5.0.default", argList)
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata)
                    
                    if(!is.null(submodels))
                    {
                      tmp <- out
                      out <- vector(mode = "list", length = nrow(submodels) + 1)
                      out[[1]] <- tmp
                      
                      for(j in seq(along = submodels$trials))
                        out[[j+1]] <- as.character(predict(modelFit, newdata, trial = submodels$trials[j]))
                    }
                    out              
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, type= "prob")
                    
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      
                      for(j in seq(along = submodels$trials))
                      {
                        tmp[[j+1]] <- predict(modelFit, newdata, type= "prob", trials = submodels$trials[j])
                      }
                      out <- tmp
                    }
                    out
                  },
                  predictors = function(x, ...) {
                    vars <- C5imp(x, metric = "splits")
                    rownames(vars)[vars$Overall > 0]
                  },
                  varImp = function(object, ...) C5imp(object, ...),
                  tags = c("Tree-Based Model", "Rule-Based Model", "Implicit Feature Selection",
                  	       "Boosting", "Ensemble Model"),
                  sort = function(x) {
                    x$model <- factor(as.character(x$model), levels = c("rules", "tree"))
                    x[order(x$trials, x$model, !x$winnow),]
                  })
