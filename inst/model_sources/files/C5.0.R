modelInfo <- list(label = "C5.0", 
                  library = c("C50", "plyr"),
                  loop = function(grid) {     
                    loop <- plyr::ddply(grid, c("model", "winnow"),
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
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      c5seq <- if(len == 1)  1 else  c(1, 10*((2:min(len, 11)) - 1))
                      out <- expand.grid(trials = c5seq, model = c("tree", "rules"), winnow = c(TRUE, FALSE))
                    } else {
                      out <- data.frame(trials = sample(1:100, replace = TRUE, size = len),
                                        model = sample(c("tree", "rules"), replace = TRUE, size = len),
                                        winnow = sample(c(TRUE, FALSE), replace = TRUE, size = len))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control"))
                    {                           
                      theDots$control$winnow <- param$winnow
                    } else theDots$control <- C50::C5.0Control(winnow = param$winnow)
                    argList <- list(x = x, y = y, weights = wts, trials = param$trials,
                                    rules = param$model == "rules")
                    argList <- c(argList, theDots)
                    do.call(C50:::C5.0.default, argList)
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
                  levels = function(x) x$obsLevels,
                  predictors = function(x, ...) {
                    vars <- C50::C5imp(x, metric = "splits")
                    rownames(vars)[vars$Overall > 0]
                  },
                  varImp = function(object, ...) C50::C5imp(object, ...),
                  tags = c("Tree-Based Model", "Rule-Based Model", "Implicit Feature Selection",
                           "Boosting", "Ensemble Model", "Handle Missing Predictor Data", "Accepts Case Weights"),
                  sort = function(x) {
                    x$model <- factor(as.character(x$model), levels = c("rules", "tree"))
                    x[order(x$trials, x$model, !x$winnow),]
                  },
                  trim = function(x) {
                    x$boostResults <- NULL
                    x$size <- NULL
                    x$call <- NULL
                    x$output <- NULL
                    x
                  })
