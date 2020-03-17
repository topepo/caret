modelInfo <- list(label = "Tree-Based Ensembles",
                  library = c("nodeHarvest"),
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('maxinter', 'mode'),
                                          class = c('numeric', 'character'),
                                          label = c('Maximum Interaction Depth', 'Prediction Mode')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(maxinter = 1:len, mode = c("mean", "outbag"))
                    } else {
                      out <- data.frame(maxinter = sample(1:20, size = len, replace = TRUE), 
                                        mode = sample(c("mean", "outbag"), size = len, replace = TRUE))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...){
                    if(!is.data.frame(x) | inherits(x, "tbl_df")) 
                      x <- as.data.frame(x, stringsAsFactors = TRUE)
                    if(is.numeric(y)) {
                      out <- nodeHarvest::nodeHarvest(x, y,
                                                      maxinter = param$maxinter,
                                                      mode = param$mode,
                                                      ...)
                    } else {
                      if(length(lev) > 2) stop("Two Class problems only")
                      out <- nodeHarvest::nodeHarvest(x,
                                                     ifelse(y == levels(y)[1], 1, 0),
                                                     maxinter = param$maxinter,
                                                     mode = param$mode,
                                                     ...)                          
                    }
                    out   
                  },
                  predict = function(modelFit, newdata, submodels = NULL){
                    if(!is.data.frame(newdata) | inherits(newdata, "tbl_df")) 
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    if(modelFit$problemType == "Regression") {
                      predict(modelFit, as.matrix(newdata), maxshow = 0)
                    } else  {
                      prbs <- predict(modelFit, as.matrix(newdata), maxshow = 0)
                      ifelse(prbs > .5, modelFit$obsLevels[1], modelFit$obsLevels[2])
                    }
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    if(!is.data.frame(newdata) | inherits(newdata, "tbl_df")) 
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, as.matrix(newdata), maxshow = 0)
                    if(is.vector(out)) {
                      out <- cbind(out, 1 - out)
                      colnames(out) <- modelFit$obsLevels
                    }
                    out
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Tree-Based Model", "Implicit Feature Selection", "Ensemble Model", "Two Class Only"),
                  sort = function(x) x[order(x$maxinter, x$mode),])
