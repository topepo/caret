modelInfo <- list(label = "CART or Ordinal Responses",
                  library = c("rpartScore", "plyr"),
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('cp', "split", "prune"),
                                          class = c("numeric", "character", "character"),
                                          label = c("Complexity Parameter", "Split Function", "Pruning Measure")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    initialFit <- rpart::rpart(.outcome ~ .,
                                               data = dat,
                                               control = rpart::rpart.control(cp = 0))$cptable
                    initialFit <- initialFit[order(-initialFit[,"CP"]), , drop = FALSE] 
                    if(search == "grid") {
                      if(nrow(initialFit) < len) {
                        tuneSeq <- expand.grid(cp = seq(min(initialFit[, "CP"]), 
                                                       max(initialFit[, "CP"]), 
                                                       length = len),
                                               split = c("abs", "quad"),
                                               prune = c("mr", "mc"))
                      } else tuneSeq <-  expand.grid(cp = initialFit[1:len,"CP"],
                                                     split = c("abs", "quad"),
                                                     prune = c("mr", "mc"))
                      colnames(tuneSeq)[1] <- "cp"
                    } else {
                      tuneSeq <- expand.grid(cp = unique(sample(initialFit[, "CP"], size = len, replace = TRUE)),
                                             split = c("abs", "quad"),
                                             prune = c("mr", "mc"))
                    }
                    
                    tuneSeq
                  },
#                   loop = function(grid) {
#                     loop <- ddply(grid, c("split", "prune"),
#                                   function(x) c(cp = max(x$cp)))
#                     submodels <- vector(mode = "list", length = nrow(loop))
#                     for(i in seq(along = loop$cp)) {
#                       index <- which(grid$prune == loop$prune[i] & 
#                                        grid$split == loop$split[i])
#                       trees <- grid[index, "cp"] 
#                       submodels[[i]] <- data.frame(cp = trees[trees != loop$cp[i]])
#                     }    
#                     list(loop = loop, submodels = submodels)
#                   },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    cpValue <- if(!last) param$cp else 0
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                      theDots$control$cp <- cpValue
                      theDots$control$xval <- 0 
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- rpart::rpart.control(cp = cpValue, xval = 0)

                    ## check to see if weights were passed in (and availible)
                    if(!is.null(wts)) theDots$weights <- wts    
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE),
                                        split = as.character(param$split),
                                        prune = as.character(param$prune),
                                        control = ctl),
                                   theDots)
                    modelArgs$data$.outcome <- as.numeric(y)

                    out <- do.call(rpartScore::rpartScore, modelArgs)

                    if(last) out <- rpart::prune.rpart(out, cp = param$cp)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {    
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out  <-  modelFit$obsLevels[predict(modelFit, newdata)]
                    
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      for(j in seq(along = submodels$cp)) {
                        prunedFit <- rpart::prune.rpart(modelFit, cp = submodels$cp[j])
                        tmp[[j+1]]  <- modelFit$obsLevels[predict(prunedFit, newdata)]
                      }
                      out <- tmp
                    }
                    out
                  },
                  prob = NULL,
                  predictors = function(x, surrogate = TRUE, ...)  {
                    out <- as.character(x$frame$var)
                    out <- out[!(out %in% c("<leaf>"))]
                    if(surrogate) {
                      splits <- x$splits
                      splits <- splits[splits[,"adj"] > 0,]
                      out <- c(out, rownames(splits))
                    }
                    unique(out)
                  },
                  varImp = function(object, surrogates = FALSE, competes = TRUE, ...) {
                    allVars <- all.vars(object$terms)
                    allVars <- allVars[allVars != ".outcome"]
                    out <- data.frame(Overall = object$variable.importance,
                                      Variable = names(object$variable.importance))
                    rownames(out) <- names(object$variable.importance)
                    
                    if(!all(allVars %in% out$Variable)) {
                      missingVars <- allVars[!(allVars %in% out$Variable)]
                      zeros <- data.frame(Overall = rep(0, length(missingVars)),
                                          Variable = missingVars)
                      out <- rbind(out, zeros)
                    }
                    rownames(out) <- out$Variable
                    out$Variable <- NULL
                    out  
                  },
                  levels = function(x) x$obsLevels,
                  trim = function(x) {
                    x$call <- list(na.action = (x$call)$na.action)
                    x$x <- NULL
                    x$y <- NULL
                    x$where <- NULL
                    x
                  },
                  tags = c("Tree-Based Model", "Implicit Feature Selection", "Handle Missing Predictor Data", 
                           "Accepts Case Weights", "Ordinal Outcomes"),
                  sort = function(x) x[order(x[,1], decreasing = TRUE),])
