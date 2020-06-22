modelInfo <- list(label = "CART",
                  library = "rpart",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    
                    ## check to see if weights were passed in (and availible)
                    if(!is.null(wts)){
                      out <- rpart::rpart(.outcome ~ ., data = dat, ...)
                    } else {
                      out <- rpart::rpart(.outcome ~ ., data = dat, weights = wts, ...)
                    }
                    out           
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {                  
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    
                    out <- if(modelFit$problemType == "Classification") 
                      predict(modelFit, newdata, type = "class") else 
                        predict(modelFit, newdata)
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata, type = "prob")
                  },
                  predictors = function(x, surrogate = TRUE, ...)  {
                    out <- as.character(x$frame$var)
                    out <- out[!(out %in% c("<leaf>"))]
                    if(surrogate)
                    {
                      splits <- x$splits
                      splits <- splits[splits[,"adj"] > 0,]
                      out <- c(out, rownames(splits))
                    }
                    unique(out)
                  },
                  varImp = function(object, surrogates = FALSE, competes = TRUE, ...) {
                    tmp <- rownames(object$splits)
                    rownames(object$splits) <- 1:nrow(object$splits)
                    splits <- data.frame(object$splits)
                    splits$var <- tmp
                    splits$type <- ""
                    
                    frame <- as.data.frame(object$frame, stringsAsFactors = TRUE)
                    index <- 0
                    for(i in 1:nrow(frame)) {
                      if(frame$var[i] != "<leaf>") {
                        index <- index + 1
                        splits$type[index] <- "primary"
                        if(frame$ncompete[i] > 0) {
                          for(j in 1:frame$ncompete[i]) {
                            index <- index + 1
                            splits$type[index] <- "competing"
                          }
                        }
                        if(frame$nsurrogate[i] > 0) {
                          for(j in 1:frame$nsurrogate[i]) {
                            index <- index + 1
                            splits$type[index] <- "surrogate"
                          }
                        }
                      }
                    }
                    splits$var <- factor(as.character(splits$var))
                    if(!surrogates) splits <- subset(splits, type != "surrogate")
                    if(!competes) splits <- subset(splits, type != "competing")
                    out <- aggregate(splits$improve,
                                     list(Variable = splits$var),
                                     sum,
                                     na.rm = TRUE)
                    
                    allVars <- colnames(attributes(object$terms)$factors)
                    if(!all(allVars %in% out$Variable)) {
                      missingVars <- allVars[!(allVars %in% out$Variable)]
                      zeros <- data.frame(x = rep(0, length(missingVars)),
                                          Variable = missingVars)
                      out <- rbind(out, zeros)
                    }
                    out2 <- data.frame(Overall = out$x)
                    rownames(out2) <- out$Variable
                    out2  
                  },
                  levels = function(x) x$obsLevels,
                  trim = function(x) {
                    x$call <- list(na.action = (x$call)$na.action)
                    x$x <- NULL
                    x$y <- NULL
                    x$where <- NULL
                    x
                  },
                  notes = "This CART model replicates the same process used by the `rpart` function where the model complexity is determined using the one-standard error method. This procedure is replicated inside of the resampling done by `train` so that an external resampling estimate can be obtained.",
                  tags = c("Tree-Based Model", "Implicit Feature Selection", "Handle Missing Predictor Data", "Accepts Case Weights"),
                  sort = function(x) x[order(x[,1], decreasing = TRUE),])
