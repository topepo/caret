modelInfo <- list(label = "CART",
                  library = "rpart",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('cp'),
                                          class = c("numeric"),
                                          label = c("Complexity Parameter")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    initialFit <- rpart::rpart(.outcome ~ .,
                                               data = dat,
                                               control = rpart::rpart.control(cp = 0))$cptable
                    initialFit <- initialFit[order(-initialFit[,"CP"]), , drop = FALSE]
                    if(search == "grid") {
                      if(nrow(initialFit) < len) {
                        tuneSeq <- data.frame(cp = seq(min(initialFit[, "CP"]),
                                                       max(initialFit[, "CP"]),
                                                       length = len))
                      } else tuneSeq <-  data.frame(cp = initialFit[1:len,"CP"])
                      colnames(tuneSeq) <- "cp"
                    } else {
                      tuneSeq <- data.frame(cp = unique(sample(initialFit[, "CP"], size = len, replace = TRUE)))
                    }

                    tuneSeq
                  },
                  loop = function(grid) {
                    grid <- grid[order(grid$cp, decreasing = FALSE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    cpValue <- if(!last) param$cp else 0
                    theDots <- list(...)
                    if(any(names(theDots) == "control"))
                    {
                      theDots$control$cp <- cpValue
                      theDots$control$xval <- 0
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- rpart::rpart.control(cp = cpValue, xval = 0)

                    ## check to see if weights were passed in (and availible)
                    if(!is.null(wts)) theDots$weights <- wts

                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE),
                                        control = ctl),
                                   theDots)
                    modelArgs$data$.outcome <- y

                    out <- do.call(rpart::rpart, modelArgs)

                    if(last) out <- rpart::prune.rpart(out, cp = param$cp)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)

                    pType <- if(modelFit$problemType == "Classification") "class" else "vector"
                    out  <- predict(modelFit, newdata, type=pType)

                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      for(j in seq(along = submodels$cp))
                      {
                        prunedFit <- rpart::prune.rpart(modelFit, cp = submodels$cp[j])
                        tmp[[j+1]]  <- predict(prunedFit, newdata, type=pType)
                      }
                      out <- tmp
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, newdata, type = "prob")

                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      for(j in seq(along = submodels$cp))
                      {
                        prunedFit <- rpart::prune.rpart(modelFit, cp = submodels$cp[j])
                        tmpProb <- predict(prunedFit, newdata, type = "prob")
                        tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, drop = FALSE], stringsAsFactors = TRUE)
                      }
                      out <- tmp
                    }
                    out
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
                    if(nrow(object$splits)>0) {
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
                    } else {
		      out <- data.frame(x = numeric(), Variable = character())
		    }
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
                  tags = c("Tree-Based Model", "Implicit Feature Selection", "Handle Missing Predictor Data", "Accepts Case Weights"),
                  sort = function(x) x[order(x[,1], decreasing = TRUE),])
