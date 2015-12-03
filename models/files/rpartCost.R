modelInfo <- list(label = "Cost-Sensitive CART",
                  library = "rpart",
                  type = "Classification",
                  parameters = data.frame(parameter = c('cp', 'Cost'),
                                          class = c("numeric", "numeric"),
                                          label = c("Complexity Parameter", "Cost")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    dat <- if(is.data.frame(x)) x else as.data.frame(x)
                    dat$.outcome <- y
                    initialFit <- rpart(.outcome ~ .,
                                        data = dat,
                                        control = rpart.control(cp = 0))$cptable
                    initialFit <- initialFit[order(-initialFit[,"CP"]), , drop = FALSE] 
                    if(search == "grid") {
                      if(nrow(initialFit) < len) {
                        tuneSeq <- expand.grid(cp = seq(min(initialFit[, "CP"]), 
                                                        max(initialFit[, "CP"]), 
                                                        length = len),
                                               Cost = 1:len)
                      } else tuneSeq <-  data.frame(cp = initialFit[1:len,"CP"], Cost = 1:len)
                      colnames(tuneSeq) <- c("cp", "Cost")
                    } else {
                      tuneSeq <- data.frame(cp = unique(sample(initialFit[, "CP"], size = len, replace = TRUE)),
                                            Cost = runif(len, min = 1, max = 30))
                    }
                    
                    tuneSeq
                  },
                  loop = function(grid) {
                    grid <- grid[order(grid$Cost, grid$cp, decreasing = TRUE),, drop = FALSE]
                    uniqueCost <- unique(grid$Cost)            
                    loop <- data.frame(Cost = uniqueCost)
                    loop$cp <- NA
                    
                    submodels <- vector(mode = "list", length = length(uniqueCost))
                    
                    for(i in seq(along = uniqueCost))
                    {
                      subCP <- grid[grid$Cost == uniqueCost[i],"cp"]
                      loop$cp[loop$Cost == uniqueCost[i]] <- subCP[which.min(subCP)]
                      submodels[[i]] <- data.frame(cp = subCP[-which.max(subCP)])
                    }  
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    theDots <- list(...)
                    if(any(names(theDots) == "control"))
                    {
                      theDots$control$cp <- param$cp
                      theDots$control$xval <- 0 
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- rpart.control(cp = param$cp, xval = 0)   
                    
                    lmat <-matrix(c(0, 1, param$Cost, 0), ncol = 2)
                    rownames(lmat) <- colnames(lmat) <- levels(y)
                    if(any(names(theDots) == "parms"))
                    {
                      theDots$parms$loss <- lmat
                    } else parms <- list(loss = lmat)
                    
                    ## check to see if weights were passed in (and availible)
                    if(!is.null(wts)) theDots$weights <- wts    
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = if(is.data.frame(x)) x else as.data.frame(x),
                                        parms = parms,
                                        control = ctl),
                                   theDots)
                    modelArgs$data$.outcome <- y
                    
                    out <- do.call("rpart", modelArgs)
                    out           
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    
                    pType <- if(modelFit$problemType == "Classification") "class" else "vector"
                    out  <- predict(modelFit, newdata, type=pType)
                    
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      for(j in seq(along = submodels$cp))
                      {
                        prunedFit <- prune.rpart(modelFit, cp = submodels$cp[j])
                        tmp[[j+1]]  <- predict(prunedFit, newdata, type=pType)
                      }
                      out <- tmp
                    }
                    out
                  },
                  levels = function(x) x$obsLevels,
                  prob = NULL,
                  tags = c("Tree-Based Model", "Implicit Feature Selection", "Cost Sensitive Learning", 
                           "Two Class Only", "Handle Missing Predictor Data", "Accepts Case Weights"),
                  sort = function(x) x[order(-x$cp, -x$Cost),])
