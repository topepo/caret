modelInfo <- list(label = "Linear Regression with Stepwise Selection",
                  library = "leaps",
                  type = "Regression",
                  parameters = data.frame(parameter = 'nvmax',
                                          class = "numeric",
                                          label = 'Maximum Number of Predictors'),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- data.frame(nvmax = 2:(len+1))
                    } else {
                      out <- data.frame(nvmax = sort(unique(sample(2:(ncol(x) - 1), size = len, replace = TRUE))))
                    }
                    out
                  },
                  loop = function(grid) {   
                    grid <- grid[order(grid$nvmax, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {   
                    theDots <- list(...)
                    if(any(names(theDots) == "nbest")) stop("'nbest' should not be specified")
                    if(any(names(theDots) == "method")) stop("'method' should not be specified")
                    if(any(names(theDots) == "nvmax")) stop("'nvmax' should not be specified")
                  
                    leaps::regsubsets(as.matrix(x), y,
                               weights = if(!is.null(wts)) wts else rep(1, length(y)),
                               nbest = 1, nvmax = param$nvmax, method = "seqrep", ...)
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    newdata <- as.matrix(newdata)
                    foo <- function(b, x) x[,names(b),drop = FALSE] %*% b
                    
                    path <- 1:(modelFit$nvmax - 1)
                    betas <- coef(modelFit, id = 1:(modelFit$nvmax - 1))
                    
                    newdata <- cbind(rep(1, nrow(newdata)), as.matrix(newdata))
                    colnames(newdata)[1] <- "(Intercept)"
                    
                    out <- foo(betas[[length(betas)]], newdata)[,1]
                    
                    if(!is.null(submodels))
                    {
                      numTerms <- unlist(lapply(betas, length))
                      if(any(names(betas[[length(betas)]]) == "(Intercept)")) numTerms <- numTerms - 1
                      ## Need to find the elements of betas that 
                      ## correspond to the values of submodels$nvmax
                      
                      keepers <- which(numTerms %in% submodels$nvmax)
                      if(length(keepers) != length(submodels$nvmax))
                        stop("Some values of 'nvmax' are not in the model sequence.")
                      
                      ## The grid code sorted from largest to smallest, so 
                      ## to match them, reverse the order
                      keepers <- rev(keepers)
                      preds <- lapply(betas[keepers], foo, x= newdata)
                      preds <- do.call("cbind", preds)
                      
                      out <- as.data.frame(cbind(out, preds), stringsAsFactors = TRUE)
                      out <- as.list(out)
                    }
                    
                    out
                  },
                  tags = c("Linear Regression", "Feature Selection Wrapper"),
                  prob = NULL,
                  sort = function(x) x[order(x[,1]),])
