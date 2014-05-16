modelInfo <- list(label = "Bagged CART",
                  library = c("ipred", "plyr"),
                  loop = NULL,
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL) data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last,classProbs, ...) {
                    theDots <- list(...)
                    if(!any(names(theDots) == "keepX")) theDots$keepX <- FALSE   
                    modelArgs <- c(list(X = x, y = y), theDots)
                    if(!is.null(wts)) modelArgs$weights <- wts   
                    do.call("ipredbagg", modelArgs)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "prob"),
                  predictors = function(x, surrogate = TRUE, ...) {
                    code <- getModelInfo("rpart", regex = FALSE)[[1]]$predictors
                    eachTree <- lapply(x$mtree,
                                       function(u, surr) code(u$btree, surrogate = surr),
                                       surr = surrogate)
                    unique(unlist(eachTree))
                  },
                  varImp = function(object, ...) {
                    allImp <- lapply(object$mtrees, function(x) varImp(x$btree), ...)
                    allImp <- lapply(allImp, 
                                     function(x) {
                                       x$variable <- rownames(x)
                                       x
                                     })
                    allImp <- do.call("rbind", allImp)
                    meanImp <- ddply(allImp, .(variable), 
                                     function(x) c(Overall = mean(x$Overall)))
                    out <- data.frame(Overall = meanImp$Overall)
                    rownames(out) <- meanImp$variable
                    out
                  },
                  tags = c("Tree-Based Model", "Ensemble Model", "Bagging"), 
                  levels = function(x) levels(x$y),
                  sort = function(x) x)
