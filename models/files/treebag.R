modelInfo <- list(label = "Bagged CART",
                  library = c("ipred", "plyr", "e1071"),
                  loop = NULL,
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = "parameter",
                                          class = "character",
                                          label = "parameter"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(parameter = "none"),
                  fit = function(x, y, wts, param, lev, last,classProbs, ...) {
                    theDots <- list(...)
                    if(!any(names(theDots) == "keepX")) theDots$keepX <- FALSE   
                    modelArgs <- c(list(X = x, y = y), theDots)
                    if(!is.null(wts)) modelArgs$weights <- wts
                    do.call(ipred::ipredbagg, modelArgs)
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
                    meanImp <- plyr::ddply(allImp, plyr::`.`(variable),
                                     function(x) c(Overall = mean(x$Overall)))
                    out <- data.frame(Overall = meanImp$Overall)
                    rownames(out) <- meanImp$variable
                    out
                  },
                  trim = function(x) {
                    trim_rpart <- function(x) {
                      x$call <- list(na.action = (x$call)$na.action)
                      x$x <- NULL
                      x$y <- NULL
                      x$where <- NULL
                      x
                    }
                    x$mtrees <- lapply(x$mtrees, 
                                       function(x){
                                         x$bindx <- NULL
                                         x$btree <- trim_rpart(x$btree)
                                         x
                                       } )
                    x
                  },
                  tags = c("Tree-Based Model", "Ensemble Model", "Bagging", "Accepts Case Weights"), 
                  levels = function(x) levels(x$y),
                  sort = function(x) x,
                  oob = function(x) {
                    if(is.null(x$X)) stop("to get OOB stats, keepX must be TRUE when calling the bagging function")
                    foo <- function(object, y, x) {
                      holdY <- y[-object$bindx]
                      tmp_x <- x[-object$bindx,,drop = FALSE]
                      if(!is.data.frame(tmp_x)) tmp_x <- as.data.frame(tmp_x, stringsAsFactors = TRUE)
                      if(is.factor(y)) {
                        tmp <- predict(object$btree, tmp_x, type = "class")
                        tmp <- factor(as.character(tmp), levels = levels(y))
                        out <- c(mean(holdY == tmp), e1071::classAgreement(table(holdY, tmp))$kappa)
                      } else {
                        tmp <- predict(object$btree, tmp_x)
                        out <- c(sqrt(mean((tmp - holdY)^2, na.rm = TRUE)),
                                 cor(holdY, tmp, use = "pairwise.complete.obs")^2)
                      }
                      out
                    }
                    eachStat <- lapply(x$mtrees, foo, y = x$y, x = x$X)
                    eachStat <- matrix(unlist(eachStat), nrow = length(eachStat[[1]]))
                    out <- c(apply(eachStat, 1, mean, na.rm = TRUE),
                             apply(eachStat, 1, sd, na.rm = TRUE))
                    names(out) <- if(is.factor(x$y)) c("Accuracy", "Kappa", "AccuracySD", "KappaSD") else c("RMSE", "Rsquared", "RMSESD", "RsquaredSD")
                    out
                  })
