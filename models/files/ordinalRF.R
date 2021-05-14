modelInfo  <- list(label = "Random Forest",
                   library = c("e1071", "ranger", "dplyr", "ordinalForest"),
                   check = function(pkg){
                     requireNamespace("ordinalForest")
                     current <- packageDescription("ordinalForest")$Version
                     expected <- "2.1"
                     if(compareVersion(current, expected) < 0)
                       stop("This modeling workflow requires ordinalForest version ",
                            expected, "or greater.", call. = FALSE)
                   },
                   loop = NULL,
                   type = c("Classification"),
                   parameters = data.frame(parameter = c("nsets", "ntreeperdiv", "ntreefinal"),
                                           class = c("numeric", "numeric", "numeric"),
                                           label = c("# score sets tried prior to the approximation",
                                                     "# of trees (small RFs)",
                                                     "# of trees (final RF)")),
                   grid = function(x, y, len = NULL, search = "grid"){
                     if(search == "grid"){
                       out <- expand.grid(nsets = seq_len(len)*50,
                                          ntreeperdiv = seq_len(len)*50,
                                          ntreefinal =  seq_len(len)*200)
                     } else {
                       out <-data.frame(nsets = sample(20, size = len, replace = TRUE)*50,
                                        ntreeperdiv =sample(20, size = len, replace = TRUE)*50,
                                        ntreefinal =  sample(2:20, size = len, replace = TRUE)*200
                       )
                     }
                     out
                   },
                   fit = function(x, y, wts, param, lev, last, classProbs, ...){
                     if((!is.data.frame(x))||dplyr::is.tbl(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
                     x$.outcome <- y
                     out <- ordinalForest::ordfor(depvar = ".outcome",
                                                  data = x,
                                                  nsets = param$nsets,
                                                  ntreeperdiv = param$ntreeperdiv,
                                                  ntreefinal =  param$ntreefinal,
                                                  ...)
                     out
                   },
                   predict = function(modelFit, newdata, submodels = NULL){
                     if((!is.data.frame(newdata))||dplyr::is.tbl(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                     out <- predict(modelFit, newdata)$ypred
                     out
                   },
                   prob = function(modelFit, newdata, submodels = NULL){
                     if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                     out <- predict(modelFit, newdata)$classprobs
                     colnames(out) <- modelFit$classes
                     as.data.frame(out)
                   },
                   predictors = function(x, ...){
                     var_index <- sort(unique(unlist(lapply(x$forestfinal$forest$split.varIDs, function(x) x))))
                     var_index <-var_index[var_index > 0]
                     x$forestfinal$forest$independent.variable.names[var_index]
                   },
                   varImp = function(object, ...){
                     if(length(object$varimp) == 0)
                       stop("No importance values available")
                     imps <- object$varimp
                     out <- data.frame(Overall = as.vector(imps))
                     rownames(out) <- names(imps)
                     out
                   },
                   levels = function(x){
                     if(x$treetype == "Classification"){
                       out <- levels(x$predictions)
                     } else out <- NULL
                     out
                   },
                   tags = c("Random Forest", "Ensemble Model", "Bagging",
                            "Implicit Feature Selection", "Ordinal Outcomes" ),
                   sort = function(x){ x[order(x[,1]),] }
)
