modelInfo <- list(label = "MetaForest",
                  library = c("metaforest"),
                  check = function(pkg) {
                    requireNamespace("metaforest")
                    current <- packageDescription("metaforest")$Version
                    expected <- "0.1.0"
                    if(compareVersion(current, expected) < 0)
                      stop("This modeling workflow requires metaforest version ",
                           expected, "or greater.", call. = FALSE)
                  },
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = c("whichweights", "mtry", "min.node.size"), # Add random, unif, fixed
                                          class = c("character", "numeric", "numeric"),
                                          label = c("Type of weights to use", "#Randomly Selected Predictors",
                                                    "Minimal Node Size")),

                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(whichweights = "random",
                                         mtry = caret::var_seq(p = ncol(x),
                                                         classification = FALSE,
                                                         len = len),
                                         min.node.size = 5)
                    } else {
                      out <-
                        data.frame(
                          whichweights = "random",
                          mtry = sample(1:ncol(x), size = len, replace = TRUE),
                          min.node.size = sample(1:(min(20,nrow(x))),
                                                 size = len, replace = TRUE)
                        )
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.data.frame(x)) x <- as.data.frame(x)
                    x$.outcome <- y
                    #x$.caseweights <- wts
                    if(!hasArg(study)) {
                      out <- metaforest::MetaForest(.outcome ~ .,
                                            data = x,
                                            whichweights = param$whichweights,
                                            mtry = param$mtry,
                                            min.node.size = param$min.node.size,
                                            ...)
                    } else {
                      out <- metaforest::ClusterMF(.outcome ~ .,
                                                   data = x,
                                                   whichweights = param$whichweights,
                                                   mtry = param$mtry,
                                                   min.node.size = param$min.node.size,
                                                   ...)
                      out
                    }
                    ## in case the resampling method is "oob"
                    if(!last) out$y <- y
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if((!is.data.frame(newdata))||dplyr::is.tbl(newdata)) newdata <- as.data.frame(newdata)
                    out <- predict(modelFit, newdata)$predictions
                    out
                  },
                  prob = NULL,
                  predictors = function(x, ...) {
                    var_index <- sort(unique(unlist(lapply(x$forest$forest$split.varIDs, function(x) x))))
                    var_index <-var_index[var_index > 0]
                    x$forest$forest$independent.variable.names[var_index]
                  },
                  varImp = function(object, ...){
                    if(length(object$forest$variable.importance) == 0)
                      stop("No importance values available")
                    imps <- object$forest$variable.importance
                    out <- data.frame(Overall = as.vector(imps))
                    rownames(out) <- names(imps)
                    out
                  },
                  levels = function(x) {
                    out <- NULL
                    out
                  },
                  oob = function(x) {
                    postResample(x$predictions, x$y)
                  },
                  tags = c("Random Forest", "Meta-Analysis", "Ensemble Model", "Bagging", "Implicit Feature Selection", "Accepts Case Weights"),
                  sort = function(x){ x[order(x[,1]),] },
                  notes = "Metaforest does not use the train methods for classes 'formula' or 'recipe', because the variance of the effect size must be a column of the training data x. The name of this column is specified using the argument 'vi'. To train a clustered MetaForest, simply provide the optional argument 'study' to specify the study ID. This should again refer to a column of x. When training a clustered MetaForest, make sure to use 'index = groupKFold(your_study_id_variable, k = 10))' in traincontrol, to sample by study ID when creating cross-validation partitions; otherwise the testing error will be positively biased."
                  )
