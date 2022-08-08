modelInfo <- list(label = "Random Forest",
                  library = c("e1071", "ranger", "dplyr"),
                  check = function(pkg) {
                    requireNamespace("ranger")
                    current <- packageDescription("ranger")$Version
                    expected <- "0.8.0"
                    if(compareVersion(current, expected) < 0)
                      stop("This modeling workflow requires ranger version ",
                           expected, "or greater.", call. = FALSE)
                  },
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c("mtry", "splitrule", "min.node.size"),
                                          class = c("numeric", "character", "numeric"),
                                          label = c("#Randomly Selected Predictors",
                                                    "Splitting Rule",
                                                    "Minimal Node Size")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      srule <-
                        if (is.factor(y))
                          "gini"
                      else
                        "variance"
                      out <- expand.grid(mtry =
                                           caret::var_seq(p = ncol(x),
                                                          classification = is.factor(y),
                                                          len = len),
                                         min.node.size = ifelse( is.factor(y), 1, 5),
                                         splitrule = c(srule, "extratrees"))
                    } else {
                      srules <- if (is.factor(y))
                        c("gini", "extratrees")
                      else
                        c("variance", "extratrees", "maxstat")
                      out <-
                        data.frame(
                          min.node.size= sample(1:(min(20,nrow(x))), size = len, replace = TRUE),
                          mtry = sample(1:ncol(x), size = len, replace = TRUE),
                          splitrule = sample(srules, size = len, replace = TRUE)
                        )
                    }
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if((!is.data.frame(x))||dplyr::is.tbl(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
                    x$.outcome <- y
                    if(!is.null(wts)) {
                      out <- ranger::ranger(dependent.variable.name = ".outcome",
                                            data = x,
                                            mtry = min(param$mtry, ncol(x)),
                                            min.node.size = param$min.node.size,
                                            splitrule = as.character(param$splitrule),
                                            write.forest = TRUE,
                                            probability = classProbs,
                                            case.weights = wts,
                                            ...)
                    } else {
                      out <- ranger::ranger(dependent.variable.name = ".outcome",
                                            data = x,
                                            mtry = min(param$mtry, ncol(x)),
                                            min.node.size = param$min.node.size,
                                            splitrule = as.character(param$splitrule),
                                            write.forest = TRUE,
                                            probability = classProbs,
                                            ...)
                    }
                    ## in case the resampling method is "oob"
                    if(!last) out$y <- y
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if((!is.data.frame(newdata))||dplyr::is.tbl(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, newdata)$predictions
                    if(!is.null(modelFit$obsLevels) & modelFit$treetype == "Probability estimation") {
                      out <- colnames(out)[apply(out, 1, which.max)]
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)$predictions
                  },
                  predictors = function(x, ...) {
                    var_index <- sort(unique(unlist(lapply(x$forest$split.varIDs, function(x) x))))
                    var_index <-var_index[var_index > 0]
                    x$forest$independent.variable.names[var_index]
                  },
                  varImp = function(object, ...){
                    if(length(object$variable.importance) == 0)
                      stop("No importance values available")
                    imps <- ranger:::importance(object)
                    out <- data.frame(Overall = as.vector(imps))
                    rownames(out) <- names(imps)
                    out
                  },
                  levels = function(x) {
                    if(x$treetype == "Probability estimation") {
                      out <- colnames(x$predictions)
                    } else {
                      if(x$treetype == "Classification") {
                        out <- levels(x$predictions)
                      } else out <- NULL
                    }
                    out
                  },
                  oob = function(x) {
                    postResample(x$predictions, x$y)
                  },
                  tags = c("Random Forest", "Ensemble Model", "Bagging",
                           "Implicit Feature Selection", "Accepts Case Weights"),
                  sort = function(x) x[order(x[,1]),])
