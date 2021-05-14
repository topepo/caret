modelInfo <- list(label = "Bagged FDA using gCV Pruning",
                  library = "earth",
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('degree'),
                                          class = c("numeric"),
                                          label = c('Product Degree')),
                  grid = function(x, y, len = NULL, search = "grid")  data.frame(degree = 1),
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                  	require(earth)
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    caret::bagFDA(.outcome ~ .,
                                  data = dat,
                                  degree = param$degree,
                                  weights = wts,
                                  ...)
                  },
                  tags = c("Multivariate Adaptive Regression Splines", "Ensemble Model",
                           "Implicit Feature Selection", "Bagging"),
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit , newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type= "probs"),
                  predictors = function(x, ...) {
                    fdaPreds <- function(x) {
                      code <- getModelInfo("earth", regex = FALSE)[[1]]$predictors
                      tmp <- predictors(x$terms)
                      out <- if(class(x$fit) == "earth") code(x$fit) else tmp
                      out
                    }
                    eachFit <- lapply(x$fit, fdaPreds)
                    unique(unlist(eachFit))
                  },
                  varImp = function(object, ...) {
                    allImp <- lapply(object$fit, varImp, ...)
                    allImp <- lapply(allImp,
                                     function (x) {
                                       x$var <- rownames(x)
                                       x
                                     },
                                     ...)
                    allImp <- do.call("rbind", allImp)

                    impDF <- plyr::ddply(allImp, .(var), function(x) c(Overall = mean(x$Overall, rm.na = TRUE)))
                    out <- data.frame(Overall = impDF$Overall)
                    rownames(out) <- impDF$var
                    out
                  },
                  levels = function(x) x$levels,
                  tags = c("Multivariate Adaptive Regression Splines", "Ensemble Model",
                           "Implicit Feature Selection", "Bagging", "Accepts Case Weights"),
                  notes = paste(
                    "Unlike other packages used by `train`, the `earth`",
                    "package is fully loaded when this model is used."
                  ),
                  sort = function(x) x[order(x$degree),],
                  oob = function(x) apply(x$oob, 2, function(x) quantile(x, probs = .5)))
