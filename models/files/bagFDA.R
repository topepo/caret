modelInfo <- list(label = "Bagged Flexible Discriminant Analysis",
                  library = c("earth", "mda"),
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c("degree", "nprune"),
                                          class = c("numeric", "numeric"),
                                          label = c('Product Degree', '#Terms')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    dat <- if(!is.data.frame(x)) as.data.frame(x, stringsAsFactors = TRUE) else x
                    dat$.outcome <- y

                    mod <- mda::fda( .outcome~., data = dat, method = earth::earth, pmethod = "none")
                    maxTerms <- nrow(mod$fit$dirs) - 1

                    maxTerms <- min(200, floor(maxTerms * .75) + 2)
                    if(search == "grid") {
                      out <- data.frame(nprune = unique(floor(seq(2, to = maxTerms, length = len))),
                                        degree = 1)
                    } else {
                      out <- data.frame(nprune = sample(2:maxTerms, size = len, replace = TRUE),
                                        degree = sample(1:2, size = len, replace = TRUE))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                  	require(earth)
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    bagFDA(.outcome ~ .,
                           data = dat,
                           degree = param$degree,
                           nprune = param$nprune,
                           weights = wts,
                           ...)
                  },
                  tags = c("Multivariate Adaptive Regression Splines", "Ensemble Model",
                           "Implicit Feature Selection", "Bagging", "Accepts Case Weights"),
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
                  notes = paste(
                    "Unlike other packages used by `train`, the `earth`",
                    "package is fully loaded when this model is used."
                  ),
                  levels = function(x) x$levels,
                  sort = function(x) x[order(x$degree, x$nprune),],
                  oob = function(x) apply(x$oob, 2, function(x) quantile(x, probs = .5)))
