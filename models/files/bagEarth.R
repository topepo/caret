modelInfo <- list(label = "Bagged MARS",
                  library = "earth",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('nprune', 'degree'),
                                          class = c("numeric", "numeric"),
                                          label = c('#Terms', 'Product Degree')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    dat <- if (is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y

                    mod <- earth::earth( .outcome~., data = dat, pmethod = "none")
                    maxTerms <- nrow(mod$dirs)

                    maxTerms <- min(200, floor(maxTerms * .75) + 2)
                    if (search == "grid") {
                      out <- data.frame(nprune = unique(floor(seq(2, to = maxTerms, length = len))),
                                        degree = 1)
                    } else {
                      out <- data.frame(nprune = sample(2:maxTerms, size = len, replace = TRUE),
                                        degree = sample(1:2, size = len, replace = TRUE))
                    }
                  },
                  loop = function(grid) {
                    deg <- unique(grid$degree)

                    loop <- data.frame(degree = deg)
                    loop$nprune <- NA

                    submodels <- vector(mode = "list", length = length(deg))
                    for (i in seq(along = deg)) {
                      np <- grid[grid$degree == deg[i],"nprune"]
                      loop$nprune[loop$degree == deg[i]] <- np[which.max(np)]
                      submodels[[i]] <- data.frame(nprune = np[-which.max(np)])
                    }
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    require(earth)
                    theDots <- list(...)
                    theDots$keepxy <- TRUE

                    ## pass in any model weights
                    if (!is.null(wts))
                      theDots$weights <- wts

                    modelArgs <- c(list(x = x, y = y,
                                        degree = param$degree,
                                        nprune = param$nprune),
                                   theDots)

                    if (is.factor(y) & !any(names(theDots) == "glm")) {
                      modelArgs$glm <- list(family = binomial, maxit = 100)
                    }

                    tmp <- do.call(getFromNamespace("bagEarth.default", "caret"), modelArgs)

                    tmp$call["nprune"] <-  param$nprune
                    tmp$call["degree"] <-  param$degree
                    tmp
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if (modelFit$problemType == "Classification") {
                      out <- predict(modelFit, newdata,  type = "class")
                    } else {
                      out <- predict(modelFit, newdata)
                    }

                    if (!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- if (is.matrix(out)) out[,1] else out

                      for (j in seq(along = submodels$nprune)) {
                        prunedFit <- update(modelFit, nprune = submodels$nprune[j])
                        if (modelFit$problemType == "Classification") {
                          tmp[[j+1]]  <-  predict(prunedFit, newdata,  type = "class")
                        } else {
                          tmp[[j+1]]  <-  predict(prunedFit, newdata)
                        }
                      }

                      out <- tmp
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- predict(modelFit, newdata, type= "prob")
                    if (!is.null(submodels))  {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out

                      for(j in seq(along = submodels$nprune))  {
                        prunedFit <- update(modelFit, nprune = submodels$nprune[j])
                        tmp2 <- predict(prunedFit, newdata, type= "prob")
                        tmp[[j+1]] <- tmp2
                      }
                      out <- tmp
                    }
                    out
                  },
                  predictors = function(x, ...) {
                    predEarth <- function(x) {
                      vi <- varImp(x)
                      notZero <- sort(unique(unlist(lapply(vi, function(x) which(x > 0)))))
                      if (length(notZero) > 0) rownames(vi)[notZero] else NULL
                    }
                    eachFit <- lapply(x$fit, predEarth)
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
                  sort = function(x) x[order(x$degree, x$nprune),],
                  oob = function(x) apply(x$oob, 2, function(x) quantile(x, probs = .5)))
