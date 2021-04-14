modelInfo <- list(label = "Multivariate Adaptive Regression Spline",
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
                    if (!is.null(wts)) theDots$weights <- wts

                    modelArgs <- c(list(x = x, y = y,
                                        degree = param$degree,
                                        nprune = param$nprune),
                                   theDots)
                    if (is.factor(y) & !any(names(theDots) == "glm")) {
                      modelArgs$glm <- list(family = binomial, maxit = 100)
                    }

                    tmp <- do.call(earth::earth, modelArgs)

                    tmp$call["nprune"] <- param$nprune
                    tmp$call["degree"] <- param$degree
                    tmp
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if (modelFit$problemType == "Classification") {
                      out <- earth:::predict.earth(modelFit, newdata,  type = "class")
                    } else {
                      out <- earth:::predict.earth(modelFit, newdata)
                    }

                    if (!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- if (is.matrix(out)) out[,1] else out
                      for(j in seq(along = submodels$nprune)) {
                        prunedFit <- earth:::update.earth(modelFit, nprune = submodels$nprune[j])
                        if (modelFit$problemType == "Classification") {
                          tmp[[j+1]] <- earth:::predict.earth(prunedFit, newdata,  type = "class")
                        } else {
                          tmp[[j+1]] <- earth:::predict.earth(prunedFit, newdata)
                        }
                        if (is.matrix(tmp[[j+1]])) tmp[[j+1]] <- tmp[[j+1]][,1]
                      }
                      out <- tmp
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- earth:::predict.earth(modelFit, newdata, type= "response")
                    if (ncol(out) > 1) {
                      out <- t(apply(out, 1, function(x) x / sum(x)))
                    } else {
                      out <- cbind(1 - out[, 1], out[, 1])
                      colnames(out) <- modelFit$obsLevels
                    }
                    out <- as.data.frame(out, stringsAsFactors = TRUE)

                    if (!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out

                      for(j in seq(along = submodels$nprune)) {
                        prunedFit <- earth:::update.earth(modelFit, nprune = submodels$nprune[j])
                        tmp2 <- earth:::predict.earth(prunedFit, newdata, type= "response")
                        if (ncol(tmp2) > 1) {
                          tmp2 <- t(apply(tmp2, 1, function(x) x / sum(x)))
                        } else {
                          tmp2 <- cbind(1 - tmp2[, 1], tmp2[, 1])
                          colnames(tmp2) <- modelFit$obsLevels
                        }
                        tmp2 <- as.data.frame(tmp2, stringsAsFactors = TRUE)
                        tmp[[j+1]] <- tmp2
                      }
                      out <- tmp
                    }
                    out
                  },
                  predictors = function(x, ...) {
                    vi <- varImp(x)
                    notZero <- sort(unique(unlist(lapply(vi, function(x) which(x > 0)))))
                    if (length(notZero) > 0) rownames(vi)[notZero] else NULL
                  },
                  varImp = function(object, value = "gcv", ...) {
                    earthImp <- earth::evimp(object)
                    if (!is.matrix(earthImp)) earthImp <- t(as.matrix(earthImp))

                    # get other variable names and padd with zeros

                    out <- earthImp
                    perfCol <- which(colnames(out) == value)

                    increaseInd <- out[,perfCol + 1]
                    out <- as.data.frame(out[,perfCol, drop = FALSE], stringsAsFactors = TRUE)
                    colnames(out) <- "Overall"

                    # At this point, we still may have some variables
                    # that are not in the model but have non-zero
                    # importance. We'll set those to zero
                    if (any(earthImp[,"used"] == 0)) {
                      dropList <- grep("-unused", rownames(earthImp), value = TRUE)
                      out$Overall[rownames(out) %in% dropList] <- 0
                    }
                    rownames(out) <- gsub("-unused", "", rownames(out))
                    out <- as.data.frame(out, stringsAsFactors = TRUE)
                    # fill in zeros for any variabels not  in out

                    xNames <- object$namesx.org
                    if (any(!(xNames %in% rownames(out)))) {
                      xNames <- xNames[!(xNames %in% rownames(out))]
                      others <- data.frame(
                        Overall = rep(0, length(xNames)),
                        row.names = xNames)
                      out <- rbind(out, others)
                    }
                    out
                  },
                  levels = function(x) x$levels,
                  tags = c("Multivariate Adaptive Regression Splines",
                           "Implicit Feature Selection",
                           "Accepts Case Weights"),
                  notes = paste(
                    "Unlike other packages used by `train`, the `earth`",
                    "package is fully loaded when this model is used."
                  ),
                  sort = function(x) x[order(x$degree, x$nprune),])
