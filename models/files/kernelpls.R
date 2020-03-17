modelInfo <- list(label = "Partial Least Squares",
                  library = "pls",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = 'ncomp',
                                          class = "numeric",
                                          label = '#Components'),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- data.frame(ncomp = seq(1, min(ncol(x) - 1, len), by = 1))
                    } else {
                      out <- data.frame(ncomp = unique(sample(1:ncol(x), size = len, replace = TRUE)))
                    }
                    out
                  },
                  loop = function(grid) {
                    grid <- grid[order(grid$ncomp, decreasing = TRUE),, drop = FALSE]
                    loop <- grid[1,,drop = FALSE]
                    submodels <- list(grid[-1,,drop = FALSE])
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    ncomp <- min(ncol(x), param$ncomp)
                    out <- if(is.factor(y))
                    {
                      caret::plsda(x, y, method = "kernelpls", ncomp = ncomp, ...)
                    } else {
                      dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                      dat$.outcome <- y
                      pls::plsr(.outcome ~ ., data = dat, method = "kernelpls", ncomp = ncomp, ...)
                    }
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- if(modelFit$problemType == "Classification")
                    {
                      if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                      out <- predict(modelFit, newdata, type="class")

                    } else as.vector(pls:::predict.mvr(modelFit, newdata, ncomp = max(modelFit$ncomp)))

                    if(!is.null(submodels))
                    {
                      ## We'll merge the first set of predictions below
                      tmp <- vector(mode = "list", length = nrow(submodels))

                      if(modelFit$problemType == "Classification")
                      {
                        if(length(submodels$ncomp) > 1)
                        {
                          tmp <- as.list(predict(modelFit, newdata, ncomp = submodels$ncomp))
                        } else tmp <- list(predict(modelFit, newdata, ncomp = submodels$ncomp))

                      } else {
                        tmp <- as.list(
                          as.data.frame(
                            apply(predict(modelFit, newdata, ncomp = submodels$ncomp), 3, function(x) list(x)),
                            stringsAsFActors = FALSE
                          )
                        )
                      }

                      out <- c(list(out), tmp)
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    out <- predict(modelFit, newdata, type = "prob",  ncomp = modelFit$tuneValue$ncomp)
                    if(length(dim(out)) == 3){
                      if(dim(out)[1] > 1) {
                        out <- out[,,1]
                      } else {
                        out <- as.data.frame(t(out[,,1]), stringsAsFactors = TRUE)
                      }
                    }
                    if(!is.null(submodels))
                    {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out

                      for(j in seq(along = submodels$ncomp))
                      {
                        tmpProb <- predict(modelFit, newdata, type = "prob",  ncomp = submodels$ncomp[j])
                        if(length(dim(tmpProb)) == 3){
                          if(dim(tmpProb)[1] > 1) {
                            tmpProb <- tmpProb[,,1]
                          } else {
                            tmpProb <- as.data.frame(t(tmpProb[,,1]), stringsAsFactors = TRUE)
                          }
                        }

                        tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, drop = FALSE], stringsAsFactors = TRUE)
                      }
                      out <- tmp
                    }
                    out
                  },
                  varImp = function(object, estimate = NULL, ...) {
                  	library(pls)
                    modelCoef <- coef(object, intercept = FALSE, comps = 1:object$ncomp)
                    perf <- MSEP(object)$val

                    nms <- dimnames(perf)
                    if(length(nms$estimate) > 1) {
                      pIndex <- if(is.null(estimate)) 1 else which(nms$estimate == estimate)
                      perf <- perf[pIndex,,,drop = FALSE]
                    }
                    numResp <- dim(modelCoef)[2]

                    if(numResp <= 2) {
                      modelCoef <- modelCoef[,1,,drop = FALSE]
                      perf <- perf[,1,]
                      delta <- -diff(perf)
                      delta <- delta/sum(delta)
                      out <- data.frame(Overall = apply(abs(modelCoef), 1,
                                                        weighted.mean, w = delta))
                    } else {
                      perf <- -t(apply(perf[1,,], 1, diff))
                      perf <- t(apply(perf, 1, function(u) u/sum(u)))
                      out <- matrix(NA, ncol = numResp, nrow = dim(modelCoef)[1])

                      for(i in 1:numResp) {
                        tmp <- abs(modelCoef[,i,, drop = FALSE])
                        out[,i] <- apply(tmp, 1,  weighted.mean, w = perf[i,])
                      }
                      colnames(out) <- dimnames(modelCoef)[[2]]
                      rownames(out) <- dimnames(modelCoef)[[1]]
                    }
                    as.data.frame(out, stringsAsFactors = TRUE)
                  },
                  predictors = function(x, ...) rownames(x$projection),
                  levels = function(x) x$obsLevels,
                  tags = c("Partial Least Squares", "Feature Extraction", "Kernel Method", "Linear Classifier", "Linear Regression"),
                  sort = function(x) x[order(x[,1]),])
