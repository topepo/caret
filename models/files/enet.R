modelInfo <- list(label = "Elasticnet",
                  library = "elasticnet",
                  type = "Regression",
                  parameters = data.frame(parameter = c('fraction', 'lambda'),
                                          class = c("numeric", "numeric"),
                                          label = c('Fraction of Full Solution', 'Weight Decay')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)),
                                         fraction = seq(0.05, 1, length = len))
                    } else {
                      out <- data.frame(lambda = 10^runif(len, min = -5, 1),
                                        fraction = runif(len, min = 0, max = 1))
                    }
                    out
                  },
                  loop = function(grid) {
                    grid <- grid[order(grid$lambda, grid$fraction, decreasing = TRUE),, drop = FALSE]
                    uniqueLambda <- unique(grid$lambda)
                    loop <- data.frame(lambda = uniqueLambda)
                    loop$fraction <- NA

                    submodels <- vector(mode = "list", length = length(uniqueLambda))

                    for(i in seq(along = uniqueLambda))
                    {
                      subFrac <- grid[grid$lambda == uniqueLambda[i],"fraction"]
                      loop$fraction[loop$lambda == uniqueLambda[i]] <- subFrac[which.max(subFrac)]
                      submodels[[i]] <- data.frame(fraction = subFrac[-which.max(subFrac)])
                    }
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    elasticnet::enet(as.matrix(x), y, lambda = param$lambda)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- elasticnet::predict.enet(modelFit, newdata,
                                   s = modelFit$tuneValue$fraction,
                                   mode = "fraction")$fit

                    if(!is.null(submodels))
                    {
                      if(nrow(submodels) > 1)
                      {
                        out <- c(
                          list(if(is.matrix(out)) out[,1]  else out),
                          as.list(
                            as.data.frame(
                              elasticnet::predict.enet(modelFit,
                                                       newx = newdata,
                                                       s = submodels$fraction,
                                                       mode = "fraction")$fit,
                              stringsAsFactor = FALSE
                            )
                          )
                        )

                      } else {
                        tmp <- elasticnet::predict.enet(modelFit,
                                       newx = newdata,
                                       s = submodels$fraction,
                                       mode = "fraction")$fit
                        out <- c(list(if(is.matrix(out)) out[,1]  else out),  list(tmp))
                      }
                    }
                    out
                  },
                  predictors = function(x, s = NULL, ...) {
                    if(is.null(s))
                    {
                      if(!is.null(x$tuneValue))
                      {
                        s <- x$tuneValue$fraction
                      } else stop("must supply a vaue of s")
                      out <- elasticnet::predict.enet(x, s = s,
                                     type = "coefficients",
                                     mode = "fraction")$coefficients

                    } else {
                      out <- elasticnet::predict.enet(x, s = s)$coefficients

                    }
                    names(out)[out != 0]
                  },
                  tags = c("Linear Regression", "Implicit Feature Selection", "L1 Regularization"),
                  prob = NULL,
                  sort = function(x) x[order(x$fraction, -x$lambda),])
