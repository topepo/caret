modelInfo <- list(label = "Boosted Generalized Linear Model",
                  library = c("plyr", "mboost"),
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('mstop', 'prune'),
                                          class = c("numeric", "character"),
                                          label = c('# Boosting Iterations', 'AIC Prune?')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(mstop = floor((1:len) * 50), prune = "no")
                    } else {
                      out <- data.frame(mstop = sample(1:1000, size = len, replace = TRUE),
                                        prune = sample(c("yes", "no"), size = len, replace = TRUE))
                    }
                  },
                  loop = function(grid) {
                    grid <- grid[order(-grid$mstop, grid$prune),]
                    loop <- plyr::ddply(grid, plyr::`.`(prune), function(x) data.frame(mstop = max(x$mstop)))
                    submodels <- vector(mode = "list", length = nrow(loop))
                    for(i in seq(along = loop$mstop)) {
                      submodels[[i]] <- subset(grid, prune == loop$prune[i] & mstop < loop$mstop[i])
                    }
                    list(loop = loop[, c("mstop", "prune")], submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    ##check for control list and over-write mstop
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                      theDots$control$mstop <- param$mstop
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- mboost::boost_control(mstop = param$mstop)

                    if(!any(names(theDots) == "family"))
                      theDots$family <- if(is.factor(y)) mboost::Binomial() else mboost::GaussReg()

                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts

                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat, control = ctl),
                                   theDots)
                    out <- do.call(mboost:::glmboost.formula, modelArgs)

                    ## from `?mstop`: The [.mboost function can be used to enhance or restrict a given
                    ## boosting model to the specified boosting iteration i. Note that in both cases the
                    ## original x will be changed to reduce the memory footprint. If the boosting model
                    ## is enhanced by specifying an index that is larger than the initial mstop, only
                    ## the missing i - mstop steps are fitted. If the model is restricted, the spare
                    ## steps are not dropped, i.e., if we increase i again, these boosting steps are
                    ## immediately available. Alternatively, the same operation can be done
                    ## by mstop(x) <- i.

                    if(param$prune == "yes") {
                      iters <- if(is.factor(y))
                        mboost::mstop(AIC(out, "classical")) else
                          mboost::mstop(AIC(out))
                      if(iters < out$mstop()) out <- out[iters]
                    }
                    out$.org.mstop <- out$mstop()

                    ## for easier printing (and tracebacks), we'll try to make the calls shorter
                    ## by adding dummy object names instead of the long object definitions that
                    ## currently exist
                    out$call["x"] <- "xData"
                    out$call["y"] <- "yData"
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predType <- ifelse(modelFit$problemType == "Classification", "class", "response")
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, newdata, type = predType)
                    if(!is.null(submodels)) {

                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- as.vector(out)
                      for(j in seq(along = submodels$mstop)) {
                        ## If the model has been pruned, make sure that the requested `mstop`
                        ## is not greater than the original value. If it is, use the orignal value.
                        ## This should only occur whenm the model was pruned .
                        this_mstop <- if(submodels$prune[j] == "yes" &
                                         submodels$mstop[j] > modelFit$.org.mstop)
                          modelFit$.org.mstop else submodels$mstop[j]
                        tmp[[j+1]]  <- as.vector(predict(modelFit[this_mstop],
                                                         newdata,
                                                         type = predType))
                      }
                      out <- tmp
                      mboost::mstop(modelFit) <- modelFit$.org.mstop
                    }
                    # cat(modelFit$mstop(), "!\n")
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    probs <- predict(modelFit, newdata, type = "response")
                    out <- cbind(1 - probs, probs)
                    colnames(out) <- modelFit$obsLevels
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      for(j in seq(along = submodels$mstop)) {
                        this_mstop <- if(submodels$prune[j] == "yes" &
                                         submodels$mstop[j] > modelFit$.org.mstop)
                          modelFit$.org.mstop else submodels$mstop[j]

                        tmpProb <- predict(modelFit[this_mstop], newdata, type = "response")
                        tmpProb <- cbind(1 - tmpProb, tmpProb)
                        colnames(tmpProb) <- modelFit$obsLevels
                        tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, drop = FALSE], stringsAsFactors = TRUE)
                      }
                      out <- tmp
                      mboost::mstop(modelFit) <- modelFit$.org.mstop
                    }
                    out
                  },
                  predictors = function(x, ...) {
                    strsplit(variable.names(x), ", ")[[1]]
                  },
                  varImp = function(object, ...) {
                    betas <- abs(coef(object))
                    betas <- betas[names(betas) != "(Intercept)"]
                    bnames <- names(betas)
                    name_check <- object$xName %in% bnames
                    if(any(!(name_check))) {
                      missing <- object$xName[!name_check]
                      beta_miss <- rep(0, length(missing))
                      names(beta_miss) <- missing
                      betas <- c(betas, beta_miss)
                    }
                    out <- data.frame(Overall = betas)
                    rownames(out) <- names(betas)
                    out
                  },
                  levels = function(x) levels(x$response),
                  notes = "The `prune` option for this model enables the number of iterations to be determined by the optimal AIC value across all iterations. See the examples in `?mboost::mstop`. If pruning is not used, the ensemble makes predictions using the exact value of the `mstop` tuning parameter value.",
                  tags = c("Generalized Linear Model", "Ensemble Model", "Boosting",
                           "Linear Classifier", "Two Class Only", "Accepts Case Weights"),
                  sort = function(x) x[order(x$mstop, x$prune),])
