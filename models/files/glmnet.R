modelInfo <- list(label = "glmnet",
                  library = "glmnet",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('alpha', 'lambda'),
                                          class = c("numeric", "numeric"),
                                          label = c('Mixing Percentage', 'Regularization Parameter')),
                  grid = function(x, y, len = NULL) 
                    expand.grid(alpha = seq(0.1, 1, length = len),
                                lambda = seq(.1, 3, length = 3 * len)),
                  loop = function(grid) {  
                    alph <- unique(grid$alpha)
                    loop <- data.frame(alpha = alph)
                    loop$lambda <- NA
                    submodels <- vector(mode = "list", length = length(alph))
                    for(i in seq(along = alph)) {
                      np <- grid[grid$alpha == alph[i],"lambda"]
                      loop$lambda[loop$alpha == alph[i]] <- np[which.max(np)]
                      submodels[[i]] <- data.frame(lambda = np[-which.max(np)])
                    }  
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    numLev <- if(is.character(y) | is.factor(y)) length(levels(y)) else NA
                    
                    theDots <- list(...)
                    
                    if(all(names(theDots) != "family")) {
                      if(!is.na(numLev)) {
                        fam <- ifelse(numLev > 2, "multinomial", "binomial")
                      } else fam <- "gaussian"    
                      theDots$family <- fam   
                    }
                    
                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts
                    
                    modelArgs <- c(list(x = as.matrix(x),
                                        y = y,
                                        alpha = param$alpha),
                                   theDots)
                    
                    out <- do.call("glmnet", modelArgs) 
                    if(!is.na(param$lambda[1])) out$lambdaOpt <- param$lambda[1]
                    out 
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
                    if(length(modelFit$obsLevels) < 2) {
                      out <- predict(modelFit, newdata, s = modelFit$lambdaOpt)
                    } else {
                      out <- predict(modelFit, newdata, s = modelFit$lambdaOpt, type = "class")
                    }
                    if(is.matrix(out)) out <- out[,1]
                      
                    if(!is.null(submodels)) {
                      if(length(modelFit$obsLevels) < 2) {
                        tmp <- as.list(as.data.frame(predict(modelFit, newdata, s = submodels$lambda)))
                      } else {
                        tmp <- predict(modelFit, newdata, s = submodels$lambda, type = "class")
                        tmp <- if(is.matrix(tmp)) as.data.frame(tmp, stringsAsFactors = FALSE) else as.character(tmp)
                        tmp <- as.list(tmp)
                      }
                      out <- c(list(out), tmp)
                    } 
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    obsLevels <- if("classnames" %in% names(modelFit)) modelFit$classnames else NULL
                    probs <- predict(modelFit,
                                     as.matrix(newdata),
                                     s = modelFit$lambdaOpt,
                                     type = "response")
                    if(length(obsLevels) == 2) {
                      probs <- as.vector(probs)
                      probs <- as.data.frame(cbind(1-probs, probs))
                      colnames(probs) <- modelFit$obsLevels
                    } else {
                      probs <- as.data.frame(probs[,,1,drop = FALSE])
                      names(probs) <- modelFit$obsLevels
                    }
                    if(!is.null(submodels)) {
                      tmp <- predict(modelFit,
                                     as.matrix(newdata),
                                     s = submodels$lambda,
                                     type = "response")
                      if(length(obsLevels) == 2) {
                        tmp <- as.list(as.data.frame(tmp))
                        tmp <- lapply(tmp,
                                      function(x, lev) {
                                        x <- as.vector(x)
                                        tmp <- data.frame(1-x, x)
                                        names(tmp) <- lev
                                        tmp
                                      },
                                      lev = modelFit$obsLevels)
                      } else tmp <- apply(tmp, 3, function(x) data.frame(x))
                      probs <- if(is.list(tmp)) c(list(probs), tmp) else list(probs, tmp)
                    }
                    probs
                  },
                  predictors = function(x, lambda = NULL, ...) {
                    if(is.null(lambda))
                    {
                      if(length(lambda) > 1) stop("Only one value of lambda is allowed right now")
                      if(!is.null(x$lambdaOpt)) {
                        lambda <- x$lambdaOpt
                      } else stop("must supply a vaue of lambda")
                    }
                    allVar <- if(is.list(x$beta)) rownames(x$beta[[1]]) else rownames(x$beta)
                    out <- unlist(predict(x, s = lambda, type = "nonzero"))
                    out <- unique(out)
                    if(length(out) > 0) {
                      out <- out[!is.na(out)]
                      out <- allVar[out]
                    }
                    out
                  },
                  varImp = function(object, lambda = NULL, ...) {
                    if(is.null(lambda)) {
                      if(length(lambda) > 1) stop("Only one value of lambda is allowed right now")
                      if(!is.null(object$lambdaOpt)) {
                        lambda <- object$lambdaOpt
                      } else stop("must supply a vaue of lambda")
                    }
                    beta <- predict(object, s = lambda, type = "coef")
                    if(is.list(beta)) {
                      out <- do.call("cbind", lapply(beta, function(x) x[,1]))
                      out <- as.data.frame(out)
                    } else out <- data.frame(Overall = beta[,1])
                    out <- out[rownames(out) != "(Intercept)",,drop = FALSE]
                    out
                  },
                  levels = function(x) if(any(names(x) == "obsLevels")) x$obsLevels else NULL,
                  tags = c("Generalized Linear Model", "Implicit Feature Selection", 
                           "L1 Regularization", "L2 Regularization", "Linear Classifier",
                           "Linear Regression"),
                  sort = function(x) x[order(-x$lambda, x$alpha),])
