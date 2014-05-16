modelInfo <- list(label = "Bagged Flexible Discriminant Analysis", 
                  library = c("earth", "mda"),
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = c("degree", "nprune"),
                                          class = c("numeric", "numeric"),
                                          label = c('Product Degree', '#Terms')),
                  grid = function(x, y, len = NULL) {
                    dat <- if(!is.data.frame(x)) as.data.frame(x) else x
                    dat$.outcome <- y
                    
                    mod <- fda( .outcome~., data = dat, method = earth, pmethod = "none")
                    maxTerms <- nrow(mod$fit$dirs) - 1
                    
                    maxTerms <- min(200, floor(maxTerms * .75) + 2)
                    data.frame(nprune = unique(floor(seq(2, to = maxTerms, length = len))),
                               degree = 1)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    bagFDA(.outcome ~ ., 
                           data = dat, 
                           degree = param$degree,
                           nprune = param$nprune, ...)
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
                    impDF <- as.data.frame(allImp)
                    meanImp <- apply(impDF, 1, mean)
                    out <- data.frame(Overall = meanImp)
                    rownames(out) <- names(meanImp)
                    out
                  },
                  levels = function(x) x$levels,
                  sort = function(x) x[order(x$degree, x$nprune),])
