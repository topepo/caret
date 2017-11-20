modelInfo <- list(label = "Support Vector Machines with Radial Basis Function Kernel",
                  library = "kernlab",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('sigma', 'C'),
                                          class = c("numeric", "numeric"),
                                          label = c('Sigma', "Cost")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
                    if(search == "grid") {
                      out <- expand.grid(sigma = mean(as.vector(sigmas[-2])),
                                         C = 2 ^((1:len) - 3))
                    } else {
                      rng <- extendrange(log(sigmas), f = .75)
                      out <- data.frame(sigma = exp(runif(len, min = rng[1], max = rng[2])),
                                        C = 2^runif(len, min = -5, max = 10))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    if(any(names(list(...)) == "prob.model") | is.numeric(y)) {
                      out <- kernlab::ksvm(x = as.matrix(x), y = y,
                                           kernel = "rbfdot",
                                           kpar = list(sigma = param$sigma),
                                           C = param$C, ...)
                    } else {
                      out <- kernlab::ksvm(x = as.matrix(x), y = y,
                                           kernel = "rbfdot",
                                           kpar = list(sigma = param$sigma),
                                           C = param$C,
                                           prob.model = classProbs,
                                           ...)
                    }
                    out            
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    svmPred <- function(obj, x) {
                      hasPM <- !is.null(unlist(obj@prob.model))
                      if(hasPM) {
                        pred <- kernlab::lev(obj)[apply(kernlab::predict(obj, x, type = "probabilities"), 
                                               1, which.max)]
                      } else pred <- kernlab::predict(obj, x)
                      pred
                    }
                    out <- try(svmPred(modelFit, newdata), silent = TRUE)
                    if(is.character(kernlab::lev(modelFit))) {
                      if(class(out)[1] == "try-error") {
                        warning("kernlab class prediction calculations failed; returning NAs")
                        out <- rep("", nrow(newdata))
                        out[seq(along = out)] <- NA
                      }
                    } else {
                      if(class(out)[1] == "try-error") {
                        warning("kernlab prediction calculations failed; returning NAs")
                        out <- rep(NA, nrow(newdata))
                      } 
                    }
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    out <- try(kernlab::predict(modelFit, newdata, type="probabilities"),
                               silent = TRUE)
                    if(class(out)[1] != "try-error") {
                      ## There are times when the SVM probability model will
                      ## produce negative class probabilities, so we
                      ## induce vlaues between 0 and 1
                      if(any(out < 0)) {
                        out[out < 0] <- 0
                        out <- t(apply(out, 1, function(x) x/sum(x)))
                      }
                      out <- out[, kernlab::lev(modelFit), drop = FALSE]
                    } else {
                      warning("kernlab class probability calculations failed; returning NAs")
                      out <- matrix(NA, nrow(newdata) * length(kernlab::lev(modelFit)), ncol = length(kernlab::lev(modelFit)))
                      colnames(out) <- kernlab::lev(modelFit)
                    }
                    out
                  },
                  predictors = function(x, ...){
                    if(hasTerms(x) & !is.null(x@terms)) {
                      out <- predictors.terms(x@terms)
                    } else {
                      out <- colnames(attr(x, "xmatrix"))
                    }
                    if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
                    if(is.null(out)) out <-NA
                    out
                  },
                  tags = c("Kernel Method", "Support Vector Machines", "Radial Basis Function",
                           "Robust Methods"),
                  levels = function(x) kernlab::lev(x),
                  sort = function(x) {
                    # If the cost is high, the decision boundary will work hard to
                    # adapt. Also, if C is fixed, smaller values of sigma yeild more
                    # complex boundaries
                    x[order(x$C, -x$sigma),]
                  })
