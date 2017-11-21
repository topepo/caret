modelInfo <- list(label = "Support Vector Machines with Class Weights",
                  library = "kernlab",
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('sigma', 'C', 'Weight'),
                                          class = c("numeric", "numeric", "numeric"),
                                          label = c('Sigma', "Cost", "Weight")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
                    if(search == "grid") {
                      out <- expand.grid(sigma = mean(as.vector(sigmas[-2])),
                                         C = 2 ^((1:len) - 3),
                                         Weight = 1:len)
                    } else {
                      rng <- extendrange(log(sigmas), f = .75)
                      out <- data.frame(sigma = exp(runif(len, min = rng[1], max = rng[2])),
                                        C = 2^runif(len, min = -5, max = 10),
                                        Weight = runif(len, min = 1, max = 25))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    if(param$Weight != 1) {
                      wts <- c(param$Weight, 1)
                      names(wts) <- levels(y)
                    } else wts <- NULL
                    
                    if(any(names(list(...)) == "prob.model") | is.numeric(y)) {
                      out <- kernlab::ksvm(x = as.matrix(x), y = y,
                                           kernel = "rbfdot",
                                           kpar = list(sigma = param$sigma),
                                           class.weights = wts,
                                           C = param$C, ...)
                    } else {
                      out <- kernlab::ksvm(x = as.matrix(x), y = y,
                                           kernel = "rbfdot",
                                          kpar = list(sigma = param$sigma),
                                          class.weights = wts,
                                          C = param$C,
                                          prob.model = classProbs,
                                          ...)
                    }
                    out            
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    out <- kernlab::predict(modelFit, newdata)
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
                  tags = c("Kernel Method", "Support Vector Machines", "Radial Basis Function", "Cost Sensitive Learning", "Two Class Only"),
                  levels = function(x) kernlab::lev(x),
                  sort = function(x) x[order(x$C, -x$sigma, x$Weight),])
