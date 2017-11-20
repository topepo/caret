modelInfo <- list(label = "Least Squares Support Vector Machine with Radial Basis Function Kernel",
                  library = "kernlab",
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('sigma', 'tau'),
                                          class = c("numeric","numeric"),
                                          label = c('Sigma', 'Regularization Parameter')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    sigmas <- kernlab::sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)
                    if(search == "grid") {
                      out <- expand.grid(sigma = seq(min(sigmas), max(sigmas), length = min(6, len)),
                                         tau = 2 ^((1:len) - 5))
                    } else {
                      rng <- extendrange(log(sigmas), f = .75)
                      out <- data.frame(sigma = exp(runif(len, min = rng[1], max = rng[2])),
                                        tau = 2^runif(len, min = -5, max = 10))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    kernlab::lssvm(x = as.matrix(x), y = y,
                                   tau = param$tau,
                                   kernel = "rbfdot",
                                   kpar = list(sigma = param$sigma), ...)         
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {  
                    out <- kernlab::predict(modelFit, as.matrix(newdata))
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = NULL,
                  predictors = function(x, ...) {
                    if(hasTerms(x) & !is.null(x@terms))
                    {
                      out <- predictors.terms(x@terms)
                    } else {
                      out <- colnames(attr(x, "xmatrix"))
                    }
                    if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
                    if(is.null(out)) out <-NA
                    out
                  },
                  tags = c("Kernel Method", "Support Vector Machines", "Radial Basis Function"),
                  levels = function(x) lev(x),
                  sort = function(x) x)
