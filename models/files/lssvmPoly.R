modelInfo <- list(label = "Least Squares Support Vector Machine with Polynomial Kernel",
                  library = "kernlab",
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('degree', 'scale', 'tau'),
                                          class = c("numeric", "numeric", "numeric"),
                                          label = c('Polynomial Degree', 'Scale', 'Regularization Parameter')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(degree = seq(1, min(len, 3)),      
                                         scale = 10 ^((1:len) - 4),
                                         tau = 2 ^((1:len) - 5))
                    } else {
                      out <- data.frame(degree = sample(1:3, size = len, replace = TRUE),
                                        scale = 10^runif(len, min = -5, log10(2)),
                                        tau = 2^runif(len, min = -5, max = 10))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    kernlab::lssvm(x = as.matrix(x), y = y,
                      	          tau = param$tau,
                                  kernel = kernlab::polydot(degree = param$degree,
                                                            scale = param$scale,
                                                            offset = 1), ...)         
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
                    if(is.null(out)) out <- names(attr(x, "scaling")$xscale$`scaled:center`)
                    if(is.null(out)) out <-NA
                    out
                  },
                  tags = c("Kernel Method", "Support Vector Machines", "Polynomial Model"),
                  levels = function(x) lev(x),
                  sort = function(x) x)
