modelInfo <- list(label = "Gaussian Process with Polynomial Kernel",
                  library = "kernlab",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('degree', 'scale'),
                                          class = c("numeric", "numeric"),
                                          label = c('Polynomial Degree', 'Scale')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(degree = seq(1, min(len, 3)),      
                                         scale = 10 ^((1:len) - 4))
                    } else {
                      out <- data.frame(degree = sample(1:3, size = len, replace = TRUE),
                                        scale = 10^runif(len, min = -5, 0))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    kernlab::gausspr(x = as.matrix(x), y = y,
                                     kernel = kernlab::polydot(degree = param$degree,
                                                       scale = param$scale,
                                                       offset = 1), ...)         
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {  
                    out <- kernlab::predict(modelFit, as.matrix(newdata))
                    if(is.matrix(out)) out <- out[,1]
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    kernlab::predict(modelFit, as.matrix(newdata), type = "probabilities")
                  },
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
                  tags = c("Kernel Method", "Gaussian Process", "Polynomial Model"),
                  levels = function(x) lev(x),
                  sort = function(x) x)
