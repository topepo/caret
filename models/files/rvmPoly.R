modelInfo <- list(label = "Relevance Vector Machines with Polynomial Kernel",
                  library = "kernlab",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = c("scale", "degree"),
                                          class = c("numeric", "numeric"),
                                          label = c("Scale", "Polynomial Degree")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- expand.grid(degree = seq(1, min(len, 3)),      
                                         scale = 10 ^((1:len) - 4))
                    } else {
                      out <- data.frame(degree = sample(1:3, size = len, replace = TRUE),
                                        scale = 10^runif(len, min = -5, 0))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    kernlab:::rvm(x = as.matrix(x), y = y,
                                  kernel = kernlab::polydot(
                                    degree = param$degree,
                                    scale = param$scale,
                                    offset = 1),
                                  ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    kernlab::predict(modelFit, newdata),
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
                  tags = c("Kernel Method", "Relevance Vector Machines", "Polynomial Model",
                           "Robust Methods"),
                  sort = function(x) x[order(x$degree, x$scale),])
