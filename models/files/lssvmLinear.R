modelInfo <- list(label = "Least Squares Support Vector Machine",
                  library = "kernlab",
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('tau'),
                                          class = c("numeric"),
                                          label = c('Regularization Parameter')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(tau = 2 ^((1:len) - 5))
                    } else {
                      out <- data.frame(tau = 2^runif(len, min = -5, max = 10))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    kernlab::lssvm(x = as.matrix(x), y = y,
                                   tau = param$tau,
                                   kernel = kernlab::polydot(degree = 1,
                                                             scale = 1,
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
                    if(is.null(out)) out <- names(attr(x, "scaling")$x.scale$`scaled:center`)
                    if(is.null(out)) out <-NA
                    out
                  },
                  tags = c("Kernel Method", "Support Vector Machines", "Linear Classifier"),
                  levels = function(x) lev(x),
                  sort = function(x) x)
