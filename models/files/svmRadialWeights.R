modelInfo <- list(label = "Support Vector Machines with Class Weights",
                  library = "kernlab",
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('sigma', 'C', 'Weight'),
                                          class = c("numeric", "numeric", "numeric"),
                                          label = c('Sigma', "Cost", "Weight")),
                  grid = function(x, y, len = NULL) {
                    library(kernlab)
                    if(length(levels(y)) != 2) stop("This model is only available for two class problem. ")
                    sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)  
                    expand.grid(sigma = mean(as.vector(sigmas[-2])),
                                C = 2 ^((1:len) - 3),
                                Weight = 1:len)
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    if(param$Weight != 1) {
                      wts <- c(param$Weight, 1)
                      names(wts) <- levels(y)
                    } else wts <- NULL
                    
                    if(classProbs) warning(paste("kernlab does not currently use class weights when",
                                                 "estimating class probabilities"))
                    
                    if(any(names(list(...)) == "prob.model") | is.numeric(y))
                    {
                      out <- ksvm(x = as.matrix(x), y = y,
                                  kernel = rbfdot,
                                  kpar = list(sigma = param$sigma),
                                  class.weights = wts,
                                  C = param$C, ...)
                    } else {
                      out <- ksvm(x = as.matrix(x), y = y,
                                  kernel = rbfdot,
                                  kpar = list(sigma = param$sigma),
                                  class.weights = wts,
                                  C = param$C,
                                  prob.model = classProbs,
                                  ...)
                    }
                    
                    out            
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    predict(modelFit, newdata)
                  },
                  prob = NULL,
                  predictors = function(x, ...){
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
                  tags = c("Kernel Method", "Support Vector Machines", "Radial Basis Function", "Cost Sensitive Learning"),
                  levels = function(x) lev(x),
                  sort = function(x) x[order(x$C, -x$sigma, x$Weight),])
