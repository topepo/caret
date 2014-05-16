modelInfo <- list(label = "Least Squares Support Vector Machine with Radial Basis Function Kernel",
                  library = "kernlab",
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('sigma'),
                                          class = c("numeric"),
                                          label = c('Sigma')),
                  grid = function(x, y, len = NULL) {
                    library(kernlab)
                    sigmas <- sigest(as.matrix(x), na.action = na.omit, scaled = TRUE)  
                    expand.grid(sigma = mean(sigmas[-2]))
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
                    lssvm(x = as.matrix(x), y = y,
                          kernel = rbfdot,
                          kpar = list(sigma = param$sigma), ...)         
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {  
                    out <- predict(modelFit, as.matrix(newdata))
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
