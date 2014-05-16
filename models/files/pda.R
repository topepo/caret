modelInfo <- list(label = "Penalized Discriminant Analysis",
                  library = "mda",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('lambda'),
                                          class = c('numeric'),
                                          label = c('Shrinkage Penalty Coefficient')),
                  grid = function(x, y, len = NULL) 
                    data.frame(lambda = 1:len),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    if(!is.null(wts))
                    {
                      out <- fda(as.formula(".outcome ~ ."),
                                 data = dat,
                                 method = gen.ridge,
                                 weights = wts,
                                 lambda = param$lambda,
                                 ...)
                    } else {
                      out <- fda(as.formula(".outcome ~ ."),
                                 data = dat,
                                 method = gen.ridge,
                                 lambda = param$lambda,
                                 ...)
                    }
                    out                   
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "posterior"),
                  tags = c("Discriminant Analysis", "Polynomial Model"),
                  sort = function(x) x[order(x[,1]),])
