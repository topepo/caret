modelInfo <- list(label = "Penalized Discriminant Analysis",
                  library = "mda",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('df'),
                                          class = c('numeric'),
                                          label = c('Degrees of Freedom')),
                  grid = function(x, y, len = NULL) 
                    data.frame(df = 2* (0:(len - 1) + 1)),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    if(!is.null(wts))
                    {
                      out <- fda(as.formula(".outcome ~ ."),
                                 data = dat,
                                 method = gen.ridge,
                                 weights = wts,
                                 df = param$df,
                                 ...)
                    } else {
                      out <- fda(as.formula(".outcome ~ ."),
                                 data = dat,
                                 method = gen.ridge,
                                 df = param$df,
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
