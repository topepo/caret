modelInfo <- list(label = "Tree Models from Genetic Algorithms",
                  library = c("evtree"),
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('alpha'),
                                          class = c('numeric'),
                                          label = c('Complexity Parameter')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(alpha = seq(1, 3, length = len)) 
                    } else {
                      out <- data.frame(alpha = runif(len, min = 1, max = 5))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...){
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control"))
                    {
                      theDots$control$alpha <- param$alpha 
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- evtree::evtree.control(alpha = param$alpha)

                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat,
                                        control = ctl),
                                   theDots)

                    out <- do.call(evtree::evtree, modelArgs)
                    out
                  },
                  levels = function(x) x$obsLevels,
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)
                    },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata, type = "prob")
                    },
                  tags = c("Tree-Based Model", "Implicit Feature Selection", "Accepts Case Weights"),
                  sort = function(x) x[order(x[,1]),])
