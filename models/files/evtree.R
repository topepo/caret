modelInfo <- list(label = "Tree Models from Genetic Algorithms",
                  library = c("evtree"),
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('alpha'),
                                          class = c('numeric'),
                                          label = c('Complexity Parameter')),
                  grid = function(x, y, len = NULL)
                    data.frame(alpha = seq(0, 1, length = len)),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...){
                    dat <- x
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control"))
                    {
                      theDots$control$alpha <- param$alpha 
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- evtree.control(alpha = param$alpha)          
                    
                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat,
                                        control = ctl),
                                   theDots)
                    
                    out <- do.call("evtree", modelArgs)
                    out  
                  },
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata, type = "prob"),
                  tags = c("Tree-Based Model", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),])
