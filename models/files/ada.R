modelInfo <- list(label = "Boosted Classification Trees",
                  library = "ada",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('iter', 'maxdepth', 'nu'),
                                          class = rep("numeric", 3),
                                          label = c('#Trees', 'Max Tree Depth', 'Learning Rate')),
                  grid = function(x, y, len = NULL) expand.grid(iter = floor((1:len) * 50),
                                                                maxdepth = seq(1, len),         
                                                                nu = .1),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control"))
                    {
                      theDots$control$maxdepth <- param$maxdepth 
                      ctl <- theDots$control
                      theDots$control <- NULL
                      
                    } else ctl <- rpart.control(maxdepth = param$maxdepth) 
                    
                    modelArgs <- c(list(x = x,
                                        y = y,
                                        iter = param$iter,
                                        nu = param$nu,              
                                        control = ctl),
                                   theDots)
                    
                    out <- do.call("ada", modelArgs)
                    
#                     out$call["x"] <- "xData"         
#                     out$call["y"] <- "yData"  
                    
                    out     
                  },
                  predict = function(modelFit, newdata, submodels = NULL)
                    predict(modelFit, newdata),
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, newdata, type = "prob")
                    colnames(out) <-  modelFit$obsLevels
                    out
                  },
                  tags = c("Tree-Based Model", "Ensemble Model", "Boosting", 
                           "Implicit Feature Selection"),
                  sort = function(x) x[order(x$iter, x$maxdepth, x$nu),])
