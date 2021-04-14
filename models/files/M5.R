modelInfo <- list(label = "Model Tree",
                  library = "RWeka",
                  loop = NULL,
                  type = "Regression",
                  parameters = data.frame(parameter = c('pruned', 'smoothed', 'rules'),
                                          class = rep("character", 3),
                                          label = c('Pruned', 'Smoothed', 'Rules')),
                  grid = function(x, y, len = NULL, search = "grid") expand.grid(pruned = c("Yes", "No"), 
                                                                smoothed = c("Yes", "No"), 
                                                                rules = c("Yes", "No")),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control"))
                    {
                      theDots$control$N <- ifelse(param$pruned == "No", TRUE, FALSE)
                      theDots$control$U <- ifelse(param$smoothed == "No", TRUE, FALSE)
                      ctl <- theDots$control
                      theDots$control <- NULL
                      
                    } else ctl <- RWeka::Weka_control(N = ifelse(param$pruned == "No", TRUE, FALSE),
                                               U = ifelse(param$smoothed == "No", TRUE, FALSE)) 
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        control = ctl),
                                   theDots)
                    modelArgs$data <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    modelArgs$data$.outcome <- y
                    
                    out <- do.call(if(param$rules == "Yes") RWeka::M5Rules else RWeka::M5P, modelArgs) 
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)
                  },
                  prob = NULL,
                  predictors = function(x, ...) predictors(x$terms),
                  tags = c("Rule-Based Model", "Tree-Based Model", "Linear Regression", "Implicit Feature Selection",
                           "Model Tree"),
                  sort = function(x) {
                    x$pruned <- factor(as.character(x$pruned), levels = c("Yes", "No"))
                    x$smoothed <- factor(as.character(x$smoothed), levels = c("Yes", "No"))
                    x$rules <- factor(as.character(x$rules), levels = c("Yes", "No"))
                    x[order(x$pruned, x$smoothed, x$rules),]
                  })
