modelInfo <- list(label = "Rule-Based Classifier",
                  library = "RWeka",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = "NumOpt",
                                          class = "numeric",
                                          label = "# Optimizations"),
                  grid = function(x, y, len = NULL, search = "grid") data.frame(NumOpt = 1:len),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x)
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control")) {
                      theDots$control$N <- param$NumOpt 
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- Weka_control(N = param$NumOpt) 
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat,
                                        control = ctl),
                                   theDots)
                    
                    out <- do.call("JRip", modelArgs) 
                    out      
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata)
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata, type = "probability")
                  },
                  levels = function(x) x$obsLevels,
                  predictors = function(x, ...) predictors(x$terms),
                  tags = c("Rule-Based Model", "Implicit Feature Selection"),
                  varImp = function(object, ...) {
                    dat <- caret:::ripperRuleSummary(object)
                    out <- dat$varUsage[,"Overall", drop = FALSE]
                    rownames(out) <- dat$varUsage$Var
                    out
                  },
                  sort = function(x) x[order(x[,1], decreasing = TRUE),])
