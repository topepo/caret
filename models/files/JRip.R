modelInfo <- list(label = "Rule-Based Classifier",
                  library = "RWeka",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c("NumOpt", "NumFolds", "MinWeights"),
                                          class = c("numeric", "numeric", "numeric"),
                                          label = c("# Optimizations", "# Folds", "Min Weights")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    upperBound <- min(max(1, floor(nrow(x) / 2)), 50)
                    if(search == "grid"){
                      out <- expand.grid(NumOpt = 1:min(len, sqrt(upperBound)), NumFolds = 2:min(len + 1, upperBound), MinWeights = 1:min(len, sqrt(upperBound)))
                      if(len == 1){
                        out <- data.frame(NumOpt = 2, NumFolds = 3, MinWeights = 2)
                      }
                    } else {
                      out <- data.frame(NumOpt = round(exp(runif(5 * len, 0, log(len)))),
                                        NumFolds = round(exp(runif(5 * len, 0, log(upperBound)))),
                                        MinWeights = round(exp(runif(5 * len, 0, log(upperBound)))))
                      out <- unique(out)
                      out <- out[1:min(nrow(out), len), ]
                    }
                    return(out)
                  } ,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "control")) {
                      theDots$control$O <- param$NumOpt
                      theDots$control$F <- param$NumFolds
                      theDots$control$N <- param$MinWeights
                      ctl <- theDots$control
                      theDots$control <- NULL
                    } else ctl <- RWeka::Weka_control(O = param$NumOpt, F = param$NumFolds, N = param$MinWeights) 
                    
                    modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                                        data = dat,
                                        control = ctl),
                                   theDots)
                    
                    out <- do.call(RWeka::JRip, modelArgs) 
                    out      
                    },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
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
                  sort = function(x) x[order(x$NumOpt, x$NumFolds, x$MinWeights, decreasing = TRUE),])
