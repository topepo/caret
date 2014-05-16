modelInfo <- list(label = "Conditional Inference Random Forest",
                  library = "party",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = 'mtry',
                                          class = 'numeric',
                                          label = "#Randomly Selected Predictors"),
                  grid = function(x, y, len = NULL) {
                    p <- ncol(x) 
                    tuneSeq <- if(p <= len) floor(seq(2, to = p, length = p))
                    else  seq(from = 2, by = 2, length.out = len)
                    if(any(table(tuneSeq) > 1))
                    {
                      tuneSeq <- unique(tuneSeq)
                      cat("note: only", length(tuneSeq), 
                          "unique complexity parameters in default grid.",
                          "Truncating the grid to", length(tuneSeq), ".\n\n")      
                    }
                    data.frame(mtry = tuneSeq)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- x
                    dat$.outcome <- y
                    theDots <- list(...)
                    
                    if(any(names(theDots) == "controls"))
                    {
                      theDots$controls@gtctrl@mtry <- as.integer(param$mtry) 
                      ctl <- theDots$controls
                      theDots$controls <- NULL
                      
                    } else ctl <- cforest_control(mtry = param$mtry)
                    
                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts
                    
                    modelArgs <- c(list(formula = as.formula(.outcome ~ .),
                                        data = dat,
                                        controls = ctl),
                                   theDots)
                    
                    out <- do.call(getFromNamespace("cforest", "party"), modelArgs)
                    out 
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    ## party builds the levels into the model object, so I'm
                    ## going to assume that all the levels will be passed to
                    ## the output
                    out <- predict(modelFit, newdata, OOB = TRUE)
                    if(is.matrix(out)) out <- out[,1]
                    if(!is.null(modelFit@responses@levels$.outcome)) out <- as.character(out)
                    
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    obsLevels <- levels(modelFit@data@get("response")[,1])
                    rawProbs <- treeresponse(modelFit, newdata)
                    probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), byrow = TRUE)
                    out <- data.frame(probMatrix)
                    colnames(out) <- obsLevels
                    rownames(out) <- NULL
                    out
                  },
                  predictors = function(x, ...) {
                    vi <- varimp(x, ...)
                    names(vi)[vi != 0]
                  },
                  varImp = function(object, ...) {
                    variableImp <- varimp(object, ...)
                    out <- data.frame(Overall = variableImp)
                    out
                  },
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
                  levels = function(x) levels(x@data@get("response")[,1]),
                  sort = function(x) x[order(x[,1]),])
