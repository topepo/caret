modelInfo <- list(label = "Generalized Additive Model using Splines",
                  library = "gam",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('df'),
                                          class = c('numeric'),
                                          label = c('Degrees of Freedom')),
                  grid = function(x, y, len = NULL) 
                    expand.grid(df = seq(1, 3, length = len)),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    args <- list(data = x)
                    args$data$.outcome <- y
                    ## if(!is.null(wts))  args$weights <- wts
                    args$formula <- caret:::smootherFormula(x,
                                                            smoother = "s",
                                                            df = param$df)
                    args$family <- if(is.factor(y)) binomial else gaussian

                    theDots <- list(...)
                    if(length(theDots) > 0) args <- c(args, theDots)
                    
                    do.call(getFromNamespace("gam", "gam"), args)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(modelFit$problemType == "Classification")
                    {
                      probs <-  gam:::predict.gam(modelFit, newdata, type = "response")
                      out <- ifelse(probs < .5,
                                    modelFit$obsLevel[1],
                                    modelFit$obsLevel[2])
                    } else {
                      out <- predict(modelFit, newdata, type = "response")
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- gam:::predict.gam(modelFit, newdata, type = "response")
                    out <- cbind(1-out, out)
                    ## glm models the second factor level, we treat the first as the
                    ## event of interest. See Details in ?glm
                    colnames(out) <-  modelFit$obsLevels
                    out
                  },
                  predictors = function(x, ...) {
                    getNames <- function(x) {
                      x <- strsplit(x, "(\\()|(,)|(\\))")
                      x <- lapply(x, function(x) x[!(x %in% c("s", "lo", ""))])
                      unlist(lapply(x, function(x) x[1]))
                    }
                    getNames(predictors(x$terms))
                  },
                  varImp = function(object, ...) {
                    getNames <- function(x) {
                      x <- strsplit(x, "(\\()|(,)|(\\))")
                      x <- lapply(x, function(x) x[!(x %in% c("s", "lo", ""))])
                      unlist(lapply(x, function(x) x[1]))
                    }
                    gamSummary <- gam:::summary.gam(object)
                    smoothed <- gamSummary$anova
                    smoothed <- smoothed[complete.cases(smoothed), grepl("^P", colnames(smoothed)), drop = FALSE] 
                    linear <- gamSummary$parametric.anova
                    linear <- linear[complete.cases(linear), grepl("^P", colnames(linear)), drop = FALSE] 
                    linear <- linear[!(rownames(linear) %in% rownames(smoothed)),,drop = FALSE]
                    colnames(smoothed) <- colnames(linear) <- "pval"
                    gams <- rbind(smoothed, linear)
                    gams <- gams[rownames(gams) != "(Intercept)",,drop = FALSE]
                    rownames(gams) <- getNames(rownames(gams))
                    colnames(gams)[1] <- "Overall"
                    gams <- as.data.frame(gams)
                    gams$Overall <- -log10(gams$Overall)
                    allPreds <- getNames(colnames(attr(object$terms,"factors")))
                    extras <- allPreds[!(allPreds %in% rownames(gams))]
                    if(any(extras)) {
                      tmp <- data.frame(Overall = rep(NA, length(extras)))
                      rownames(tmp) <- extras
                      gams <- rbind(gams, tmp)
                    }
                    gams
                  },
                  tags = c("Generalized Linear Model", "Generalized Additive Model"),
                  sort = function(x) x)
