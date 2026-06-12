modelInfo <- list(label = "Generalized Additive Model using Splines",
                  library = "mgcv",
                  loop = NULL,
                  type = c('Regression', 'Classification'),
                  parameters = data.frame(parameter = c('select', 'method'),
                                          class = c('logical', 'character'),
                                          label = c('Feature Selection', 'Method')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(select = c(TRUE, FALSE), method = "GCV.Cp")
                    } else {
                      out <- data.frame(select = sample(c(TRUE, FALSE), size = len, replace = TRUE),
                                        method = sample(c("GCV.Cp", "ML", "REML"),
                                                        size = len, replace = TRUE))
                    }
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    require(mgcv)
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    modForm <- caret:::smootherFormula(x)
                    if(is.factor(y)) {
                      dat$.outcome <- ifelse(y == lev[1], 0, 1)
                      dist <- binomial()
                    } else {
                      dat$.outcome <- y
                      dist <- gaussian()
                    }
                    modelArgs <- list(formula = modForm,
                                      data = dat,
                                      select = param$select,
                                      method = as.character(param$method))
                    ## Intercept family if passed in
                    theDots <- list(...)
                    if(!any(names(theDots) == "family")) modelArgs$family <- dist
                    modelArgs <- c(modelArgs, theDots)

                    out <- do.call(mgcv::bam, modelArgs)
                    out

                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    if(modelFit$problemType == "Classification") {
                      probs <-  predict(modelFit, newdata, type = "response")
                      out <- ifelse(probs < .5,
                                    modelFit$obsLevel[1],
                                    modelFit$obsLevel[2])
                    } else {
                      out <- predict(modelFit, newdata, type = "response")
                    }
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, newdata, type = "response")
                    out <- cbind(1-out, out)
                    ## glm models the second factor level, we treat the first as the
                    ## event of interest. See Details in ?glm
                    colnames(out) <-  modelFit$obsLevels
                    out
                  },
                  predictors = function(x, ...) {
                    predictors(x$terms)
                  },
                  levels = function(x) x$obsLevels,
                  varImp = function(object, ...) {
                    smoothed <- summary(object)$s.table[, "p-value", drop = FALSE]
                    linear <- summary(object)$p.table
                    linear <- linear[, grepl("^Pr", colnames(linear)), drop = FALSE]
                    gams <- rbind(smoothed, linear)
                    gams <- gams[rownames(gams) != "(Intercept)",,drop = FALSE]
                    rownames(gams) <- gsub("^s\\(", "", rownames(gams))
                    rownames(gams) <- gsub("\\)$", "", rownames(gams))
                    colnames(gams)[1] <- "Overall"
                    gams <- as.data.frame(gams, stringsAsFactors = TRUE)
                    gams$Overall <- -log10(gams$Overall)
                    allPreds <- colnames(attr(object$terms,"factors"))
                    extras <- allPreds[!(allPreds %in% rownames(gams))]
                    if(any(extras)) {
                      tmp <- data.frame(Overall = rep(NA, length(extras)))
                      rownames(tmp) <- extras
                      gams <- rbind(gams, tmp)
                    }
                    gams
                  },
                  notes =
                    paste(
                      'Which terms enter the model in a nonlinear manner is determined',
                      'by the number of unique values for the predictor. For example,',
                      'if a predictor only has four unique values, most basis expansion',
                      'method will fail because there are not enough granularity in the',
                      'data. By default, a predictor must have at least 10 unique',
                      'values to be used in a nonlinear basis expansion.',
                      'Unlike other packages used by `train`, the `mgcv`',
                      'package is fully loaded when this model is used.'
                    ),
                  tags = c("Generalized Linear Model", "Generalized Additive Model"),
                  sort = function(x) x)
