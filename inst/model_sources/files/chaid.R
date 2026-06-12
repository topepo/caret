modelInfo <- list(label = "CHi-squared Automated Interaction Detection",
                  library = "CHAID",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c('alpha2', 'alpha3', 'alpha4'),
                                          class = rep('numeric', 3),
                                          label = c('Merging Threshold', 
                                                    "Splitting former Merged Threshold", "
                                                    Splitting former Merged Threshold")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- data.frame(alpha2 = seq(from = .05, to = 0.01, length = len),
                                        alpha3 = -1,
                                        alpha4 = seq(from = .05, to = 0.01, length = len))
                    } else {
                      out <- data.frame(alpha2 = runif(len, min = 0.000001, max = .1),
                                        alpha3 = runif(len, min =-.1, max = .1),
                                        alpha4 = runif(len, min = 0.000001, max = .1))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    theDots <- list(...)
                    if(any(names(theDots) == "control")) {
                        theDots$control$alpha2 <- param$alpha2
                        theDots$control$alpha3 <- param$alpha3
                        theDots$control$alpha4 <- param$alpha4
                        ctl <- theDots$control
                        theDots$control <- NULL
                      } else ctl <- CHAID::chaid_control(alpha2 = param$alpha2,
                                                         alpha3 = param$alpha3,
                                                         alpha4 = param$alpha4)
                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts
                    modelArgs <- c(
                                   list(
                                        formula = as.formula(".outcome ~ ."),
                                        data = dat,
                                        control = ctl),
                                   theDots)
                    out <- do.call(CHAID::chaid, modelArgs)
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata, type = "prob")
                  },
                  levels = function(x) x$obsLevels,
                  predictors = function(x, surrogate = TRUE, ...) {
                    predictors(terms(x))
                  },
                  tags = c('Tree-Based Model', "Implicit Feature Selection", "Two Class Only", "Accepts Case Weights"),
                  sort = function(x) x[order(-x$alpha2, -x$alpha4, -x$alpha3),])
