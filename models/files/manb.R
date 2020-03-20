modelInfo <- list(label = "Model Averaged Naive Bayes Classifier",
                  library = "bnclassify",
                  type = "Classification",
                  parameters = data.frame(parameter = c("smooth", "prior"),
                                          class = c("numeric", "numeric"),
                                          label = c("Smoothing Parameter", "Prior Probability")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") { 
                      out <- expand.grid(smooth = 0:(len-1), prior = seq(.1, .9, length = len))
                    } else {
                      out <- data.frame(smooth= runif(len, min = 0, max = 10),
                                        prior = runif(len))
                    }
                    out$smooth[out$smooth <= 0] <- .05
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    struct <- bnclassify::nb(class = '.outcome', dataset = dat)
                    bnclassify::lp(struct, dat, smooth = param$smooth,
                                   manb_prior = param$prior,
                                   ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata)       
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    predict(modelFit, newdata, prob = TRUE) 
                  },
                  levels = function(x) x$obsLevels,
                  predictors = function(x, s = NULL, ...) x$xNames,
                  tags = c("Bayesian Model", "Categorical Predictors Only"),
                  sort = function(x) x[order(x[,1]),])
