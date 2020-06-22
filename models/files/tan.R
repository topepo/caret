modelInfo <- list(label = "Tree Augmented Naive Bayes Classifier",
                  library = "bnclassify",
                  type = "Classification",
                  parameters = data.frame(parameter = c('score', "smooth"),
                                          class = c("character", "numeric"),
                                          label = c('Score Function', "Smoothing Parameter")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") { 
                      out <- expand.grid(score = c('loglik', 'bic', 'aic'),
                                         smooth = 0:(len-1))
                    } else {
                      out <- data.frame(smooth= runif(len, min = 0, max = 10),
                                        score = sample(c('loglik', 'bic', 'aic'),
                                                       size = len, replace = TRUE))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                    dat$.outcome <- y
                    bnclassify::bnc('tan_cl', class = '.outcome', dataset = dat,
                                    smooth = param$smooth,
                                    dag_args = list(score = as.character(param$score)),
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
