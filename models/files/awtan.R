modelInfo <- list(label = "Tree Augmented Naive Bayes Classifier with Attribute Weighting",
                  library = "bnclassify",
                  type = "Classification",
                  parameters = data.frame(parameter = c('score', "smooth"),
                                          class = c("character", "numeric"),
                                          label = c('Score Function', "Smoothing Parameter")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    out <- expand.grid(score = c('loglik', 'bic', 'aic'),
                                       smooth = 1:2)
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    dat <- if(is.data.frame(x)) x else as.data.frame(x)
                    dat$.outcome <- y
                    struct <- bnclassify::tan_cl(class = '.outcome', dataset = dat, score = as.character(param$score))
                    bnclassify::lpawnb(struct, dat, smooth = param$smooth, 
                           trees = 10, bootstrap_size = 0.5, 
                           ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata)       
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata, prob = TRUE) 
                  },
                  levels = function(x) x$obsLevels,
                  predictors = function(x, s = NULL, ...) x$xNames,
                  tags = c("Bayesian Model", "Categorical Predictors Only"),
                  sort = function(x) x[order(x[,1]),],
                  notes = paste('Not on CRAN but can be installed from',
                                'GitHub at `bmihaljevic/bnclassify`.'))
