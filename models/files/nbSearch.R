modelInfo <- list(
  label = "Semi-Naive Structure Learner Wrapper",
  library = "bnclassify",
  type = "Classification",
  parameters = data.frame(parameter = c("k", 'epsilon', "smooth", "final_smooth", "direction"),
                          class = c(rep("numeric", 4), "character"),
                          label = c('#Folds', "Minimum Absolute Improvement", 
                                    "Smoothing Parameter",
                                    "Final Smoothing Parameter", "Search Direction")),
  grid = function(x, y, len = NULL, search = "grid") {
    if(search == "grid") { 
      out <- expand.grid(k = 10, epsilon = 0.01, smooth = 0.01,
                         final_smooth = 1,
                         direction = c("forward", "backwards"))
    } else {
      out <- data.frame(k = sample(3:10, size = len, replace = TRUE),
                        epsilon = runif(len, min = 0, max = .05),
                        smooth= runif(len, min = 0, max = 10),
                        final_smooth= runif(len, min = 0, max = 10),
                        direction = sample(c("forward", "backwards"), 
                                           size = len, replace = TRUE))
    }
    out
  },
  loop = NULL,
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
    dat$.outcome <- y
    if(param$direction == "forward") {
      struct <- bnclassify::fssj(class = '.outcome', dataset = dat,
                                 k = param$k,
                                 epsilon = param$epsilon,
                                 smooth = param$smooth)
    } else {
      struct <- bnclassify::bsej(class = '.outcome', dataset = dat,
                                 k = param$k,
                                 epsilon = param$epsilon,
                                 smooth = param$smooth)
    }
    bnclassify::lp(struct, dat, smooth = param$final_smooth, ...)
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
  sort = function(x) x[order(x[,1]),]
)
