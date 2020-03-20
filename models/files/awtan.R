modelInfo <- list(
  label = "Tree Augmented Naive Bayes Classifier with Attribute Weighting",
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
    dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
    dat$.outcome <- y
    args <- list(
      x = bnclassify::tan_cl(
        '.outcome',
        dataset = dat,
        score = as.character(param$score)
      ),
      dataset = dat,
      smooth = param$smooth
    )
    dots <- list(...)
    if (!any(names(dots) == "awnb_trees"))
      dots$awnb_trees <- 10
    if (!any(names(dots) == "awnb_bootstrap"))
      dots$awnb_bootstrap <- 10
    
    args <- c(args, dots)
    do.call(bnclassify::lp, args)
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
  sort = function(x) x[order(x[,1]),],
  check = function(pkg) {
    requireNamespace("kohonen")
    current <- packageDescription("bnclassify")$Version
    expected <- "0.3.3"
    if(compareVersion(current, expected) < 0)
      stop("This modeling workflow requires bnclassify version ",
           expected, "or greater.", call. = FALSE)
  }
)
