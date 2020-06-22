modelInfo <- list(
  label = "Adjacent Categories Probability Model for Ordinal Data",
  library = "VGAM",
  loop = NULL,
  type = "Classification",
  parameters = data.frame(parameter = c("parallel", "link"),
                          class = c("logical", "character"),
                          label = c("Parallel Curves", "Link Function")),
  grid = function(x, y, len = NULL, search = "grid") {
    links <- c("loge")
    if(search == "grid") {
      out <- expand.grid(parallel = c(TRUE, FALSE), link = links)
    } else {
      out <- data.frame(parallel = sample(c(TRUE, FALSE), size = len, replace = TRUE),
                        link = sample(links, size = len, replace = TRUE))
    }
  },
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    theDots <- list(...)
    if(any(names(theDots) == "family")) {
      stop(paste("The `family` argument cannot be pass from `train` to `vglm`.",
                 "If you need to change the values of `reverse`, multiple.responses`",
                 "or `whitespace` you will have to use a custom model (see",
                 "http://topepo.github.io/caret/custom_models.html for details)."))
    }

    fam <- do.call(VGAM::cumulative, list(link = as.character(param$link), parallel = param$parallel))

    ## Set up data
    dat <- if (is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
    dat$.outcome <- y

    ## you can't programatically pass argments to `vglm`. This solution is from
    ## the pakcage maintainer T Yee on 4/15/16:
    ## Pass in model weights, if any
    if (!is.null(wts)) {
      run_this <- eval(substitute(expression({
        paste("VGAM::vglm(.outcome ~ ., ",
              "VGAM::acat(link = '",  .lnk , "', ",
              "parallel = ", .par ,
              "), ",
              "data = dat)", sep = "")}),
        list(.par = param$parallel, .lnk = as.character(param$link))))
      run_this <- eval(run_this)
      out <- eval(parse(text = run_this))
    } else {
      run_this <- eval(substitute(expression({
        paste("VGAM::vglm(.outcome ~ ., ",
              "VGAM::acat(link = '",  .lnk , "', ",
              "parallel = ", .par ,
              "), weights = wts,",
              "data = dat)", sep = "")}),
        list(.par = param$parallel, .lnk = as.character(param$link))))
      run_this <- eval(run_this)
      out <- eval(parse(text = run_this))
    }
    out
  },
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
    out <- VGAM::predictvglm(modelFit, newdata = newdata, type = "response")
    ordered(modelFit@misc$ynames[apply(out, 1, which.max)], levels = modelFit@misc$ynames)
    },
  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL){
    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
    out <- VGAM::predictvglm(modelFit, newdata = newdata, type = "response")
    out <- as.data.frame(out, stringsAsFactors = TRUE)
    names(out) <- modelFit@misc$ynames
    out
  },
  varImp = NULL,
  predictors = function(x, ...) caret:::predictors.terms(x@terms$terms),
  levels = function(x)
    if (any(names(x) == "obsLevels")) x$obsLevels else NULL,
  tags = c("Logistic Regression", "Linear Classifier", "Accepts Case Weights", "Ordinal Outcomes"),
  sort = function(x) x)
