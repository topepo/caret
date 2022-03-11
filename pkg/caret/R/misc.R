subsemble_index <- function(y, J = 2, V = 10){
  dat <- data.frame(y = y, index = seq(along = y))
  outer_index <- sample(1:J, size = nrow(dat), replace = TRUE)
  outer_splits <- vector(mode = "list", length = J)
  for(i in 1:J) {
    outer_splits[[i]] <- dat[outer_index == i,]
    outer_splits[[i]]$label <- well_numbered("Outer", J)[i]
  }
  foo <- function(x, V = 10) {
    inner_index_0 <- createFolds(x$y, k = V, returnTrain = TRUE)
    modeling_index <- lapply(inner_index_0, function(x, y) y[x], y = x$index)
    holdout_index <- lapply(inner_index_0, function(x, y) y[-unique(x)], y = x$index)
    names(modeling_index) <- names(holdout_index) <- paste(x$label[1], names(modeling_index), sep = ".")
    list(model = modeling_index, holdout = holdout_index)
  }
  all_index <- lapply(outer_splits, foo, V = V)
  model_index <- holdout_index <- NULL
  for(i in seq(along = all_index)) {
    model_index   <- c(model_index,   all_index[[i]]$model)
    holdout_index <- c(holdout_index, all_index[[i]]$holdout)
  }
  list(model = model_index, holdout = holdout_index)
}

#' @rdname caret-internal
#' @export
well_numbered <- function(prefix, items) {
  paste0(prefix, gsub(" ", "0", format(1:items)))
}


#' @importFrom stats runif
evalSummaryFunction <- function(y, wts = NULL, perf = NULL, ctrl, lev, metric, method) {
  n <- if(inherits(y, "Surv")) nrow(y) else length(y)
  ## sample doesn't work for Surv objects
  if(!inherits(y, "Surv")) {
    if(is.factor(y)) {
      values <- rep_len(levels(y), min(10, n))
      pred_samp <- factor(sample(values), levels = lev)
      obs_samp  <- factor(sample(values), levels = lev)
    } else {
      pred_samp <- sample(y, min(10, n))
      obs_samp <- sample(y, min(10, n))
    }
  } else {
    pred_samp <- y[sample(1:n, min(10, n)), "time"]
    obs_samp <- y[sample(1:n, min(10, n)),]
  }

  ## get phoney performance to obtain the names of the outputs
  testOutput <- data.frame(pred = pred_samp, obs = obs_samp)
  if(!is.null(perf)) {
    if(is.vector(perf))
      stop("`perf` should be a data frame", call. = FALSE)
    perf <- perf[sample(1:nrow(perf), nrow(testOutput)),, drop = FALSE]
    testOutput <-cbind(testOutput, perf)
  }

  if(ctrl$classProbs) {
    for(i in seq(along = lev)) testOutput[, lev[i]] <- runif(nrow(testOutput))
    testOutput[, lev] <- t(apply(testOutput[, lev], 1, function(x) x/sum(x)))
  } else {
    if(metric == "ROC" & !ctrl$classProbs)
      stop("train()'s use of ROC codes requires class probabilities. See the classProbs option of trainControl()")
  }
  if(!is.null(wts)) testOutput$weights <- sample(wts, min(10, length(wts)))
  testOutput$rowIndex <- sample(1:n, size = nrow(testOutput))
  ctrl$summaryFunction(testOutput, lev, method)
}


hasDots <- function(grid, info) {
  mnames <- sort(as.character(info$parameters$parameter))
  mnames2 <- paste(".", mnames, sep = "")
  gnames <- sort(colnames(grid))
  out <- all.equal(mnames2, gnames)
  if(!inherits(out, "logical")) out <- FALSE
  out
}

model2method <- function(x)
{
  ## There are some disconnecs between the object class and the
  ## method used by train.

  switch(x,
         randomForest = "rf",
         rvm = "rvmRadial",
         ksvm = "svmRadial",
         lssvm = "lssvmRadial",
         gausspr = "gaussprRadial",
         NaiveBayes = "nb",
         classbagg =, regbagg = "treebag",
         plsda = "pls",
         pamrtrained = "pam",
         x)
}

#' @importFrom stats runif binomial
Kim2009 <- function(n)
{
  grid <- matrix(runif(n*10), ncol = 10)
  grid <- as.data.frame(grid, stringsAsFactors = TRUE)
  names(grid) = paste("x", 1:10, sep = "")
  grid$x5 <- floor((grid$x5*3)+1)
  pred <- -10 + 10 * sin(pi * grid$x1* grid$x2) + 5*(grid$x3 - .5)^2 + 5*grid$x4 + 2*grid$x5
  prob <-  binomial()$linkinv(pred)
  grid$Class <- ifelse(prob <= runif(n), "Class1", "Class2")
  grid$Class <- factor(grid$Class, levels = c("Class1","Class2"))
  grid
}

#' @rdname caret-internal
#' @importFrom stats as.formula
#' @export
gamFormula <- function(data, smoother = "s", cut = 8, y = "y")
{
  nzv <- nearZeroVar(data)
  if(length(nzv) > 0) data <- data[, -nzv, drop = FALSE]

  numValues <- apply(data, 2, function(x) length(unique(x)))
  prefix <- rep("", ncol(data))
  prefix[numValues > cut] <- paste(smoother, "(", sep = "")
  suffix <- rep("", ncol(data))
  suffix[numValues > cut] <- ")"
  rhs <- paste(prefix, names(numValues), suffix, sep = "")
  rhs <- paste(rhs, collapse = "+")
  form <- as.formula(paste(y, "~", rhs, sep = ""))
  form
}

printCall <- function(x)
{
  call <- paste(deparse(x), collapse = "\n")
  #     cat("\nCall:\n", call, "\n\n", sep = "")
  ## or

  cat("\nCall:\n", truncateText(deparse(x, width.cutoff = 500)), "\n\n", sep = "")
  invisible(call)
}

#' @rdname caret-internal
#' @export
flatTable <- function(pred, obs)
{
  cells <- as.vector(table(pred, obs))
  if(length(cells) == 0) cells <- rep(NA, length(levels(obs))^2)
  names(cells) <- paste(".cell", seq(along= cells), sep = "")
  cells
}


prettySeq <- function(x) paste("Resample", gsub(" ", "0", format(seq(along = x))), sep = "")

#' @rdname caret-internal
#' @export
ipredStats    <- function(x) getModelInfo("treebag", regex = FALSE)[[1]]$oob(x)

#' @rdname caret-internal
#' @export
rfStats       <- function(x) getModelInfo("rf", regex = FALSE)[[1]]$oob(x)

#' @rdname caret-internal
#' @export
cforestStats  <- function(x) getModelInfo("cforest", regex = FALSE)[[1]]$oob(x)

#' @rdname caret-internal
#' @export
bagEarthStats <- function(x) getModelInfo("bagEarth", regex = FALSE)[[1]]$oob(x)


#' @importFrom stats complete.cases cor
#' @export
R2 <- function(pred, obs, formula = "corr", na.rm = FALSE)
{
  n <- sum(complete.cases(pred))
  switch(formula,
         corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
         traditional = 1 - (sum((obs-pred)^2, na.rm = na.rm)/((n-1)*var(obs, na.rm = na.rm))))
}

#' @export
RMSE <- function(pred, obs, na.rm = FALSE) sqrt(mean((pred - obs)^2, na.rm = na.rm))

#' @export
MAE <- function(pred, obs, na.rm = FALSE) mean(abs(pred - obs), na.rm = na.rm)

#' @importFrom utils capture.output
partRuleSummary <- function(x)
{
  predictors <- all.vars(x$terms)
  predictors <- predictors[predictors != as.character(x$terms[[2]])]
  classes <- levels(x$predictions)
  rules <- capture.output(print(x))
  conditions <- grep("(<=|>=|<|>|=)", rules, value = TRUE)
  classPred <- grep("\\)$", conditions, value = TRUE)
  varUsage <- data.frame(Var = predictors,
                         Overall = 0)
  for(i in seq(along = predictors))
    varUsage$Overall[i] <- sum(grepl(paste("^", predictors[i], sep = ""), conditions))

  numClass <- rep(NA, length(classes))
  names(numClass) <- classes
  for(i in seq(along = classes))
    numClass[i] <- sum(grepl(paste(":", classes[i], sep = " "), classPred))

  list(varUsage = varUsage,
       numCond = length(conditions),
       classes = numClass)

}

#' @importFrom utils capture.output
ripperRuleSummary <- function(x)
{
  predictors <- all.vars(x$terms)
  predictors <- predictors[predictors != as.character(x$terms[[2]])]
  classes <- levels(x$predictions)
  rules <- capture.output(print(x))
  ## remove header
  rules <- rules[-(1:min(which(rules == "")))]
  conditions <- grep("(<=|>=|<|>|=)", rules, value = TRUE)
  varUsage <- data.frame(Var = predictors,
                         Overall = 0)
  for(i in seq(along = predictors))
    varUsage$Overall[i] <- sum(grepl(paste("\\(", predictors[i], sep = ""), conditions))

  numClass <- rep(NA, length(classes))
  names(numClass) <- classes
  for(i in seq(along = classes))
    numClass[i] <- sum(grepl(paste(x$terms[[2]], "=", classes[i], sep = ""), conditions))

  list(varUsage = varUsage,
       numCond = length(conditions),
       classes = numClass)

}

##########################################################################################################

## splitIndicies takes a number of tasks (n) and divides it into k groups
## of roughly equal size. The result is an integer vector of task groups

splitIndicies <- function(n, k)
{
  out <- rep(1:k, n%/%k)
  if(n %% k > 0)  out <- c(out, sample(1:k, n %% k))
  sort(out)
}

## This makes a list of copies of another list


repList <- function(x, times = 3, addIndex = FALSE)
{
  out <- vector(mode = "list", length = times)
  out <- lapply(out, function(a, b) b, b = x)
  if(addIndex) for(i in seq(along = out)) out[[i]]$.index <- i
  out
}

useMathSymbols <- function(x)
{
  if(x == "Rsquared") x <- expression(R^2)
  x
}

#' @importFrom stats approx
depth2cp <- function(x, depth)
{
  out <- approx(x[,"nsplit"], x[,"CP"], depth)$y
  out[depth > max(x[,"nsplit"])] <- min(x[,"CP"]) * .99
  out
}

#' @importFrom stats as.formula
smootherFormula <- function(data, smoother = "s", cut = 10, df = 0, span = .5, degree = 1, y = ".outcome")
{
  nzv <- nearZeroVar(data)
  if(length(nzv) > 0) data <- data[, -nzv, drop = FALSE]

  numValues <- sort(apply(data, 2, function(x) length(unique(x))))
  prefix <- rep("", ncol(data))
  suffix <- rep("", ncol(data))
  prefix[numValues > cut] <- paste(smoother, "(", sep = "")
  if(smoother == "s")
  {
    suffix[numValues > cut] <- if(df == 0) ")" else paste(", df=", df, ")", sep = "")
  }
  if(smoother == "lo")
  {
    suffix[numValues > cut] <- paste(", span=", span, ",degree=", degree, ")", sep = "")
  }
  if(smoother == "rcs")
  {
    suffix[numValues > cut] <- ")"
  }
  rhs <- paste(prefix, names(numValues), suffix, sep = "")
  rhs <- paste(rhs, collapse = "+")
  form <- as.formula(paste(y, rhs, sep = "~"))
  form
}

varSeq <- function(x)
{
  vars <- apply(summary(x)$which, 1, function(x) names(which(x)))
  vars <- lapply(vars, function(x) x[x != "(Intercept)"])
  vars
}

cranRef <- function(x) paste("{\\tt \\href{http://cran.r-project.org/web/packages/", x, "/index.html}{", x, "}}", sep = "")

makeTable <- function(x)
{
  params <- paste("\\code{", as.character(x$parameter), "}", sep = "", collapse = ", ")
  params <- ifelse(params == "\\code{parameter}", "None", params)

  data.frame(method = as.character(x$model)[1],
             Package = cranRef(as.character(x$Package)[1]),
             Parameters = params)


}

scrubCall <- function(x)
{
  items <- c("x", "y", "data")
  for(i in items) if(nchar(as.character(x[i])) > 100) x[i] <- "scrubbed"
  x
}


requireNamespaceQuietStop <- function(package) {
  if (!requireNamespace(package, quietly = TRUE))
    stop(paste('package',package,'is required'), call. = FALSE)
}

get_resample_perf <- function (x, ...) UseMethod("get_resample_perf")

get_resample_perf.train <- function(x) {
  if(x$control$returnResamp == "none")
    stop("use returnResamp == 'none' in trainControl()", call. = FALSE)
  out <- merge(x$resample, x$bestTune)
  out[, c(x$perfNames, "Resample")]
}

get_resample_perf.rfe <- function(x) {
  if(x$control$returnResamp == "none")
    stop("use returnResamp == 'none' in trainControl()", call. = FALSE)
  out <- subset(x$resample, Variables == x$bestSubset)
  out[, c(x$perfNames, "Resample")]
}

get_resample_perf.sbf <- function(x) {
  if(x$control$returnResamp == "none")
    stop("use returnResamp == 'none' in trainControl()", call. = FALSE)
  x$resample
}

get_resample_perf.safs <- function(x) {
  out <- subset(x$external, Iter == x$optIter)
  out[, !(names(out) %in% "Iter")]
}

get_resample_perf.gafs <- function(x) {
  out <- subset(x$external, Iter == x$optIter)
  out[, !(names(out) %in% "Iter")]
}




#' Sequences of Variables for Tuning
#'
#' This function generates a sequence of \code{mtry} values for random forests.
#'
#' If the number of predictors is less than 500, a simple sequence of values of
#' length \code{len} is generated between 2 and \code{p}. For larger numbers of
#' predictors, the sequence is created using \code{log2} steps.
#'
#' If \code{len = 1}, the defaults from the \code{randomForest} package are
#' used.
#'
#' @param p The number of predictors
#' @param classification Is the outcome a factor (\code{classification = TRUE}
#' or numeric?)
#' @param len The number of \code{mtry} values to generate.
#' @return a numeric vector
#' @author Max Kuhn
#' @keywords models
#' @examples
#'
#' var_seq(p = 100, len = 10)
#' var_seq(p = 600, len = 10)
#'
#' @export var_seq
var_seq <- function(p, classification = FALSE, len = 3) {
  if(len == 1) {
    tuneSeq <- if(!classification) max(floor(p/3), 1) else floor(sqrt(p))
  } else {
    if(p <= len)
    {
      tuneSeq <- floor(seq(2, to = p, length = p))
    } else {
      if(p < 500 ) tuneSeq <- floor(seq(2, to = p, length = len))
      else tuneSeq <- floor(2^seq(1, to = log(p, base = 2), length = len))
    }
  }
  if(any(table(tuneSeq) > 1)) {
    tuneSeq <- unique(tuneSeq)
    cat(
      "note: only",
      length(tuneSeq),
      "unique complexity parameters in default grid.",
      "Truncating the grid to",
      length(tuneSeq), ".\n\n")
  }
  tuneSeq
}

parse_sampling <- function(x, check_install = TRUE) {
  ## x could be
  ### a string to match to a existing method
  ### a function
  ### a list

  ## output should be a list with elements
  ### name
  ### func
  ### before_pp (logical)
  x_class <- class(x)[1]
  if(!(x_class %in% c("character", "function", "list"))) {
    stop(paste("The sampling argument should be either a",
               "string, function, or list. See",
               "http://topepo.github.io/caret/model-training-and-tuning.html"),
         call. = FALSE)
  }
  if(x_class == "character") {
    x <- x[1]
    load(system.file("models", "sampling.RData", package = "caret"))
    s_method <- names(sampling_methods)
    if(!(x %in% s_method)) {
      stop("That sampling scheme is not in caret's built-in library",
           call. = FALSE)
    } else {
      x <- list(name = x,
                func = sampling_methods[x][[1]],
                first = TRUE)
    }
    pkgs <- switch(x$name, rose = "ROSE", smote = "themis", "")
    if(pkgs != "" & check_install)
      checkInstall(pkgs)
  } else {
    if(x_class == "function") {
      check_samp_func(x)
      x <- list(name = "custom",
                func = x,
                first = TRUE)
    } else {
      check_samp_list(x)
    }
  }
  x
}

check_samp_func <- function(x) {
  s_args <- sort(names(formals(x)))
  if(length(s_args) != 2) {
    stop("the 'sampling' function should have arguments 'x' and 'y'",
         call. = FALSE)
  } else {
    if(!all(s_args == c("x", "y")))
      stop("the 'sampling' function should have arguments 'x' and 'y'",
           call. = FALSE)
  }
  invisible(NULL)
}

check_samp_list <- function(x) {
  exp_names <- sort(c("name", "func", "first"))
  x_names <- sort(names(x))
  if(length(x_names) != length(exp_names)) {
    stop(paste("the 'sampling' list should have elements",
               paste(exp_names, sep = "", collapse = ", ")),
         call. = FALSE)
  } else {
    if(!all(exp_names == x_names))
      stop(paste("the 'sampling' list should have elements",
                 paste(exp_names, sep = "", collapse = ", ")),
           call. = FALSE)
  }
  check_samp_func(x$func)
  if(!is.logical(x$first))
    stop("The element 'first' should be a logical", call. = FALSE)
  invisible(NULL)
}



#' Get sampling info from a train model
#'
#' Placeholder.
#'
#' Placeholder.
#'
#' @param method Modeling method.
#' @param regex Whether to use regex matching.
#' @param ... additional arguments to passed to grepl.
#' @return A list
#' @export getSamplingInfo
getSamplingInfo <- function(method = NULL, regex = TRUE, ...) {
  load(system.file("models", "sampling.RData", package = "caret"))
  if (!is.null(method)) {
    keepers <- if (regex)
      grepl(method, names(sampling_methods), ...) else
        which(method == names(sampling_methods))[1]
    sampling_methods <- sampling_methods[keepers]
  }
  if (length(sampling_methods) == 0)
    stop("That sampling method is not in caret's built-in library",
         call. = FALSE)
  sampling_methods
}

get_labels <- function(mods, format = FALSE) {
  lib <- getModelInfo()
  lib_labs <- unlist(lapply(lib, function(x) x$label))
  labs <- mods
  is_match <- mods %in% names(lib)
  if(any(is_match)) labs[is_match] <- lib_labs[mods[is_match]]
  if(format) {
    labs <- gsub("-", "--", labs)
    labs <- gsub("with Polynomial Kernel", "(Polynomial)", labs)
    labs <- gsub("with Radial Basis Function Kernel", "(RBF)", labs)
    labs <- gsub("with Linear Kernel", "(Linear)", labs)
    labs <- gsub("Linear Discriminant Analysis", "LDA", labs)
    labs <- gsub("Quadratic Discriminant Analysis", "QDA", labs)
    labs <- gsub("Multivariate Adaptive Regression Spline", "MARS", labs)
    labs[labs == "glmnet"] <- "\\textsf{glmnet}"
  }
  if(length(mods) > 1) data.frame(model = mods, label = labs) else labs[1]
}

check_dims <- function(x, y) {
  n <- if(inherits(y, "Surv")) nrow(y) else length(y)
  stopifnot(nrow(x) > 1)
  stopifnot(nrow(x) == n)
  invisible(NULL)
}

get_model_type <- function(y, method = NULL) {
  type <- if(class(y)[1] %in% c("numeric", "Surv", "integer")) "Regression" else "Classification"
  type
}

#' @importFrom grDevices extendrange
get_range <- function(y) {
  if(class(y)[1] == "factor") return(NA)
  if(class(y)[1] %in% c("numeric", "integer")) extendrange(y) else extendrange(y[, "time"])
}

#' @rdname caret-internal
#' @export
outcome_conversion <- function(x, lv) {
  if(is.factor(x) | is.character(x)) {
    if(!is.null(attributes(lv)) && any(names(attributes(lv)) == "ordered" && attr(lv, "ordered")))
      x <- ordered(as.character(x), levels = lv) else
        x <- factor(as.character(x), levels = lv)
  }
  x
}

check_na_conflict <- function(call_obj) {

  ## check for na.action info:
  if("na.action" %in% names(as.list(call_obj))) {
    nam <- as.character(call_obj$na.action)
  } else nam <- "na.fail"

  ## check for preprocess info:
  has_pp <- grepl("^preProc", names(call_obj))
  if(any(has_pp)) {
    pp <- as.character(call_obj[has_pp])
    imputes <- if(any(grepl("impute", tolower(pp)))) TRUE else FALSE
  } else imputes <- FALSE

  if(imputes & any(nam %in% c("na.omit", "na.exclude")))
    warning(paste0("`preProcess` includes an imputation method but missing ",
                   "data will be eliminated by the formula method using `na.action=",
                   nam, "`. Consider using `na.actin=na.pass` instead."),
            call. = FALSE)
  invisible(NULL)
}


# in case an object is a sparse matrix or tibble
# do not use `drop` as an argument
subset_x <- function(x, ind) {
  if(is.matrix(x) | is.data.frame(x) | inherits(x, "dgCMatrix"))
    x <- x[ind,,drop = FALSE] else
      x <- x[ind,]
    x
}

fail_warning <- function(settings, msg, where = "model fit", iter, verb) {
  if (is.list(msg)) {
    is_fail <- vapply(msg, inherits, c(x = TRUE), what = "try-error")
    msg <- msg[is_fail]
  }

  if (!is.character(msg))
    msg <- as.character(msg)

  wrn <- paste(colnames(settings),
               settings,
               sep = "=",
               collapse = ", ")
  wrn <- paste(where, " failed for ", iter,
               ": ", wrn, " ", msg, sep = "")
  if (verb)
    cat(wrn, "\n")

  warning(wrn, call. = FALSE)
  invisible(wrn)
}

fill_failed_pred <- function(index, lev, submod){
  ## setup a dummy results with NA values for all predictions
  nPred <- length(index)
  if(!is.null(lev)) {
    predicted <- rep("", nPred)
    predicted[seq(along = predicted)] <- NA
  } else {
    predicted <- rep(NA, nPred)
  }
  if(!is.null(submod)) {
    tmp <- predicted
    predicted <- vector(mode = "list", length = nrow(submod) + 1)
    for(i in seq(along = predicted)) predicted[[i]] <- tmp
    rm(tmp)
  }
  predicted
}

fill_failed_prob <- function(index, lev, submod) {
  probValues <- matrix(NA, nrow = length(index), ncol = length(lev))
  probValues <- as.data.frame(probValues, stringsAsFactors = TRUE)
  colnames(probValues) <- lev
  if (!is.null(submod))
    probValues <- rep(list(probValues), nrow(submod) + 1L)
  probValues
}

optimism_xy <- function(ctrl, x, y, wts, iter, lev, method, mod, predicted, submod, loop) {
  indexExtra <- ctrl$indexExtra[[iter]]

  if(is.null(indexExtra) || inherits(mod, "try-error") || inherits(predicted, "try-error"))
    return (NULL)

  predictedExtra <- lapply(indexExtra, function(index) {
    pred <- predictionFunction(method = method,
                               modelFit = mod$fit,
                               newdata = subset_x(x, index),
                               preProc = mod$preProc,
                               param = submod)
  })

  if(ctrl$classProbs)
    probValuesExtra <- lapply(indexExtra, function(index) {
      probFunction(method = method,
                   modelFit = mod$fit,
                   newdata = subset_x(x, index),
                   preProc = mod$preProc,
                   param = submod)
    })
  else
    probValuesExtra <- lapply(indexExtra, function(index) {
      probValues <- matrix(NA, nrow = length(index), ncol = length(lev))
      probValues <- as.data.frame(probValues, stringsAsFactors = TRUE)
      colnames(probValues) <- lev
      if (!is.null(submod)) probValues <- rep(list(probValues), nrow(submod) + 1L)
      probValues
    })

  if(!is.null(submod)) {
    allParam <- expandParameters(loop, submod)
    allParam <- allParam[complete.cases(allParam),, drop = FALSE]

    predictedExtra <- Map(predictedExtra, indexExtra, f = function(predicted, holdoutIndex) {
      lapply(predicted, function(x) {
        y <- y[holdoutIndex]
        wts <- wts[holdoutIndex]
        x <- outcome_conversion(x, lv = lev)
        out <- data.frame(pred = x, obs = y, stringsAsFactors = FALSE)
        if(!is.null(wts)) out$weights <- wts
        out$rowIndex <- holdoutIndex
        out
      })
    })

    if(ctrl$classProbs)
      predictedExtra <- Map(predictedExtra, probValuesExtra, f = function(predicted, probValues) {
        Map(cbind, predicted, probValues)
      })

    thisResampleExtra <- lapply(predictedExtra, function(predicted) {
      lapply(predicted,
             ctrl$summaryFunction,
             lev = lev,
             model = method)
    })
    thisResampleExtra[[1L]] <- lapply(thisResampleExtra[[1L]], function(res) {
      names(res) <- paste0(names(res), "Orig")
      res
    })
    thisResampleExtra[[2L]] <- lapply(thisResampleExtra[[2L]], function(res) {
      names(res) <- paste0(names(res), "Boot")
      res
    })
    thisResampleExtra <- do.call(cbind, lapply(thisResampleExtra, function(x) do.call(rbind, x)))
    thisResampleExtra <- cbind(allParam, thisResampleExtra)

  } else {
    thisResampleExtra <- Map(predictedExtra, indexExtra, probValuesExtra,
                             f = function(predicted, holdoutIndex, probValues) {
                               if(is.factor(y)) predicted <- outcome_conversion(predicted, lv = lev)
                               tmp <-  data.frame(pred = predicted,
                                                  obs = y[holdoutIndex],
                                                  stringsAsFactors = FALSE)
                               ## Sometimes the code above does not coerce the first
                               ## columnn to be named "pred" so force it
                               names(tmp)[1] <- "pred"
                               if(!is.null(wts)) tmp$weights <- wts[holdoutIndex]
                               if(ctrl$classProbs) tmp <- cbind(tmp, probValues)
                               tmp$rowIndex <- holdoutIndex
                               ctrl$summaryFunction(tmp, lev = lev, model = method)
                             })
    names(thisResampleExtra[[1L]]) <- paste0(names(thisResampleExtra[[1L]]), "Orig")
    names(thisResampleExtra[[2L]]) <- paste0(names(thisResampleExtra[[2L]]), "Boot")
    thisResampleExtra <- unlist(unname(thisResampleExtra), recursive = FALSE)
    thisResampleExtra <- cbind(as.data.frame(t(thisResampleExtra), stringsAsFactors = TRUE), loop)
  }

  # return
  thisResampleExtra
}

optimism_rec <- function(ctrl, dat, iter, lev, method, mod_rec, predicted, submod, loop) {
  indexExtra <- ctrl$indexExtra[[iter]]

  if(is.null(indexExtra) || model_failed(mod_rec) || inherits(predicted, "try-error"))
    return (NULL)

  predictedExtra <- lapply(indexExtra, function(index) {
    pred <- rec_pred(method = method,
                     object = mod_rec,
                     newdata = subset_x(dat, index),
                     param = submod)
    trim_values(pred, ctrl, is.null(lev))
  })

  if(ctrl$classProbs)
    probValuesExtra <- lapply(indexExtra, function(index) {
      rec_prob(method = method,
               object = mod_rec,
               newdata = subset_x(dat, index),
               param = submod)
    })
  else
    probValuesExtra <- lapply(indexExtra, function(index) {
      fill_failed_prob(index, lev, submod)
    })

  if(!is.null(submod)) {
    allParam <- expandParameters(loop, submod)
    allParam <- allParam[complete.cases(allParam),, drop = FALSE]

    predictedExtra <- Map(predictedExtra, indexExtra, f = function(predicted, holdoutIndex) {
      lapply(predicted, function(x) {
        x <- outcome_conversion(x, lv = lev)
        dat <- holdout_rec(mod_rec, dat, holdoutIndex)
        dat$pred <- x
        dat
      })
    })

    if(ctrl$classProbs)
      predictedExtra <- Map(predictedExtra, probValuesExtra, f = function(predicted, probValues) {
        Map(cbind, predicted, probValues)
      })

    thisResampleExtra <- lapply(predictedExtra, function(predicted) {
      lapply(predicted,
             ctrl$summaryFunction,
             lev = lev,
             model = method)
    })
    thisResampleExtra[[1L]] <- lapply(thisResampleExtra[[1L]], function(res) {
      names(res) <- paste0(names(res), "Orig")
      res
    })
    thisResampleExtra[[2L]] <- lapply(thisResampleExtra[[2L]], function(res) {
      names(res) <- paste0(names(res), "Boot")
      res
    })
    thisResampleExtra <- do.call(cbind, lapply(thisResampleExtra, function(x) do.call(rbind, x)))
    thisResampleExtra <- cbind(allParam, thisResampleExtra)

  } else {
    thisResampleExtra <- Map(predictedExtra, indexExtra, probValuesExtra,
                             f = function(predicted, holdoutIndex, probValues) {
                               tmp <- holdout_rec(mod_rec, dat, holdoutIndex)
                               tmp$pred <- outcome_conversion(predicted, lv = lev)
                               if(ctrl$classProbs) tmp <- cbind(tmp, probValues)
                               tmp <- merge(tmp, loop, all = TRUE)
                               ctrl$summaryFunction(tmp, lev = lev, model = method)
                             })
    names(thisResampleExtra[[1L]]) <- paste0(names(thisResampleExtra[[1L]]), "Orig")
    names(thisResampleExtra[[2L]]) <- paste0(names(thisResampleExtra[[2L]]), "Boot")
    thisResampleExtra <- unlist(unname(thisResampleExtra), recursive = FALSE)
    thisResampleExtra <- cbind(as.data.frame(t(thisResampleExtra), stringsAsFactors = TRUE), loop)
  }

  # return
  thisResampleExtra
}

parallel_check <- function(pkg, models) {
  if(any(search() == "package:doMC") && getDoParRegistered() && pkg %in% models$library)
    warning("Models using ", pkg, " will not work with parallel processing with multicore/doMC",
            call. = FALSE)
  flush.console()
}

# ------------------------------------------------------------------------------

terms_ptype <- function(x, data) {
  cols <- attr(x, "variables")[-2]
  cols <- all.vars(cols)
  data[0, cols, drop = FALSE]
}
