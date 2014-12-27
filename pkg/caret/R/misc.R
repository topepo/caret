
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

well_numbered <- function(prefix, items) {
  paste0(prefix, gsub(" ", "0", format(1:items)))
}


evalSummaryFunction <- function(y, wts, ctrl, lev, metric, method) {
  ## get phoney performance to obtain the names of the outputs
  testOutput <- data.frame(pred = sample(y, min(10, length(y))),
                           obs = sample(y, min(10, length(y))))
  
  if(ctrl$classProbs)
  {
    for(i in seq(along = lev)) testOutput[, lev[i]] <- runif(nrow(testOutput))
    testOutput[, lev] <- t(apply(testOutput[, lev], 1, function(x) x/sum(x)))
  } else {
    if(metric == "ROC" & !ctrl$classProbs)
      stop("train()'s use of ROC codes requires class probabilities. See the classProbs option of trainControl()")
  }
  if(!is.null(wts)) testOutput$weights <- sample(wts, min(10, length(wts)))
  ctrl$summaryFunction(testOutput, lev, method)
}


hasDots <- function(grid, info) {
  mnames <- sort(as.character(info$parameters$parameter))
  mnames2 <- paste(".", mnames, sep = "")
  gnames <- sort(colnames(grid))
  out <- all.equal(mnames2, gnames)
  if(class(out)[1] != "logical") out <- FALSE
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


Kim2009 <- function(n)
{
  grid <- matrix(runif(n*10), ncol = 10)
  grid <- as.data.frame(grid)
  names(grid) = paste("x", 1:10, sep = "")
  grid$x5 <- floor((grid$x5*3)+1)
  pred <- -10 + 10 * sin(pi * grid$x1* grid$x2) + 5*(grid$x3 - .5)^2 + 5*grid$x4 + 2*grid$x5
  prob <-  binomial()$linkinv(pred)
  grid$Class <- ifelse(prob <= runif(n), "Class1", "Class2")
  grid$Class <- factor(grid$Class, levels = c("Class1","Class2"))
  grid
}


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

flatTable <- function(pred, obs)
  {
    cells <- as.vector(table(pred, obs))
    if(length(cells) == 0) cells <- rep(NA, length(levels(obs))^2)
    names(cells) <- paste(".cell", seq(along= cells), sep = "")
    cells
  }


prettySeq <- function(x) paste("Resample", gsub(" ", "0", format(seq(along = x))), sep = "")

ipredStats <- function(x)
{
  requireNamespaceQuietStop("e1071")
  ## error check
  if(is.null(x$X)) stop("to get OOB stats, keepX must be TRUE when calling the bagging function")
  
  foo <- function(object, y, x)
    {
      holdY <- y[-object$bindx]
      if(is.factor(y))
        {
          requireNamespaceQuietStop("e1071")
          tmp <- predict(object$btree, x[-object$bindx,,drop = FALSE], type = "class")
          tmp <- factor(as.character(tmp), levels = levels(y))
          out <- c(
                   mean(holdY == tmp),
                   e1071::classAgreement(table(holdY, tmp))$kappa)
          
        } else {
          tmp <- predict(object$btree, x[-object$bindx,,drop = FALSE])

          out <- c(
                   sqrt(mean((tmp - holdY)^2, na.rm = TRUE)),
                   cor(holdY, tmp, use = "pairwise.complete.obs")^2)
        }
      out    
    }
  eachStat <- lapply(x$mtrees, foo, y = x$y, x = x$X)
  eachStat <- matrix(unlist(eachStat), nrow = length(eachStat[[1]]))
  out <- c(
           apply(eachStat, 1, mean, na.rm = TRUE),
           apply(eachStat, 1, sd, na.rm = TRUE))
  names(out) <- if(is.factor(x$y)) c("Accuracy", "Kappa", "AccuracySD", "KappaSD") else c("RMSE", "Rsquared", "RMSESD", "RsquaredSD")
  out
}

rfStats <- function(x)
{
  out <- switch(
                x$type,
                regression =   c(sqrt(max(x$mse[length(x$mse)], 0)), x$rsq[length(x$rsq)]),
                classification = {
                  requireNamespaceQuietStop("e1071")
                  c(
                    1 - x$err.rate[x$ntree, "OOB"],
                    e1071::classAgreement(x$confusion[,-dim(x$confusion)[2]])[["kappa"]])
                })
  names(out) <- if(x$type == "regression") c("RMSE", "Rsquared") else c("Accuracy", "Kappa")
  out              
}

cforestStats <- function(x)
{
  library(party)
  
  obs <- x@data@get("response")[,1]
  pred <- predict(x,  x@data@get("input"), OOB = TRUE)
  postResample(pred, obs)
  

}

bagEarthStats <- function(x) apply(x$oob, 2, function(x) quantile(x, probs = .5))


R2 <- function(pred, obs, formula = "corr", na.rm = FALSE)
  {
    n <- sum(complete.cases(pred))
    switch(formula,
           corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
           traditional = 1 - (sum((obs-pred)^2, na.rm = na.rm)/((n-1)*var(obs, na.rm = na.rm))))
  }


RMSE <- function(pred, obs, na.rm = FALSE) sqrt(mean((pred - obs)^2, na.rm = na.rm))

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

depth2cp <- function(x, depth)
  {
    out <- approx(x[,"nsplit"], x[,"CP"], depth)$y
    out[depth > max(x[,"nsplit"])] <- min(x[,"CP"]) * .99
    out
  }

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

class2ind <- function(x, drop2nd = FALSE) {
	if(!is.factor(x)) stop("'x' should be a factor")
	y <- model.matrix(~ x - 1) 
	colnames(y) <- gsub("^x", "", colnames(y))
	attributes(y)$assign <- NULL
	attributes(y)$contrasts <- NULL
	if(length(levels(x)) == 2 & drop2nd) {
		y <- y[,1]
	}
	y
}

requireNamespaceQuietStop <- function(package) {
    if (!requireNamespace(package, quietly = TRUE))
        stop(paste('package',package,'is required'))
}

get_resample_perf <- function (x, ...) UseMethod("get_resample_perf")

get_resample_perf.train <- function(x) {
  if(x$control$returnResamp == "none")
    stop("use returnResamp == 'none' in trainControl()")
  out <- merge(x$resample, x$bestTune)
  out[, c(x$perfNames, "Resample")]
}

get_resample_perf.rfe <- function(x) {
  if(x$control$returnResamp == "none")
    stop("use returnResamp == 'none' in trainControl()")
  out <- subset(x$resample, Variables == x$bestSubset) 
  out[, c(x$perfNames, "Resample")]
}

get_resample_perf.sbf <- function(x) {
  if(x$control$returnResamp == "none")
    stop("use returnResamp == 'none' in trainControl()")
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

