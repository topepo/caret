
sa_internal_names <- c('Best','Note','Random','Prob','Iter','Cycle',
                       'SinceRestart','Size','Similarity','Similarity_B','Resample')

sa_external_names <- c('Iter','Resample')

sa_func_check <- function(x) {
  fnames <- names(x)
  required <- c('fit', 'fitness_intern', 'pred', 'fitness_extern',
                'initial', 'perturb', 'prob', 'selectIter')
  missing <- !(required %in% fnames)
  if(any(missing))
    stop(paste("The following functions are missing from the 'func' argument:",
               paste(required[missing], sep = "", collapse = ",")))
  invisible(x)
  args <- lapply(x, function(x) names(formals(x)))
  expected <- list(fit = c('x', 'y', 'lev', 'last', '...'),
                   fitness_intern = c('object', 'x', 'y', 'maximize', 'p'),
                   pred = c('object', 'x'),
                   initial = c('vars', 'prob', '...'),
                   perturb = c('x', 'vars', 'number'),
                   prob = c('old', 'new', 'iteration'),
                   selectIter = c('x', 'metric', 'maximize'))

  if(is.vector(x$initial)) {
    x <- x[!(names(x) == "initial")]
    expected <- expected[!(names(expected) == "initial")]
  }

  check_names <- names(x)
  check_names <- check_names[check_names != "fitness_extern"]
  for(i in check_names) {
    .args <- names(formals(x[[i]]))
    .check <- same_args(.args, expected[[i]])
    if(!.check) {
      stop(paste("Arguments to function", i, "should be {",
                 paste(expected[[i]], sep = "", collapse = ", "),
                 "}  and these were given {",
                 paste(.args, sep = "", collapse = ", "), "}\n"))
    }
  }
}



sa_bl_correct0 <- function(x) {
  extras <- c('Best', 'Note', 'Random', 'Prob', 'Iter',
              'Cycle', 'SinceRestart', 'Size', 'Resample')
  bl <- x[which.min(x$Iter),]
  bl <- bl[, !(names(bl) %in% extras)]
  perf_names <- names(bl)
  for(i in perf_names) x[, i] <- x[,i] - bl[,i]
  x
}

sa_bl_correct <- function(x) ddply(x, .(Resample), sa_bl_correct0 )

#' @importFrom utils getFromNamespace
#' @export
print.safs <- function (x, top = 5,
                        digits = max(3, getOption("digits") - 3),
                        ...) {
  cat("\nSimulated Annealing Feature Selection\n\n")

  cat(x$dims[1],
      " samples\n",
      x$dims[2],
      " predictor", ifelse(x$dims[2] > 1, "s\n", "\n"),
      sep = "")
  if(!is.null(x$levels)){
    cat(length(x$levels),
        "classes:",
        paste("'", x$levels, "'", sep = "", collapse = ", "),
        "\n")
  }
  cat("\n")

  cat("Maximum search iterations:", max(x$iters), "\n")
  if(x$control$improve < Inf) {
    num_re <- ddply(x$internal, "Resample", function(x) sum(x$Note == "Restart"))
    cat("Restart after ", x$control$improve, " iterations without improvement (",
        round(mean(num_re$V1), 1), " restarts on average)\n", sep = "")
  }
  cat("\n")
  inames <- names(x$internal)
  inames <- inames[!(inames %in% sa_internal_names)]
  enames <- names(x$external)
  enames <- enames[!(enames %in% sa_external_names)]
  cat("Internal performance value", ifelse(length(inames) > 1, "s: ", ": "),
      paste(inames, sep = "", collapse = ", "), "\n", sep = "")
  cat("Subset selection driven to",
      if(x$control$maximize["internal"]) "maximize internal" else "minimize internal",
      x$control$metric["internal"],  "\n")
  cat("\n")
  cat("External performance value", ifelse(length(enames) > 1, "s: ", ": "),
      paste(enames, sep = "", collapse = ", "), "\n", sep = "")
  if(x$auto) {
    cat("Best iteration chose by",
        if(x$control$maximize["external"]) "maximizing external" else "minimizing external",
        x$control$metric["external"],  "\n")
  } else {
    cat("Best iteration chosen manually\n")
  }
  resampleN <- unlist(lapply(x$control$index, length))
  numResamp <- length(resampleN)
  resampText <- getFromNamespace("resampName", "caret")(x)
  cat("External resampling method:", resampText, "\n")
  if(x$control$holdout > 0)
    cat("Subsampling for internal fitness calculation: ",
        round(x$control$holdout*100, digits), "%\n", sep = "")

  cat("\n")

  vars <- sort(table(unlist(x$resampled_vars)), decreasing = TRUE)

  top <- min(top, length(vars))

  smallVars <- vars[1:top]
  smallVars <- round(smallVars/length(x$control$index)*100, 1)

  varText <- paste0(names(smallVars), " (", smallVars, "%)")
  varText <- paste(varText, collapse = ", ")

  if(!all(is.na(smallVars))) {
    cat("During resampling:\n  * the top ",
        top,
        " selected variables (out of a possible ",
        x$dims[2],
        "):\n    ",
        varText,
        "\n",
        sep = "")
    cat("  * on average, ",
        round(mean(unlist(lapply(x$resampled_vars, length))), 1),
        " variables were selected (min = ",
        round(min(unlist(lapply(x$resampled_vars, length))), 1),
        ", max = ",
        round(max(unlist(lapply(x$resampled_vars, length))), 1),
        ")\n\n",
        sep = "")
  } else {
    cat("During resampling, no variables were selected.\n\n")
  }

  cat("In the final search using the entire training set:\n",
      "  *", length(x$optVariables), "features selected at iteration",
      x$optIter, "including:\n    ",
      paste(x$optVariables[1:min(length(x$optVariables), top)],
            sep = "", collapse = ", "),
      if(length(x$optVariables) > top) "..." else "",
      "\n")
  perf_dat <- subset(x$external, Iter == x$optIter)
  perf_dat <- perf_dat[!(names(perf_dat) %in% c("Iter", "Resample"))]
  perf <- colMeans(perf_dat)
  cat("   * external performance at this iteration is\n\n")
  ch_perf  <- format(perf, digits = digits, row.names = FALSE)
  ch_perf[1] <- paste("    ", ch_perf[1])
  print(ch_perf, quote = FALSE)
  cat("\n")

  invisible(x)
}

#' @export
predict.safs <- function (object, newdata, ...) {
  if (any(names(object) == "recipe") && !is.null(object$recipe)) {
    newdata <-
      bake(object$recipe, newdata, all_predictors(), composition = "data.frame")
  } else {
    newdata <- newdata[, object$optVariables, drop = FALSE]
  }
  object$control$functions$pred(object$fit, newdata)
}



#' @title Control parameters for GA and SA feature selection
#'
#' @description
#' Control the computational nuances of the \code{\link{gafs}} and
#' \code{\link{safs}} functions
#'
#' Many of these options are the same as those described for
#' \code{\link[caret]{trainControl}}. More extensive documentation and examples
#' can be found on the \pkg{caret} website at
#' \url{http://topepo.github.io/caret/feature-selection-using-genetic-algorithms.html#syntax} and
#' \url{http://topepo.github.io/caret/feature-selection-using-simulated-annealing.html#syntax}.
#'
#' The \code{functions} component contains the information about how the model
#' should be fit and summarized. It also contains the elements needed for the
#' GA and SA modules (e.g. cross-over, etc).
#'
#' The elements of \code{functions} that are the same for GAs and SAs are:
#' \itemize{
#' \item \code{fit}, with arguments \code{x}, \code{y}, \code{lev},
#' \code{last}, and \code{...}, is used to fit the classification or regression
#' model
#' \item \code{pred}, with arguments \code{object} and \code{x}, predicts
#' new samples
#' \item \code{fitness_intern}, with arguments \code{object},
#' \code{x}, \code{y}, \code{maximize}, and \code{p}, summarizes performance
#' for the internal estimates of fitness
#' \item \code{fitness_extern}, with
#' arguments \code{data}, \code{lev}, and \code{model}, summarizes performance
#' using the externally held-out samples
#' \item \code{selectIter}, with
#' arguments \code{x}, \code{metric}, and \code{maximize}, determines the best
#' search iteration for feature selection.
#' }
#'
#' The elements of \code{functions} specific to genetic algorithms are:
#' \itemize{
#' \item \code{initial}, with arguments \code{vars}, \code{popSize}
#' and \code{...}, creates an initial population.
#' \item \code{selection}, with
#' arguments \code{population}, \code{fitness}, \code{r}, \code{q}, and
#' \code{...}, conducts selection of individuals.
#' \item \code{crossover}, with
#' arguments \code{population}, \code{fitness}, \code{parents} and \code{...},
#' control genetic reproduction.
#' \item \code{mutation}, with arguments
#' \code{population}, \code{parent} and \code{...}, adds mutations.
#' }
#'
#' The elements of \code{functions} specific to simulated annealing are:
#' \itemize{
#' \item \code{initial}, with arguments \code{vars}, \code{prob}, and
#' \code{...}, creates the initial subset.
#' \item \code{perturb}, with
#' arguments \code{x}, \code{vars}, and \code{number}, makes incremental
#' changes to the subsets.
#' \item \code{prob}, with arguments \code{old},
#' \code{new}, and \code{iteration}, computes the acceptance probabilities
#' }
#'
#' The pages \url{http://topepo.github.io/caret/feature-selection-using-genetic-algorithms.html} and
#' \url{http://topepo.github.io/caret/feature-selection-using-simulated-annealing.html} have more details about each of
#' these functions.
#'
#' \code{holdout} can be used to hold out samples for computing the internal
#' fitness value. Note that this is independent of the external resampling
#' step. Suppose 10-fold CV is being used. Within a resampling iteration,
#' \code{holdout} can be used to sample an additional proportion of the 90\%
#' resampled data to use for estimating fitness. This may not be a good idea
#' unless you have a very large training set and want to avoid an internal
#' resampling procedure to estimate fitness.
#'
#' The search algorithms can be parallelized in several places:
#' \enumerate{
#' \item each externally resampled GA or SA can be run independently
#' (controlled by the \code{allowParallel} options)
#' \item within a GA, the
#' fitness calculations at a particular generation can be run in parallel over
#' the current set of individuals (see the \code{genParallel})
#' \item if inner resampling is used, these can be run in parallel (controls depend on the
#' function used. See, for example, \code{\link[caret]{trainControl}})
#' \item any parallelization of the individual model fits. This is also specific to the modeling function.
#' }
#'
#' It is probably best to pick one of these areas for parallelization and the
#' first is likely to produces the largest decrease in run-time since it is the
#' least likely to incur multiple re-starting of the worker processes. Keep in
#' mind that if multiple levels of parallelization occur, this can effect the
#' number of workers and the amount of memory required exponentially.
#'
#' @aliases safsControl gafsControl
#' @param functions a list of functions for model fitting, prediction etc (see
#' Details below)
#' @param method The resampling method: \code{boot}, \code{boot632}, \code{cv},
#' \code{repeatedcv}, \code{LOOCV}, \code{LGOCV} (for repeated training/test
#' splits)
#' @param metric a two-element string that specifies what summary metric will
#' be used to select the optimal number of iterations from the external fitness
#' value and which metric should guide subset selection. If specified, this
#' vector should have names \code{"internal"} and \code{"external"}. See
#' \code{\link{gafs}} and/or \code{\link{safs}} for explanations of the
#' difference.
#' @param maximize a two-element logical: should the metrics be maximized or
#' minimized? Like the \code{metric} argument, this this vector should have
#' names \code{"internal"} and \code{"external"}.
#' @param number Either the number of folds or number of resampling iterations
#' @param repeats For repeated k-fold cross-validation only: the number of
#' complete sets of folds to compute
#' @param verbose a logical for printing results
#' @param returnResamp A character string indicating how much of the resampled
#' summary metrics should be saved. Values can be ``all'' or ``none''
#' @param p For leave-group out cross-validation: the training percentage
#' @param index a list with elements for each resampling iteration. Each list
#' element is the sample rows used for training at that iteration.
#' @param indexOut a list (the same length as \code{index}) that dictates which
#' sample are held-out for each resample. If \code{NULL}, then the unique set
#' of samples not contained in \code{index} is used.
#' @param seeds a vector or integers that can be used to set the seed during
#' each search. The number of seeds must be equal to the number of resamples
#' plus one.
#' @param holdout the proportion of data in [0, 1) to be held-back from
#' \code{x} and \code{y} to calculate the internal fitness values
#' @param improve the number of iterations without improvement before
#' \code{\link{safs}} reverts back to the previous optimal subset
#' @param genParallel if a parallel backend is loaded and available, should
#' \code{\link{gafs}} use it tp parallelize the fitness calculations within a
#' generation within a resample?
#' @param allowParallel if a parallel backend is loaded and available, should
#' the function use it?
#' @return An echo of the parameters specified
#' @author Max Kuhn
#' @seealso \code{\link{safs}}, \code{\link{safs}}, , \code{\link{caretGA}},
#' \code{\link{rfGA}}, \code{\link{treebagGA}}, \code{\link{caretSA}},
#' \code{\link{rfSA}}, \code{\link{treebagSA}}
#' @references \url{http://topepo.github.io/caret/feature-selection-using-genetic-algorithms.html},
#' \url{http://topepo.github.io/caret/feature-selection-using-simulated-annealing.html}
#' @keywords utilities
#' @export safsControl
safsControl <- function(functions = NULL,
                        method = "repeatedcv",
                        metric = NULL,
                        maximize = NULL,
                        number = ifelse(grepl("cv", method), 10, 25),
                        repeats = ifelse(grepl("cv", method), 1, 5),
                        verbose = FALSE,
                        returnResamp = "final",
                        p = .75,
                        index = NULL,
                        indexOut = NULL,
                        seeds = NULL,
                        holdout = 0,
                        improve = Inf,
                        allowParallel = TRUE) {
  if(!(method %in% c("cv", "boot", "repeatedcv", "LGOCV", "LOOCV")))
    stop('method should be one of: "cv", "boot", "repeatedcv", "LGOCV" or "LOOCV"')
  if(holdout < 0 | holdout >= 1) stop("'holdout' should be in [0, 1)")
  if(improve < 2 ) stop("'improve' should be >= 2")

  if(!is.null(metric)) {
    if(length(metric)  != 2)
      stop("'metric' should be a two-element named vector. See ?safsControl")
    if(is.null(names(metric)) || any(sort(names(metric)) != c("external", "internal")))
      stop("'metric' should have names 'internal' and 'external' See ?safsControl")
  }
  if(!is.null(maximize)) {
    if(length(maximize)  != 2)
      stop("'maximize' should be a two-element named vector. See ?safsControl")
    if(is.null(names(maximize)) || any(sort(names(maximize)) != c("external", "internal")))
      stop("'maximize' should have names 'internal' and 'external' See ?safsControl")
  }

  list(functions = if(is.null(functions)) caretFuncs else functions,
       method = method,
       metric = metric,
       maximize = maximize,
       number = number,
       repeats = repeats,
       returnResamp = returnResamp,
       verbose = verbose,
       p = p,
       index = index,
       indexOut = indexOut,
       seeds = seeds,
       holdout = holdout,
       improve = improve,
       allowParallel = allowParallel)
}

#' Simulated annealing feature selection
#'
#' @description
#' Supervised feature selection using simulated annealing
#'
#' \code{\link{safs}} conducts a supervised binary search of the predictor
#' space using simulated annealing (SA). See Kirkpatrick et al (1983) for more
#' information on this search algorithm.
#'
#' @details
#'
#' This function conducts the search of the feature space repeatedly within
#' resampling iterations. First, the training data are split be whatever
#' resampling method was specified in the control function. For example, if
#' 10-fold cross-validation is selected, the entire simulated annealing search
#' is conducted 10 separate times. For the first fold, nine tenths of the data
#' are used in the search while the remaining tenth is used to estimate the
#' external performance since these data points were not used in the search.
#'
#' During the search, a measure of fitness (i.e. SA energy value) is needed to
#' guide the search. This is the internal measure of performance. During the
#' search, the data that are available are the instances selected by the
#' top-level resampling (e.g. the nine tenths mentioned above). A common
#' approach is to conduct another resampling procedure. Another option is to
#' use a holdout set of samples to determine the internal estimate of
#' performance (see the holdout argument of the control function). While this
#' is faster, it is more likely to cause overfitting of the features and should
#' only be used when a large amount of training data are available. Yet another
#' idea is to use a penalized metric (such as the AIC statistic) but this may
#' not exist for some metrics (e.g. the area under the ROC curve).
#'
#' The internal estimates of performance will eventually overfit the subsets to
#' the data. However, since the external estimate is not used by the search, it
#' is able to make better assessments of overfitting. After resampling, this
#' function determines the optimal number of iterations for the SA.
#'
#' Finally, the entire data set is used in the last execution of the simulated
#' annealing algorithm search and the final model is built on the predictor
#' subset that is associated with the optimal number of iterations determined
#' by resampling (although the update function can be used to manually set the
#' number of iterations).
#'
#' This is an example of the output produced when \code{safsControl(verbose =
#' TRUE)} is used:
#'
#' \preformatted{
#' Fold03 1 0.401 (11)
#' Fold03 2 0.401->0.410 (11+1, 91.7\%) *
#' Fold03 3 0.410->0.396 (12+1, 92.3\%) 0.969 A
#' Fold03 4 0.410->0.370 (12+2, 85.7\%) 0.881
#' Fold03 5 0.410->0.399 (12+2, 85.7\%) 0.954 A
#' Fold03 6 0.410->0.399 (12+1, 78.6\%) 0.940 A
#' Fold03 7 0.410->0.428 (12+2, 73.3\%) *
#' }
#'
#' The text "Fold03" indicates that this search is for the third
#' cross-validation fold. The initial subset of 11 predictors had a fitness
#' value of 0.401. The next iteration added a single feature the the existing
#' best subset of 11 (as indicated by "11+1") that increased the fitness value
#' to 0.410. This new solution, which has a Jaccard similarity value of 91.7\%
#' to the current best solution, is automatically accepted. The third iteration
#' adds another feature to the current set of 12 but does not improve the
#' fitness. The acceptance probability for this difference is shown to be
#' 95.6\% and the "A" indicates that this new sub-optimal subset is accepted.
#' The fourth iteration does not show an increase and is not accepted. Note
#' that the Jaccard similarity value of 85.7\% is the similarity to the current
#' best solution (from iteration 2) and the "12+2" indicates that there are two
#' additional features added from the current best that contains 12 predictors.
#'
#' The search algorithm can be parallelized in several places: \enumerate{
#' \item each externally resampled SA can be run independently (controlled by
#' the \code{allowParallel} option of \code{\link{safsControl}}) \item if inner
#' resampling is used, these can be run in parallel (controls depend on the
#' function used. See, for example, \code{\link[caret]{trainControl}}) \item
#' any parallelization of the individual model fits. This is also specific to
#' the modeling function.  }
#'
#' It is probably best to pick one of these areas for parallelization and the
#' first is likely to produces the largest decrease in run-time since it is the
#' least likely to incur multiple re-starting of the worker processes. Keep in
#' mind that if multiple levels of parallelization occur, this can effect the
#' number of workers and the amount of memory required exponentially.
#'
#' @aliases safs.default safs
#' @param x An object where samples are in rows and features are in columns.
#' This could be a simple matrix, data frame or other type (e.g. sparse
#' matrix). For the recipes method, \code{x} is a recipe object.  See Details below.
#' @param y a numeric or factor vector containing the outcome for each sample.
#' @param data an object of class \code{\link{rfe}}.
#' @param iters number of search iterations
#' @param differences a logical: should the difference in fitness values with
#' and without each predictor be calculated?
#' @param safsControl a list of values that define how this function acts. See
#' \code{\link{safsControl}} and URL.
#' @param \dots arguments passed to the classification or regression routine
#' specified in the function \code{safsControl$functions$fit}
#' @return an object of class \code{safs}
#' @author Max Kuhn
#' @seealso \code{\link{safsControl}}, \code{\link{predict.safs}}
#' @references \url{http://topepo.github.io/caret/feature-selection-using-genetic-algorithms.html}
#'
#' \url{http://topepo.github.io/caret/feature-selection-using-simulated-annealing.html}
#'
#' Kuhn and Johnson (2013), Applied Predictive Modeling, Springer
#'
#' Kirkpatrick, S., Gelatt, C. D., and Vecchi, M. P. (1983). Optimization by
#' simulated annealing. Science, 220(4598), 671.
#' @keywords models
#' @examples
#'
#' \dontrun{
#'
#' set.seed(1)
#' train_data <- twoClassSim(100, noiseVars = 10)
#' test_data  <- twoClassSim(10,  noiseVars = 10)
#'
#' ## A short example
#' ctrl <- safsControl(functions = rfSA,
#'                     method = "cv",
#'                     number = 3)
#'
#' rf_search <- safs(x = train_data[, -ncol(train_data)],
#'                   y = train_data$Class,
#'                   iters = 3,
#'                   safsControl = ctrl)
#'
#' rf_search
#' }
#'
#' @export safs
safs <- function (x, ...) UseMethod("safs")

#' @rdname safs
#' @export
"safs.default" <-
  function(x, y,
           iters = 10,
           differences = TRUE,
           safsControl = safsControl(),
           ...) {
    startTime <- proc.time()
    funcCall <- match.call(expand.dots = TRUE)

    if(is.null(safsControl$metric))
      safsControl$metric <- rep(ifelse(is.factor(y), "Accuracy", "RMSE"), 2)
    if(is.null(safsControl$maximize))
      safsControl$maximize <- rep(ifelse(safsControl$metric %in% c("RMSE", "MAE", "logLoss"), FALSE, TRUE), 2)
    if(is.null(names(safsControl$metric)))
      names(safsControl$metric) <- c("internal", "external")
    if(is.null(names(safsControl$maximize)))
      names(safsControl$maximize) <- c("internal", "external")

    if(nrow(x) != length(y)) stop("there should be the same number of samples in x and y")
    numFeat <- ncol(x)
    classLevels <- levels(y)

    if(is.null(safsControl$index))
      safsControl$index <- switch(tolower(safsControl$method),
                                  cv = createFolds(y, safsControl$number, returnTrain = TRUE),
                                  repeatedcv = createMultiFolds(y, safsControl$number, safsControl$repeats),
                                  loocv = createFolds(y, length(y), returnTrain = TRUE),
                                  boot =, boot632 = createResample(y, safsControl$number),
                                  test = createDataPartition(y, 1, safsControl$p),
                                  lgocv = createDataPartition(y, safsControl$number, safsControl$p))

    if(is.null(names(safsControl$index)))
      names(safsControl$index) <- getFromNamespace("prettySeq", "caret")(safsControl$index)

    ## Create hold-out indicies
    if(is.null(safsControl$indexOut)){
      safsControl$indexOut <- lapply(safsControl$index,
                                     function(training, allSamples) allSamples[-unique(training)],
                                     allSamples = seq(along = y))
      names(safsControl$indexOut) <- getFromNamespace("prettySeq", "caret")(safsControl$indexOut)
    }

    if(!is.null(safsControl$seeds)) {
      if(length(safsControl$seeds) < length(safsControl$index) + 1)
        stop(paste("There must be at least",
                   length(safsControl$index) + 1,
                   "random number seeds passed to safsControl"))
    } else {
      safsControl$seeds <- sample.int(100000, length(safsControl$index) + 1)
    }

    ## check summary function and metric
    testOutput <- data.frame(pred = sample(y, min(10, length(y))),
                             obs = sample(y, min(10, length(y))))

    if(is.factor(y))
      for(i in seq(along = classLevels)) testOutput[, classLevels[i]] <- runif(nrow(testOutput))

    test <- safsControl$functions$fitness_extern(testOutput, lev = classLevels)
    perfNames <- names(test)
    if(is.null(perfNames)) {
      warning(paste("The external fitness results should be a *named* vector;",
                    "new name(s) are",
                    paste(paste0("external", 1:length(test)), sep = "", collapse = ", ")),
              immediate. = TRUE)
      perfNames <- paste0("external", 1:length(test))
    }

    if(!(safsControl$metric["external"] %in% perfNames)) {
      warning(paste("The metric '", safsControl$metric["external"], "' is not created by the external summary function; '",
                    perfNames[1], "' will be used instead", sep = ""))
      safsControl$metric["external"] <- perfNames[1]
    }

    `%op%` <- getOper(safsControl$allowParallel && getDoParWorkers() > 1)
    #     sa_resampled <- external <- vector(mode = "list", length = length(safsControl$index))
    result <- foreach(i = seq(along = safsControl$index), .combine = "c", .verbose = FALSE, .errorhandling = "stop") %op% {
      sa_select(x[safsControl$index[[i]],,drop=FALSE],
                y[safsControl$index[[i]]],
                funcs = safsControl$functions,
                sa_metric = safsControl$metric,
                sa_maximize = safsControl$maximize,
                iters = iters,
                sa_verbose = safsControl$verbose,
                testX = x[safsControl$indexOut[[i]],,drop=FALSE],
                testY = y[safsControl$indexOut[[i]]],
                sa_seed = safsControl$seeds[i],
                improve = safsControl$improve,
                Resample = names(safsControl$index)[i],
                holdout = safsControl$holdout,
                lvl = classLevels,
                ...)
    }

    external <- result[names(result) == "external"]
    external <- do.call("rbind", external)
    rownames(external) <- NULL
    internal <- result[names(result) == "internal"]
    internal <- do.call("rbind", internal)
    rownames(internal) <- NULL
    selected_vars <- result[names(result) == "final"]
    names(selected_vars) <- names(safsControl$index)
    if(differences) {
      diffs <- try(process_diffs(result[names(result) == "diffs"],
                                 colnames(x)),
                   silent = TRUE)
      if (inherits(diffs, "try-error")) {

        msg <- strsplit(as.character(diffs), " :", fixed = TRUE)[[1]][2]
        warning(
          paste0("Variable differences could not be computed:", msg)
        )
        diffs <- NULL
      }
    } else diffs <- NULL
    rm(result)

    if(safsControl$verbose) cat("+ final SA\n")

    if(safsControl$holdout > 0) {
      in_holdout <- createDataPartition(y,
                                        p = safsControl$holdout,
                                        list = FALSE)
      in_model <- seq(along = y)[-unique(in_holdout)]
    } else {
      in_model <- seq(along = y)
      in_holdout <- NULL
    }
    final_sa <- sa_select(x[in_model,,drop=FALSE],
                          y[in_model],
                          funcs = safsControl$functions,
                          sa_metric = safsControl$metric,
                          sa_maximize = safsControl$maximize,
                          iters = iters,
                          sa_verbose = safsControl$verbose,
                          testX = if(!is.null(in_holdout)) x[in_holdout,,drop=FALSE] else NULL,
                          testY = if(!is.null(in_holdout)) y[in_holdout] else NULL,
                          sa_seed = safsControl$seeds[length(safsControl$seeds)],
                          improve = safsControl$improve,
                          lvl = classLevels,
                          ...)

    averages <- ddply(external, .(Iter),
                      function(x, nms) {
                        apply(x[, perfNames, drop = FALSE], 2, mean)
                      },
                      nms = perfNames)

    if(!is.null(safsControl$functions$selectIter)) {
      best_index <- safsControl$functions$selectIter(averages,
                                                     metric = safsControl$metric["external"],
                                                     maximize = safsControl$maximize["external"])
      best_iter <- averages$Iter[best_index]
      best_vars <- colnames(x)[final_sa$subsets[[best_index]]]
    } else {
      best_index <- if(safsControl$maximize["external"])
        which.max(averages[,safsControl$metric["external"]]) else
          which.min(averages[,safsControl$metric["external"]])
      best_iter <- averages$Iter[best_index]
      best_vars <- colnames(x)[final_sa$subsets[[best_index]]]
    }
    if(safsControl$verbose) cat("+ final model\n")

    fit <- safsControl$functions$fit(x[, best_vars, drop=FALSE], y, lev = lvls, last = TRUE, ...)

    endTime <- proc.time()
    res <- list(fit = fit,
                sa = final_sa,
                external = external,
                internal = internal,
                all_vars  = colnames(x),
                resampled_vars = selected_vars,
                averages = averages,
                iters = iters,
                optVariables = best_vars,
                optIter = best_iter,
                control = safsControl,
                dims = dim(x),
                differences = diffs,
                perfNames = perfNames,
                auto = TRUE,
                the_dots = list(...),
                recipe = NULL,
                times = list(everything = endTime - startTime),
                levels = if(is.factor(y)) classLevels else NULL)

    class(res) <- "safs"
    res
  }



#' Ancillary simulated annealing functions
#'
#' @description
#' Built-in functions related to simulated annealing
#'
#' These functions are used with the \code{functions} argument of the
#' \code{\link{safsControl}} function. More information on the details of these
#' functions are at \url{http://topepo.github.io/caret/feature-selection-using-simulated-annealing.html}.
#'
#' The \code{initial} function is used to create the first predictor subset.
#' The function \code{safs_initial} randomly selects 20\% of the predictors.
#' Note that, instead of a function, \code{\link{safs}} can also accept a
#' vector of column numbers as the initial subset.
#'
#' \code{safs_perturb} is an example of the operation that changes the subset
#' configuration at the start of each new iteration. By default, it will change
#' roughly 1\% of the variables in the current subset.
#'
#' The \code{prob} function defines the acceptance probability at each
#' iteration, given the old and new fitness (i.e. energy values). It assumes
#' that smaller values are better. The default probability function computed
#' the percentage difference between the current and new fitness value and
#' using an exponential function to compute a probability: \preformatted{ prob
#' = exp[(current-new)/current*iteration] }
#'
#' @aliases safs_initial safs_perturb safs_prob caretSA rfSA treebagSA
#' @param vars the total number of possible predictor variables
#' @param prob The probability that an individual predictor is included in the
#' initial predictor set
#' @param x the integer index vector for the current subset
#' @param old,new fitness values associated with the current and new subset
#' @param iteration the number of iterations overall or the number of
#' iterations since restart (if \code{improve} is used in
#' \code{\link{safsControl}})
#' @param number the number of predictor variables to perturb
#' @param \dots not currently used
#' @return The return value depends on the function. Note that the SA code
#' encodes the subsets as a vector of integers that are included in the subset
#' (which is different than the encoding used for GAs).
#'
#' The objects \code{caretSA}, \code{rfSA} and \code{treebagSA} are example
#' lists that can be used with the \code{functions} argument of
#' \code{\link{safsControl}}.
#'
#' In the case of \code{caretSA}, the \code{...} structure of
#' \code{\link{safs}} passes through to the model fitting routine. As a
#' consequence, the \code{\link{train}} function can easily be accessed by
#' passing important arguments belonging to \code{\link{train}} to
#' \code{\link{safs}}. See the examples below. By default, using \code{caretSA}
#' will used the resampled performance estimates produced by
#' \code{\link{train}} as the internal estimate of fitness.
#'
#' For \code{rfSA} and \code{treebagSA}, the \code{randomForest} and
#' \code{bagging} functions are used directly (i.e. \code{\link{train}} is not
#' used). Arguments to either of these functions can also be passed to them
#' though the \code{\link{safs}} call (see examples below). For these two
#' functions, the internal fitness is estimated using the out-of-bag estimates
#' naturally produced by those functions. While faster, this limits the user to
#' accuracy or Kappa (for classification) and RMSE and R-squared (for
#' regression).
#' @author Max Kuhn
#' @seealso \code{\link{safs}}, \code{\link{safsControl}}
#' @references \url{http://topepo.github.io/caret/feature-selection-using-simulated-annealing.html}
#' @examples
#'
#' selected_vars <- safs_initial(vars = 10 , prob = 0.2)
#' selected_vars
#'
#' ###
#'
#' safs_perturb(selected_vars, vars = 10, number = 1)
#'
#' ###
#'
#' safs_prob(old = .8, new = .9, iteration = 1)
#' safs_prob(old = .5, new = .6, iteration = 1)
#'
#' grid <- expand.grid(old = c(4, 3.5),
#'                     new = c(4.5, 4, 3.5) + 1,
#'                     iter = 1:40)
#' grid <- subset(grid, old < new)
#'
#' grid$prob <- apply(grid, 1,
#'                    function(x)
#'                      safs_prob(new = x["new"],
#'                                old= x["old"],
#'                                iteration = x["iter"]))
#'
#' grid$Difference <- factor(grid$new - grid$old)
#' grid$Group <- factor(paste("Current Value", grid$old))
#'
#' ggplot(grid, aes(x = iter, y = prob, color = Difference)) +
#'   geom_line() + facet_wrap(~Group) + theme_bw() +
#'   ylab("Probability") + xlab("Iteration")
#'
#' \dontrun{
#' ###
#' ## Hypothetical examples
#' lda_sa <- safs(x = predictors,
#'                y = classes,
#'                safsControl = safsControl(functions = caretSA),
#'                ## now pass arguments to `train`
#'                method = "lda",
#'                metric = "Accuracy"
#'                trControl = trainControl(method = "cv", classProbs = TRUE))
#'
#' rf_sa <- safs(x = predictors,
#'               y = classes,
#'               safsControl = safsControl(functions = rfSA),
#'               ## these are arguments to `randomForest`
#'               ntree = 1000,
#'               importance = TRUE)
#' 	}
#'
#'
#'
#' @export safs_initial
safs_initial <- function (vars, prob = .20, ...)  {
  sort(sample.int(vars, size = floor(vars*prob)+1))
}

#' @rdname safs_initial
#' @export
safs_perturb <- function(x, vars, number = floor(length(x)*.01) + 1) {
  bin <- index2vec(x, vars)
  change <- sample(seq(along = bin), size = number)
  bin[change] <- ifelse(bin[change] == 1, 0, 1)
  sort(which(bin == 1))
}

#' @rdname safs_initial
#' @export
safs_prob <- function(old, new, iteration = 1) {
  if(new < old) return(1)
  ediff <- as.vector(old - new)
  ediff <- ediff/abs(old)
  exp(ediff*iteration)
}

sa_wrapper <- function(ind, x, y, funcs, holdoutX, holdoutY, testX, testY,
                       perf, holdoutPerf, testPerf,
                       sa_metric, sa_maximize, lvl = lvl, last = FALSE, ...) {
  mod <- funcs$fit(x[, ind, drop=FALSE], y, lev = lvl, last = last,...)
  if (!is.null(holdoutX)) {
    intern_x <- holdoutX[, ind, drop = FALSE]
    if(!is.null(holdoutPerf))
      intern_x <- cbind(intern_x, holdoutPerf)
  } else {
    intern_x <- x[, ind, drop = FALSE]
    if(!is.null(perf))
      intern_x <- cbind(intern_x, perf)
  }
  internal <- funcs$fitness_intern(
    mod,
    x = intern_x,
    y = if(!is.null(holdoutY)) holdoutY else y,
    p = ncol(x)
    )
  if(!is.null(testX)) {
    modelPred <- funcs$pred(mod, testX[, ind, drop=FALSE])
    if(is.data.frame(modelPred) | is.matrix(modelPred)) {
      if(is.matrix(modelPred)) modelPred <- as.data.frame(modelPred, stringsAsFactors = TRUE)
      modelPred$obs <- testY
      modelPred$Size <- length(ind)
    } else modelPred <- data.frame(pred = modelPred, obs = testY, Size = sum(ind == 1))
    if(!is.null(testPerf))
      modelPred <- cbind(modelPred, testPerf)
    # perf_data for test set

    external <- funcs$fitness_extern(modelPred, lev = levels(testY))
    if(is.null(names(external))) {
      names(external) <- paste0("external", 1:length(external))
    }
  } else external <- NULL

  if(sa_maximize["internal"])
    internal[sa_metric["internal"]] <- -internal[sa_metric["internal"]]

  list(internal = internal, external = external)
}

###################################################################
##

#' @importFrom stats runif
sa_select <- function(x, y,
                      ## testX, testY: optional holdout data for computing
                      ## the fitness function
                      testX = NULL, testY = NULL,
                      # added for recipes only
                      perf = NULL,
                      testPerf = NULL,
                      iters = 20,
                      funcs = NULL,
                      sa_metric = NULL,
                      sa_maximize = TRUE,
                      sa_verbose = TRUE,
                      holdout = 0,
                      sa_seed = NULL,
                      improve = 10,
                      lvl = NULL,
                      Resample = "",
                      ...) {
  sa_func_check(funcs)
  if(!is.null(sa_seed)) set.seed(sa_seed[1])
  dig <- options()$digits

  if(holdout > 0) {
    in_holdout <- createDataPartition(y,
                                      p = holdout,
                                      list = FALSE)
    holdout_x <- x[in_holdout,,drop = FALSE]
    holdout_y <- y[in_holdout]
    holdout_perf <- perf[in_holdout,,drop = FALSE]
    x <- x[-in_holdout,,drop = FALSE]
    y <- y[-in_holdout]
    perf <- perf[-in_holdout,,drop = FALSE]

  } else {
    holdout_x <- NULL
    holdout_y <- NULL
    holdout_perf <- NULL
  }


  p <- ncol(x)
  cycle <- 1
  since_restart <- 0
  since_improve <- 0
  last_improve <- 0

  restarts <- 1
  subsets <- vector(mode = "list", length = iters)
  internal <- data.frame(
    Best = rep(0*NA, iters),
    Note = "",
    Random = runif(iters),
    Prob = rep(1, iters),
    Iter = 1:(iters),
    Cycle = rep(0*NA, iters),
    SinceRestart = rep(0*NA, iters),
    Size = rep(0*NA, iters),
    Similarity = rep(0*NA, iters),
    Similarity_B = rep(0*NA, iters),
    stringsAsFactors = FALSE
  )
  external <- if(!is.null(testX)) data.frame(Iter = 1:(iters)) else NULL

  for(i in 1:iters){
    if(i == 1)  {
      if(is.function(funcs$initial)) {
        best_subset <- new_subset <- current_subset <- funcs$initial(vars = p)
      } else {
        if(max(funcs$initial) > p)
          stop(paste("The initial vector uses columns not in the data"))
        best_subset <- new_subset <- current_subset <- funcs$initial
      }
    } else  {
      new_subset <- funcs$perturb(current_subset, vars = p)
    }

    if(length(new_subset) == 0) new_subset <- sample.int(p, 1)
    subsets[[i]] <- new_subset
    if(i > 1) {
      internal$Similarity[i] <- jack_sim(index2vec(subsets[[i-1]], p),
                                         index2vec(subsets[[i  ]], p))
      internal$Similarity_B[i] <- jack_sim(index2vec(best_subset, p),
                                           index2vec(new_subset, p))
    }

    since_restart <- since_restart + 1
    internal$SinceRestart[i] <- since_restart
    new_obj <- sa_wrapper(ind = new_subset,
                          x = x, y = y,
                          funcs,
                          holdoutX = holdout_x, holdoutY = holdout_y,
                          testX = testX, testY = testY,
                          # perf_data for holdout and test
                          perf = perf,
                          holdoutPerf = holdout_perf,
                          testPerf = testPerf,
                          sa_metric = sa_metric,
                          sa_maximize = sa_maximize,
                          lvl = lvl,
                          last = Resample == "",
                          ...)

    ## Use the initial results to setup containers for
    ## the remaining iterations
    if(i == 1) {
      k <- length(new_obj$internal)
      perf_names <- names(new_obj$internal)
      for(new_var in perf_names) internal[,new_var] <- NA
      nr <- ncol(internal)
      internal[1, (nr-k+1):nr] <- new_obj$internal
      if(!is.null(testX)) {
        for(new_var in names(new_obj$external)) external[,new_var] <- NA
        external[1, -1] <- new_obj$external
      }
    } else {
      internal[i, (nr-k+1):nr] <- new_obj$internal
      if(!is.null(testX)) external[i, -1] <- new_obj$external
    }

    if(sa_verbose){
      if(i > 1) {
        cat(Resample, " ", format(1:iters)[i], " ",
            if(sa_maximize["internal"])
              signif(-internal$Best[i-1], digits = dig) else
                signif(internal$Best[i-1], digits = dig),
            "->" ,
            if(sa_maximize["internal"])
              signif(-new_obj$internal[sa_metric["internal"]], digits = dig) else
                signif(new_obj$internal[sa_metric["internal"]], digits = dig),
            change_text(best_subset, new_subset, p),
            sep = "")
      } else {
        cat(Resample, " ", format(1:iters)[i], " ",
            if(sa_maximize["internal"])
              signif(-new_obj$internal[sa_metric["internal"]], digits = dig) else
                signif(new_obj$internal[sa_metric["internal"]], digits = dig),
            " (" , length(best_subset), ")\n",
            sep = "")
      }
    }

    internal$Size[i] <- length(new_subset)
    internal$Cycle[i] <- cycle
    if(i == 1 || new_obj$internal[sa_metric["internal"]] < internal$Best[i-1]) {
      current_subset <- new_subset
      best_subset <- new_subset
      internal$Best[i] <- new_obj$internal[sa_metric["internal"]]
      internal$Note[i] <- "Improved"
      last_improve <- i
      since_improve <- 0
      if(sa_verbose & i > 1) cat(" *\n")
    } else {
      if(i > 1) {
        internal$Prob[i] <- funcs$prob(old = internal$Best[i-1],
                                       new = new_obj$internal[sa_metric["internal"]],
                                       iteration = since_restart)
        since_improve <- since_improve + 1
        if(sa_verbose)
          cat(" ", signif(internal$Prob[i], digits = dig), " ")
      } else internal$Prob[i] <- 1

      if(internal$Prob[i] > internal$Random[i]) {
        current_subset <- new_subset
        internal$Best[i] <- internal$Best[i-1]
        internal$Note[i] <- "Accepted"
        if(sa_verbose & i > 1) cat("A\n")
      } else {
        internal$Obj[i] <- internal$Obj[i-1]
        internal$Best[i] <- internal$Best[i-1]
        internal$Note[i] <- "Discarded"
        if(sa_verbose & i > 1) cat("\n")
      }
    }

    if(since_improve == improve) {
      internal$Note[i] <- "Restart"
      current_subset <- subsets[[last_improve]]
      cycle <- cycle + 1
      since_restart <- 0
      since_improve <- 0
      if(sa_verbose)
        cat(Resample, "restart, goto iter", last_improve, "\n")
    }
  }
  if(sa_maximize["internal"]) {
    internal[, sa_metric["internal"]] <- -internal[, sa_metric["internal"]]
    internal$Best <- -internal$Best
  }
  mod <- funcs$fit(x[, best_subset, drop=FALSE], y, lev = lvl, last = TRUE, ...)
  if(Resample != "") internal$Resample <- Resample
  if(Resample != "" && !is.null(testX)) external$Resample <- Resample

  diffs <- try(get_fitness_differences(colnames(x),
                                       subsets,
                                       external[, !(names(external) %in% sa_external_names), drop = FALSE]),
               silent = TRUE)
  if (inherits(diffs, "try-error"))  diffs <- NULL
  list(internal = internal,
       subsets = subsets,
       external = external,
       final = names(x)[best_subset],
       fit = mod,
       diffs = diffs)
}


###################################################################
##

#' @importFrom stats update
#' @method plot safs
#' @export
#' @export plot.safs
plot.safs <- function(x,
                      metric = x$control$metric["external"],
                      estimate = c("internal", "external"),
                      output = "ggplot",
                      ...) {
  int_names <- names(x$internal)[!(names(x$internal) %in% sa_internal_names)]
  ext_names <- names(x$external)[!(names(x$external) %in% sa_external_names)]
  common <- intersect(int_names, ext_names)
  both_estimates <- length(estimate) == 2  && all(sort(estimate) == c("external", "internal"))

  if(both_estimates){
    if(!metric %in% common) stop(paste("'", metric, "' not computed in both estimates"))
    tmp_e <- x$external[, c("Iter", "Resample", common)]
    tmp_e$Estimate <- "External"
    tmp_i <- x$internal[, c("Iter", "Resample", common)]
    tmp_i$Estimate <- "Internal"
    plot_dat <- rbind(tmp_e, tmp_i)
  } else {
    if("internal" %in% estimate) {
      if(!metric %in% int_names) stop(paste("'", metric, "' not computed internally"))
      plot_dat <- x$internal[, c("Iter", "Resample", int_names)]
    }
    if("external" %in% estimate) {
      if(!metric %in% int_names) stop(paste("'", metric, "' not computed externally"))
      plot_dat <- x$external[, c("Iter", "Resample", ext_names)]
    }
  }
  if(output == "data") out <- plot_dat
  plot_dat <- if(both_estimates)
    ddply(plot_dat, c("Iter", "Estimate"),
          function(x) c(Mean = mean(x[, metric]))) else
            ddply(plot_dat, c("Iter"),
                  function(x) c(Mean = mean(x[, metric])))

  if(output == "ggplot") {
    out <- if(both_estimates)
      ggplot(plot_dat, aes(x = Iter, y = Mean, color = Estimate)) + geom_point() else
        ggplot(plot_dat, aes(x = Iter, y = Mean)) + geom_point()
    out <- out + xlab("Iteration")

  }
  if(output == "lattice") {
    out <- if(both_estimates)
      xyplot(Mean ~ Iter, data = plot_dat, groups = Estimate, ...) else
        xyplot(Mean ~ Iter, data = plot_dat, ...)
    out <- update(out, xlab = "Iteration")
  }
  out
}


#' @method ggplot safs
#' @export ggplot.safs
#' @export
#' @rdname plot.gafs
ggplot.safs <-
  function (data = NULL, mapping = NULL, ..., environment = NULL) {
    plot.safs(x = data, ...)
  }

###################################################################
##
#' @rdname safs_initial
#' @importFrom stats predict
#' @export
caretSA <- list(fit = function(x, y, lev = NULL, last = FALSE, ...) train(x, y, ...),
                pred = function(object, x) {
                  tmp <- predict(object, x)
                  if(object$control$classProbs) {
                    out <- cbind(data.frame(pred = tmp),
                                 as.data.frame(predict(object, x, type = "prob"), stringsAsFactors = TRUE))
                  } else out <- tmp
                  out
                },
                fitness_intern = function(object, x, y, maximize, p){
                  perf_val <- getTrainPerf(object)
                  perf_val <- perf_val[names(perf_val) != "method"]
                  perf_val <- unlist(perf_val)
                  names(perf_val) <- gsub("Train", "", names(perf_val))
                  perf_val
                },
                fitness_extern = defaultSummary,
                initial = safs_initial,
                perturb = safs_perturb,
                prob = safs_prob,
                selectIter = best)

#' @rdname safs_initial
#' @export
treebagSA <- list(fit = function(x, y, lev = NULL, last = FALSE, ...) {
  loadNamespace("ipred")
  ipred::ipredbagg(y, x, ...)
},
pred = function(object, x) {
  tmp <- predict(object, x)
  if(is.factor(object$y)) {
    out <- cbind(data.frame(pred = tmp),
                 as.data.frame(predict(object, x, type = "prob"), stringsAsFactors = TRUE))
  } else out <- tmp
  out
},
fitness_intern = function(object, x, y, maximize, p)
  ipredStats(object)[1:2],
fitness_extern = defaultSummary,
initial = safs_initial,
perturb = safs_perturb,
prob = safs_prob,
selectIter = best)

#' @rdname safs_initial
#' @export
rfSA <-  list(fit = function(x, y, lev = NULL, last = FALSE, ...) {
  loadNamespace("randomForest")
  randomForest::randomForest(x, y, ...)
},
pred = function(object, x) {
  tmp <- predict(object, x)
  if(is.factor(object$y)) {
    out <- cbind(data.frame(pred = tmp),
                 as.data.frame(predict(object, x, type = "prob"), stringsAsFactors = TRUE))
  } else out <- tmp
  out
},
fitness_intern = function(object, x, y, maximize, p) rfStats(object),
fitness_extern = defaultSummary,
initial = safs_initial,
perturb = safs_perturb,
prob = safs_prob,
selectIter = best)



#' Update or Re-fit a SA or GA Model
#'
#' @description
#' \code{update} allows a user to over-ride the search iteration selection
#' process.
#'
#' Based on the results of plotting a \code{\link{gafs}} or \code{\link{safs}}
#' object, these functions can be used to supersede the number of iterations
#' determined analytically from the resamples.
#'
#' Any values of \code{...} originally passed to \code{\link{gafs}} or
#' \code{\link{safs}} are automatically passed on to the updated model (i.e.
#' they do not need to be supplied again to \code{update}.
#'
#' @aliases update.safs update.gafs
#' @param object An object produced by \code{\link{gafs}} or \code{\link{safs}}
#' @param iter a single numeric integer
#' @param x,y the original training data used in the call to \code{\link{gafs}}
#' or \code{\link{safs}}. Only required for non-recipe methods.
#' @param \dots not currently used
#' @return an object of class \code{\link{gafs}} or \code{\link{safs}}
#' @author Max Kuhn
#' @seealso \code{\link{gafs}}, \code{\link{safs}}
#' @keywords models
#' @examples
#'
#' \dontrun{
#' set.seed(1)
#' train_data <- twoClassSim(100, noiseVars = 10)
#' test_data  <- twoClassSim(10,  noiseVars = 10)
#'
#' ## A short example
#' ctrl <- safsControl(functions = rfSA,
#'                     method = "cv",
#'                     number = 3)
#'
#' rf_search <- safs(x = train_data[, -ncol(train_data)],
#'                   y = train_data$Class,
#'                   iters = 3,
#'                   safsControl = ctrl)
#'
#' rf_search2 <- update(rf_search,
#' 	                 iter = 1,
#' 	                 x = train_data[, -ncol(train_data)],
#'                      y = train_data$Class)
#' rf_search2
#' }
#' @method update safs
#' @export
update.safs <- function(object, iter, x, y, ...) {
  iter <- iter[1]
  if (iter > length(object$sa$subsets))
    stop(paste("iter must be less than", length(object$sa$subsets)))
  if (!is.null(object$recipe)) {
    if (is.null(object$recipe$template))
      stop("Recipe is missing data to be juiced.", call. = FALSE)
    args <-
      list(x = juice(object$recipe, all_predictors(), composition = "data.frame"),
           y = juice(object$recipe, all_outcomes(), composition = "data.frame")[[1]],
           lev = object$levels,
           last = TRUE)
  } else {
    if (is.null(x) | is.null(y))
      stop("the original training data is needed to refit the model")
    args <- list(x = x[, object$sa$subsets[[iter]], drop=FALSE],
                 y = y, lev = object$levels, last = TRUE)
  }

  if (length(object$the_dots) > 0)
    args <- c(args, object$the_dots)
  if (object$control$verbose)
    cat("Refitting model to use", length(object$sa$subsets[[iter]]),
        "predictors from iteration", iter, "\n")
  object$fit <- do.call(object$control$functions$fit, args)
  object$auto <- FALSE
  object$optVariables <- colnames(args$x)[object$sa$subsets[[iter]]]
  object$optIter <- iter
  object
}

#' @export
"varImp.safs" <- function(object,
                          metric = object$control$metric["external"],
                          maximize = object$control$maximize["external"],
                          ...) {

  if(is.null(object$differences))
    stop("must have used `differences = TRUE`")
  out <- object$differences[,metric, drop = FALSE]
  rownames(out) <- as.character(object$differences$Variable)
  if(!maximize) out[, metric, drop = FALSE] <- -out[, metric, drop = FALSE]
  out <- out[order(-out[, metric]),, drop = FALSE]
  out
}

#' @rdname safs
#' @method safs recipe
#' @import recipes
#' @export
"safs.recipe" <-
  function(x, data,
           iters = 10,
           differences = TRUE,
           safsControl = safsControl(),
           ...) {
    startTime <- proc.time()
    funcCall <- match.call(expand.dots = TRUE)

    if(safsControl$verbose)
      cat("Preparing recipe\n")

    orig_rec <- x
    trained_rec <- prep(
      x, training = data,
      fresh = TRUE,
      retain = TRUE,
      verbose = FALSE,
      stringsAsFactors = TRUE
    )
    x <- juice(trained_rec, all_predictors(), composition = "data.frame")
    y <- juice(trained_rec, all_outcomes(), composition = "data.frame")
    if(ncol(y) > 1)
      stop("`safs` doesn't support multivariate outcomes", call. = FALSE)
    y <- y[[1]]
    is_weight <- summary(trained_rec)$role == "case weight"
    if(any(is_weight))
      stop("`safs` does not allow for weights.", call. = FALSE)

    is_perf <- summary(trained_rec)$role == "performance var"
    if(any(is_perf)) {
      perf_data <- juice(trained_rec, has_role("performance var"))
    } else perf_data <- NULL

    if(is.null(safsControl$metric))
      safsControl$metric <- rep(ifelse(is.factor(y), "Accuracy", "RMSE"), 2)
    if(is.null(safsControl$maximize))
      safsControl$maximize <- rep(ifelse(safsControl$metric %in% c("RMSE", "MAE", "logLoss"), FALSE, TRUE), 2)
    if(is.null(names(safsControl$metric)))
      names(safsControl$metric) <- c("internal", "external")
    if(is.null(names(safsControl$maximize)))
      names(safsControl$maximize) <- c("internal", "external")

    if(nrow(x) != length(y)) stop("there should be the same number of samples in x and y")
    numFeat <- ncol(x)
    classLevels <- levels(y)

    if(is.null(safsControl$index))
      safsControl$index <- switch(
        tolower(safsControl$method),
        cv = createFolds(y, safsControl$number, returnTrain = TRUE),
        repeatedcv = createMultiFolds(y, safsControl$number, safsControl$repeats),
        loocv = createFolds(y, length(y), returnTrain = TRUE),
        boot =, boot632 = createResample(y, safsControl$number),
        test = createDataPartition(y, 1, safsControl$p),
        lgocv = createDataPartition(y, safsControl$number, safsControl$p)
      )

    if(is.null(names(safsControl$index)))
      names(safsControl$index) <- getFromNamespace("prettySeq", "caret")(safsControl$index)

    ## Create hold-out indicies
    if(is.null(safsControl$indexOut)){
      safsControl$indexOut <-
        lapply(safsControl$index,
               function(training, allSamples) allSamples[-unique(training)],
               allSamples = seq(along = y))
      names(safsControl$indexOut) <-
        getFromNamespace("prettySeq", "caret")(safsControl$indexOut)
    }

    if(!is.null(safsControl$seeds)) {
      if(length(safsControl$seeds) < length(safsControl$index) + 1)
        stop(paste("There must be at least",
                   length(safsControl$index) + 1,
                   "random number seeds passed to safsControl"))
    } else {
      safsControl$seeds <- sample.int(100000, length(safsControl$index) + 1)
    }

    ## check summary function and metric
    testOutput <- data.frame(pred = sample(y, min(10, length(y))),
                             obs = sample(y, min(10, length(y))))

    if(is.factor(y))
      for(i in seq(along = classLevels)) testOutput[, classLevels[i]] <- runif(nrow(testOutput))
    if(!is.null(perf_data))
      testOutput <- cbind(
        testOutput,
        perf_data[sample(1:nrow(perf_data), nrow(testOutput)),, drop = FALSE]
      )

    test <- safsControl$functions$fitness_extern(testOutput, lev = classLevels)
    perfNames <- names(test)
    if(is.null(perfNames)) {
      warning(paste("The external fitness results should be a *named* vector;",
                    "new name(s) are",
                    paste(paste0("external", 1:length(test)), sep = "", collapse = ", ")),
              immediate. = TRUE)
      perfNames <- paste0("external", 1:length(test))
    }

    if(!(safsControl$metric["external"] %in% perfNames)) {
      warning(paste("The metric '", safsControl$metric["external"], "' is not created by the external summary function; '",
                    perfNames[1], "' will be used instead", sep = ""))
      safsControl$metric["external"] <- perfNames[1]
    }

    `%op%` <- getOper(safsControl$allowParallel && getDoParWorkers() > 1)

    result <- foreach(
      i = seq(along = safsControl$index),
      .combine = "c",
      .verbose = FALSE,
      .errorhandling = "stop",
      .packages = "recipes"
      ) %op% {

      # reprocess recipe
      resampled_rec <- prep(
        orig_rec,
        training = data[safsControl$index[[i]], ],
        fresh = TRUE,
        retain = TRUE,
        verbose = FALSE,
        stringsAsFactors = TRUE
      )
      x_tr <- juice(resampled_rec, all_predictors(), composition = "data.frame")
      y_tr <- juice(resampled_rec, all_outcomes(), composition = "data.frame")
      y_tr <- y_tr[[1]]
      x_te <- bake(resampled_rec, new_data = data[ -safsControl$index[[i]], ],
                   all_predictors(), composition = "data.frame")
      y_te <- bake(resampled_rec, new_data = data[ -safsControl$index[[i]], ],
                   all_outcomes(), composition = "data.frame")
      y_te <- y_te[[1]]

      if(any(is_perf)) {
        perf_tr <- juice(resampled_rec, has_role("performance var"))
        perf_te <- bake(
          resampled_rec,
          new_data = data[ -safsControl$index[[i]], ],
          has_role("performance var")
        )
      } else {
        perf_tr <- NULL
        perf_te <- NULL
      }

      sa_select(
        x = x_tr,
        y = y_tr,
        funcs = safsControl$functions,
        sa_metric = safsControl$metric,
        sa_maximize = safsControl$maximize,
        iters = iters,
        sa_verbose = safsControl$verbose,
        testX = x_te,
        testY = y_te,
        perf =  perf_tr,
        testPerf = perf_te,
        sa_seed = safsControl$seeds[i],
        improve = safsControl$improve,
        Resample = names(safsControl$index)[i],
        holdout = safsControl$holdout,
        lvl = classLevels,
        ...
      )
    }

    external <- result[names(result) == "external"]
    external <- do.call("rbind", external)
    rownames(external) <- NULL
    internal <- result[names(result) == "internal"]
    internal <- do.call("rbind", internal)
    rownames(internal) <- NULL
    selected_vars <- result[names(result) == "final"]
    names(selected_vars) <- names(safsControl$index)
    if(differences) {
      diffs <- try(process_diffs(result[names(result) == "diffs"],
                                 colnames(x)),
                   silent = TRUE)
      if (inherits(diffs, "try-error")) {
        diffs <- NULL
        warning("An error occured when computing the variable differences")
      }
    } else diffs <- NULL
    rm(result)

    if(safsControl$verbose) cat("+ final SA\n")

    if(safsControl$holdout > 0) {
      in_holdout <- createDataPartition(y,
                                        p = safsControl$holdout,
                                        list = FALSE)
      in_model <- seq(along = y)[-unique(in_holdout)]
    } else {
      in_model <- seq(along = y)
      in_holdout <- NULL
    }
    final_sa <- sa_select(
      x = x[in_model,,drop=FALSE],
      y = y[in_model],
      funcs = safsControl$functions,
      sa_metric = safsControl$metric,
      sa_maximize = safsControl$maximize,
      iters = iters,
      sa_verbose = safsControl$verbose,
      testX = if(!is.null(in_holdout)) x[in_holdout,,drop=FALSE] else NULL,
      testY = if(!is.null(in_holdout)) y[in_holdout] else NULL,
      perf = perf_data[in_model,,drop=FALSE],
      testPerf = if(!is.null(in_holdout)) perf_data[in_holdout,,drop=FALSE] else NULL,
      sa_seed = safsControl$seeds[length(safsControl$seeds)],
      improve = safsControl$improve,
      lvl = classLevels,
      ...
    )
    averages <-
      ddply(external, .(Iter),
            function(x, nms) {
              apply(x[, perfNames, drop = FALSE], 2, mean)
            },
            nms = perfNames)

    if(!is.null(safsControl$functions$selectIter)) {
      best_index <-
        safsControl$functions$selectIter(
          averages,
          metric = safsControl$metric["external"],
          maximize = safsControl$maximize["external"]
        )
      best_iter <- averages$Iter[best_index]
      best_vars <- colnames(x)[final_sa$subsets[[best_index]]]
    } else {
      best_index <- if(safsControl$maximize["external"])
        which.max(averages[,safsControl$metric["external"]]) else
          which.min(averages[,safsControl$metric["external"]])
      best_iter <- averages$Iter[best_index]
      best_vars <- colnames(x)[final_sa$subsets[[best_index]]]
    }


    if(safsControl$verbose) cat("+ final model\n")

    fit <- safsControl$functions$fit(x[, best_vars, drop=FALSE], y, lev = lvls, last = TRUE, ...)

    endTime <- proc.time()

    # remove some items that won't be used again
    final_sa$sa$fit <- NULL
    final_sa$sa$final <- NULL
    final_sa$sa$diffs <- NULL

    res <- list(
      fit = fit,
      sa = final_sa,
      external = external,
      internal = internal,
      all_vars  = colnames(x),
      resampled_vars = selected_vars,
      averages = averages,
      iters = iters,
      optVariables = best_vars,
      optIter = best_iter,
      control = safsControl,
      dims = dim(x),
      differences = diffs,
      perfNames = perfNames,
      auto = TRUE,
      the_dots = list(...),
      recipe = trained_rec,
      times = list(everything = endTime - startTime),
      levels = if(is.factor(y)) classLevels else NULL
    )

    class(res) <- "safs"
    res
  }




