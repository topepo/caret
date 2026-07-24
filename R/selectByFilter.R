#' @rdname caret-internal
#' @export
sbfIter <- function(
  x,
  y,
  testX,
  testY,
  testPerf = NULL,
  sbfControl = sbfControl(),
  ...
) {
  if (is.null(colnames(x))) {
    stop("x must have column names")
  }

  if (
    is.null(testX) ||
      is.null(testY)
  ) {
    stop("a test set must be specified")
  }

  if (sbfControl$multivariate) {
    scores <- sbfControl$functions$score(x, y)
    if (length(scores) != ncol(x)) {
      stop(
        paste(
          "when control$multivariate == TRUE, 'scores'",
          "should return a vector with",
          ncol(x),
          "numeric values"
        )
      )
    }
  } else {
    scores <- vapply(x, sbfControl$functions$score, double(1), y = y)
  }

  retained <- sbfControl$functions$filter(scores, x, y)
  ## deal with zero length results

  testX <- testX[, which(retained), drop = FALSE]

  fitObject <-
    sbfControl$functions$fit(
      x = x[, which(retained), drop = FALSE],
      y = y,
      ...
    )

  modelPred <- sbfControl$functions$pred(fitObject, testX)
  if (is.data.frame(modelPred) || is.matrix(modelPred)) {
    if (is.matrix(modelPred)) {
      modelPred <- as.data.frame(modelPred, stringsAsFactors = TRUE)
    }
    modelPred$obs <- testY
  } else {
    modelPred <- data.frame(pred = modelPred, obs = testY)
  }
  if (!is.null(testPerf)) {
    modelPred <- cbind(modelPred, testPerf)
  }

  list(variables = names(retained)[which(retained)], pred = modelPred)
}


######################################################################
######################################################################

#' Selection By Filtering (SBF)
#'
#' Model fitting after applying univariate filters
#'
#' More details on this function can be found at
#' <http://topepo.github.io/caret/feature-selection-using-univariate-filters.html>.
#'
#' This function can be used to get resampling estimates for models when
#' simple, filter-based feature selection is applied to the training data.
#'
#' For each iteration of resampling, the predictor variables are univariately
#' filtered prior to modeling. Performance of this approach is estimated using
#' resampling. The same filter and model are then applied to the entire
#' training set and the final model (and final features) are saved.
#'
#' `sbf` can be used with "explicit parallelism", where different resamples
#' (e.g. cross-validation group) can be split up and run on multiple machines
#' or processors. By default, `sbf` will use a single processor on the host
#' machine. As of version 4.99 of this package, the framework used for parallel
#' processing uses the \pkg{foreach} package. To run the resamples in parallel,
#' the code for `sbf` does not change; prior to the call to `sbf`, a parallel
#' backend is registered with \pkg{foreach} (see the examples below).
#'
#' The modeling and filtering techniques are specified in [sbfControl()].
#' Example functions are given in [lmSBF()].
#'
#' @aliases sbf sbf.default sbf.formula predict.sbf
#' @param x a data frame containing training data where samples are in rows and
#'   features are in columns. For the recipes method, `x` is a recipe object.
#' @param y a numeric or factor vector containing the outcome for each sample.
#' @param form A formula of the form `y ~ x1 + x2 + ...`
#' @param data Data frame from which variables specified in `formula` are
#'   preferentially to be taken.
#' @param subset An index vector specifying the cases to be used in the
#'   training sample. (NOTE: If given, this argument must be named.)
#' @param na.action A function to specify the action to be taken if NAs are
#'   found. The default action is for the procedure to fail. An alternative is
#'   na.omit, which leads to rejection of cases with missing values on any
#'   required variable. (NOTE: If given, this argument must be named.)
#' @param contrasts a list of contrasts to be used for some or all the factors
#'   appearing as variables in the model formula.
#' @param sbfControl a list of values that define how this function acts. See
#'   [sbfControl()]. (NOTE: If given, this argument must be named.)
#' @param object an object of class `sbf`
#' @param newdata a matrix or data frame of predictors. The object must have
#'   non-null column names
#' @param \dots for `sbf`: arguments passed to the classification or regression
#'   routine (such as [randomForest::randomForest()]). For `predict.sbf`:
#'   augments cannot be passed to the prediction function using `predict.sbf`
#'   as it uses the function originally specified for prediction.
#' @return for `sbf`, an object of class `sbf` with elements:
#'
#' * `pred`: if `sbfControl$saveDetails` is `TRUE`, this is a list of
#'           predictions for the hold-out samples at each resampling iteration.
#'           Otherwise it is `NULL`
#' * `variables`: a list of variable names that survived the filter at each
#'                resampling iteration
#' * `results`: a data frame of results aggregated over the resamples
#' * `fit`: the final model fit with only the filtered variables
#' * `optVariables`: the names of the variables that survived the filter using
#'                   the training set
#' * `call`: the function call
#' * `control`: the control object
#' * `resample`: if `sbfControl$returnResamp` is "all", a data frame of the
#'               resampled performance measures. Otherwise, `NULL`
#' * `metrics`: a character vector of names of the performance measures
#' * `dots`: a list of optional arguments that were passed in
#'
#' For `predict.sbf`, a vector of predictions.
#' @author Max Kuhn
#' @seealso [sbfControl()]
#' @family feature-selection
#' @keywords models
#' @examplesIf !caret:::is_cran_check()
#'
#' data(BloodBrain)
#'
#' ## Use a GAM is the filter, then fit a random forest model
#' RFwithGAM <- sbf(
#'   bbbDescr,
#'   logBBB,
#'   sbfControl = sbfControl(functions = rfSBF, verbose = FALSE, method = "cv")
#' )
#' RFwithGAM
#'
#' predict(RFwithGAM, bbbDescr[1:10, ])
#'
#' ## classification example with parallel processing
#'
#' ## library(doMC)
#'
#' ## Note: if the underlying model also uses foreach, the
#' ## number of cores specified above will double (along with
#' ## the memory requirements)
#' ## registerDoMC(cores = 2)
#'
#' data(mdrr)
#' mdrrDescr <- mdrrDescr[, -nearZeroVar(mdrrDescr)]
#' mdrrDescr <- mdrrDescr[, -findCorrelation(cor(mdrrDescr), .8)]
#'
#' set.seed(1)
#' filteredNB <- sbf(
#'   mdrrDescr,
#'   mdrrClass,
#'   sbfControl = sbfControl(
#'     functions = nbSBF,
#'     verbose = FALSE,
#'     method = "repeatedcv",
#'     repeats = 5,
#'     saveDetails = TRUE
#'   )
#' )
#' confusionMatrix(filteredNB)
#'
#' @export sbf
sbf <- function(x, ...) UseMethod("sbf")

#' @rdname sbf
#' @export
"sbf.default" <-
  function(x, y, sbfControl = sbfControl(), ...) {
    startTime <- proc.time()
    funcCall <- match.call(expand.dots = TRUE)

    numFeat <- ncol(x)
    classLevels <- levels(y)

    if (sbfControl$method == "oob") {
      stop("out-of-bag resampling cannot be used with this function")
    }

    if (is.null(sbfControl$index)) {
      sbfControl$index <- switch(
        tolower(sbfControl$method),
        cv = createFolds(y, sbfControl$number, returnTrain = TRUE),
        repeatedcv = createMultiFolds(y, sbfControl$number, sbfControl$repeats),
        loocv = createFolds(y, length(y), returnTrain = TRUE),
        boot = ,
        boot632 = createResample(y, sbfControl$number),
        test = createDataPartition(y, 1, sbfControl$p),
        lgocv = createDataPartition(y, sbfControl$number, sbfControl$p)
      )
    }

    if (is.null(names(sbfControl$index))) {
      names(sbfControl$index) <- prettySeq(sbfControl$index)
    }
    if (is.null(sbfControl$indexOut)) {
      sbfControl$indexOut <- lapply(
        sbfControl$index,
        function(training, allSamples) allSamples[-unique(training)],
        allSamples = seq(along.with = y)
      )
      names(sbfControl$indexOut) <- prettySeq(sbfControl$indexOut)
    }
    ## check summary function and metric
    testOutput <- data.frame(
      pred = sample(y, min(10, length(y))),
      obs = sample(y, min(10, length(y)))
    )

    if (is.factor(y)) {
      for (i in seq(along.with = classLevels)) {
        testOutput[, classLevels[i]] <- runif(nrow(testOutput))
      }
    }

    test <- sbfControl$functions$summary(testOutput, lev = classLevels)
    perfNames <- names(test)

    ## Set or check the seeds when needed
    if (is.null(sbfControl$seeds)) {
      sbfControl$seeds <- sample.int(
        n = 1000000,
        size = length(sbfControl$index) + 1
      )
    } else {
      if (!(length(sbfControl$seeds) == 1 && is.na(sbfControl$seeds))) {
        if (length(sbfControl$seeds) != length(sbfControl$index) + 1) {
          stop(paste(
            "Bad seeds: the seed object should be an integer vector of length",
            length(sbfControl$index) + 1
          ))
        }
      }
    }

    #########################################################################

    if (sbfControl$method == "LOOCV") {
      tmp <- looSbfWorkflow(
        x = x,
        y = y,
        ppOpts = preProcess,
        ctrl = sbfControl,
        lev = classLevels,
        ...
      )
      resamples <- do.call(
        "rbind",
        tmp$everything[names(tmp$everything) == "pred"]
      )
      rownames(resamples) <- seq_len(nrow(resamples))
      selectedVars <- tmp$everything[names(tmp$everything) == "variables"]
      performance <- tmp$performance
    } else {
      tmp <- nominalSbfWorkflow(
        x = x,
        y = y,
        ppOpts = preProcess,
        ctrl = sbfControl,
        lev = classLevels,
        ...
      )
      resamples <- do.call(
        "rbind",
        tmp$everything[names(tmp$everything) == "resamples"]
      )
      rownames(resamples) <- seq_len(nrow(resamples))
      selectedVars <- tmp$everything[names(tmp$everything) == "selectedVars"]
      performance <- tmp$performance
    }

    #########################################################################

    varList <- unique(unlist(selectedVars))
    if (sbfControl$multivariate) {
      scores <- sbfControl$functions$score(x, y)
      if (length(scores) != ncol(x)) {
        stop(paste(
          "when control$multivariate == TRUE, 'scores'",
          "should return a vector with",
          ncol(x),
          "numeric values"
        ))
      }
    } else {
      scores <- apply(x, 2, sbfControl$functions$score, y = y)
    }
    retained <- sbfControl$functions$filter(scores, x, y)

    finalTime <- system.time(
      fit <-
        sbfControl$functions$fit(
          x[, retained, drop = FALSE],
          y,
          ...
        )
    )

    performance <- data.frame(t(performance))
    performance <- performance[,
      !grepl("\\.cell|Resample", colnames(performance))
    ]

    if (is.factor(y) && any(names(resamples) == ".cell1")) {
      keepers <- c("Resample", grep("\\.cell", names(resamples), value = TRUE))
      resampledCM <- resamples[, keepers]
      resamples <- resamples[, -grep("\\.cell", names(resamples))]
    } else {
      resampledCM <- NULL
    }

    resamples <- switch(
      sbfControl$returnResamp,
      none = NULL,
      all = ,
      final = resamples
    )

    endTime <- proc.time()
    times <- list(everything = endTime - startTime, final = finalTime)

    #########################################################################
    ## Now, based on probability or static ranking, figure out the best vars
    ## and the best subset size and fit final model

    out <- structure(
      list(
        pred = if (sbfControl$saveDetails) tmp else NULL,
        variables = selectedVars,
        results = performance,
        fit = fit,
        optVariables = names(retained)[retained],
        call = funcCall,
        control = sbfControl,
        resample = resamples,
        metrics = perfNames,
        times = times,
        resampledCM = resampledCM,
        obsLevels = classLevels,
        dots = list(...)
      ),
      class = "sbf"
    )
    if (sbfControl$timingSamps > 0) {
      out$times$prediction <-
        system.time(
          predict(
            out,
            x[1:min(nrow(x), sbfControl$timingSamps), , drop = FALSE]
          )
        )
    } else {
      out$times$prediction <- rep(NA, 3)
    }
    out
  }

#' @rdname sbf
#' @export
sbf.formula <- function(form, data, ..., subset, na.action, contrasts = NULL) {
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval.parent(m$data))) {
    m$data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  m$... <- m$contrasts <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  xint <- match("(Intercept)", colnames(x), nomatch = 0)
  if (xint > 0) {
    x <- x[, -xint, drop = FALSE]
  }
  y <- model.response(m)
  res <- sbf(as.data.frame(x, stringsAsFactors = TRUE), y, ...)
  res$terms <- Terms
  res$coefnames <- colnames(x)
  res$call <- match.call()
  res$na.action <- attr(m, "na.action")
  res$contrasts <- cons
  res$xlevels <- .getXlevels(Terms, m)
  class(res) <- c("sbf", "sbf.formula")
  res
}


######################################################################

#' @rdname sbf
#' @export
"sbf.recipe" <-
  function(x, data, sbfControl = sbfControl(), ...) {
    startTime <- proc.time()
    funcCall <- match.call(expand.dots = TRUE)

    orig_rec <- x
    trained_rec <- prep(
      x,
      training = data,
      fresh = TRUE,
      retain = TRUE,
      verbose = FALSE,
      stringsAsFactors = TRUE
    )
    x <- juice(trained_rec, all_predictors(), composition = "data.frame")
    y <- juice(trained_rec, all_outcomes(), composition = "data.frame")
    if (ncol(y) > 1) {
      stop("`safs` doesn't support multivariate outcomes", call. = FALSE)
    }
    y <- y[[1]]
    is_weight <- summary(trained_rec)$role == "case weight"
    if (any(is_weight)) {
      stop("`safs` does not allow for weights.", call. = FALSE)
    }

    is_perf <- summary(trained_rec)$role == "performance var"
    if (any(is_perf)) {
      perf_data <- juice(trained_rec, has_role("performance var"))
    } else {
      perf_data <- NULL
    }

    numFeat <- ncol(x)
    classLevels <- levels(y)

    if (sbfControl$method == "oob") {
      stop("out-of-bag resampling cannot be used with this function")
    }

    if (is.null(sbfControl$index)) {
      sbfControl$index <- switch(
        tolower(sbfControl$method),
        cv = createFolds(y, sbfControl$number, returnTrain = TRUE),
        repeatedcv = createMultiFolds(y, sbfControl$number, sbfControl$repeats),
        loocv = createFolds(y, length(y), returnTrain = TRUE),
        boot = ,
        boot632 = createResample(y, sbfControl$number),
        test = createDataPartition(y, 1, sbfControl$p),
        lgocv = createDataPartition(y, sbfControl$number, sbfControl$p)
      )
    }

    if (is.null(names(sbfControl$index))) {
      names(sbfControl$index) <- prettySeq(sbfControl$index)
    }
    if (is.null(sbfControl$indexOut)) {
      sbfControl$indexOut <- lapply(
        sbfControl$index,
        function(training, allSamples) allSamples[-unique(training)],
        allSamples = seq(along.with = y)
      )
      names(sbfControl$indexOut) <- prettySeq(sbfControl$indexOut)
    }
    ## check summary function and metric
    testOutput <- data.frame(
      pred = sample(y, min(10, length(y))),
      obs = sample(y, min(10, length(y)))
    )

    if (is.factor(y)) {
      for (i in seq(along.with = classLevels)) {
        testOutput[, classLevels[i]] <- runif(nrow(testOutput))
      }
    }
    if (!is.null(perf_data)) {
      testOutput <- cbind(
        testOutput,
        perf_data[
          sample(seq_len(nrow(perf_data)), nrow(testOutput)),
          ,
          drop = FALSE
        ]
      )
    }

    test <- sbfControl$functions$summary(testOutput, lev = classLevels)
    perfNames <- names(test)

    ## Set or check the seeds when needed
    if (is.null(sbfControl$seeds)) {
      sbfControl$seeds <- sample.int(
        n = 1000000,
        size = length(sbfControl$index) + 1
      )
    } else {
      if (!(length(sbfControl$seeds) == 1 && is.na(sbfControl$seeds))) {
        if (length(sbfControl$seeds) != length(sbfControl$index) + 1) {
          stop(paste(
            "Bad seeds: the seed object should be an integer vector of length",
            length(sbfControl$index) + 1
          ))
        }
      }
    }

    #########################################################################

    if (sbfControl$method == "LOOCV") {
      tmp <- sbf_loo_rec(
        rec = orig_rec,
        data = data,
        ctrl = sbfControl,
        lev = classLevels,
        ...
      )
      resamples <- do.call(
        "rbind",
        tmp$everything[names(tmp$everything) == "pred"]
      )
      rownames(resamples) <- seq_len(nrow(resamples))
      selectedVars <- tmp$everything[names(tmp$everything) == "variables"]
      performance <- tmp$performance
    } else {
      tmp <- sbf_rec(
        rec = orig_rec,
        data = data,
        ctrl = sbfControl,
        lev = classLevels,
        ...
      )
      resamples <- do.call(
        "rbind",
        tmp$everything[names(tmp$everything) == "resamples"]
      )
      rownames(resamples) <- seq_len(nrow(resamples))
      selectedVars <- tmp$everything[names(tmp$everything) == "selectedVars"]
      performance <- tmp$performance
    }

    #########################################################################

    varList <- unique(unlist(selectedVars))
    if (sbfControl$multivariate) {
      scores <- sbfControl$functions$score(x, y)
      if (length(scores) != ncol(x)) {
        stop(paste(
          "when control$multivariate == TRUE, 'scores'",
          "should return a vector with",
          ncol(x),
          "numeric values"
        ))
      }
    } else {
      scores <- apply(x, 2, sbfControl$functions$score, y = y)
    }
    retained <- sbfControl$functions$filter(scores, x, y)

    finalTime <- system.time(
      fit <-
        sbfControl$functions$fit(
          x = x[, retained, drop = FALSE],
          y = y,
          ...
        )
    )

    performance <- data.frame(t(performance))
    performance <- performance[,
      !grepl("\\.cell|Resample", colnames(performance))
    ]

    if (is.factor(y) && any(names(resamples) == ".cell1")) {
      keepers <- c("Resample", grep("\\.cell", names(resamples), value = TRUE))
      resampledCM <- resamples[, keepers]
      resamples <- resamples[, -grep("\\.cell", names(resamples))]
    } else {
      resampledCM <- NULL
    }

    resamples <- switch(
      sbfControl$returnResamp,
      none = NULL,
      all = ,
      final = resamples
    )

    endTime <- proc.time()
    times <- list(everything = endTime - startTime, final = finalTime)

    #########################################################################
    ## Now, based on probability or static ranking, figure out the best vars
    ## and the best subset size and fit final model

    out <- structure(
      list(
        pred = if (sbfControl$saveDetails) tmp else NULL,
        variables = selectedVars,
        results = performance,
        fit = fit,
        optVariables = names(retained)[retained],
        call = funcCall,
        control = sbfControl,
        resample = resamples,
        metrics = perfNames,
        times = times,
        resampledCM = resampledCM,
        obsLevels = classLevels,
        dots = list(...),
        recipe = trained_rec
      ),
      class = "sbf"
    )
    if (sbfControl$timingSamps > 0) {
      out$times$prediction <-
        system.time(
          predict(
            out,
            x[1:min(nrow(x), sbfControl$timingSamps), , drop = FALSE]
          )
        )
    } else {
      out$times$prediction <- rep(NA, 3)
    }
    out
  }


sbf_rec <- function(rec, data, ctrl, lev, ...) {
  loadNamespace("caret")

  resampleIndex <- ctrl$index
  if (ctrl$method %in% c("boot632")) {
    resampleIndex <- c(list("AllData" = rep(0, nrow(x))), resampleIndex)
    ctrl$indexOut <- c(list("AllData" = rep(0, nrow(x))), ctrl$indexOut)
  }

  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  result <- foreach(
    iter = seq(along.with = resampleIndex),
    .combine = "c",
    .verbose = FALSE,
    .errorhandling = "stop",
    .packages = c("caret", "recipes")
  ) %op%
    {
      if (!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) {
        set.seed(ctrl$seeds[iter])
      }

      loadNamespace("caret")
      requireNamespaceQuietStop("methods")

      if (names(resampleIndex)[iter] != "AllData") {
        modelIndex <- resampleIndex[[iter]]
        holdoutIndex <- ctrl$indexOut[[iter]]
      } else {
        modelIndex <- seq_len(nrow(data))
        holdoutIndex <- modelIndex
      }

      # reprocess recipe
      resampled_rec <- prep(
        rec,
        training = data[modelIndex, ],
        fresh = TRUE,
        retain = TRUE,
        verbose = FALSE,
        stringsAsFactors = TRUE
      )
      x_tr <- juice(resampled_rec, all_predictors(), composition = "data.frame")
      y_tr <- juice(resampled_rec, all_outcomes(), composition = "data.frame")
      y_tr <- y_tr[[1]]
      x_te <- bake(
        resampled_rec,
        new_data = data[holdoutIndex, ],
        all_predictors(),
        composition = "data.frame"
      )
      y_te <- bake(
        resampled_rec,
        new_data = data[holdoutIndex, ],
        all_outcomes(),
        composition = "data.frame"
      )
      y_te <- y_te[[1]]
      is_perf <- summary(resampled_rec)$role == "performance var"
      if (any(is_perf)) {
        perf_tr <- juice(resampled_rec, has_role("performance var"))
        perf_te <- bake(
          resampled_rec,
          new_data = data[holdoutIndex, ],
          has_role("performance var")
        )
      } else {
        perf_tr <- NULL
        perf_te <- NULL
      }

      sbfResults <- sbfIter(
        x = x_tr,
        y = y_tr,
        testX = x_te,
        testY = y_te,
        testPerf = perf_te,
        sbfControl = ctrl,
        ...
      )
      if (ctrl$saveDetails) {
        tmpPred <- sbfResults$pred
        tmpPred$Resample <- names(resampleIndex)[iter]
        tmpPred$rowIndex <- (1:nrow(data))[unique(holdoutIndex)]
      } else {
        tmpPred <- NULL
      }
      resamples <- ctrl$functions$summary(sbfResults$pred, lev = lev)
      if (is.factor(y_tr) && length(lev) <= 50) {
        resamples <- c(
          resamples,
          flatTable(sbfResults$pred$pred, sbfResults$pred$obs)
        )
      }
      resamples <- data.frame(t(resamples))
      resamples$Resample <- names(resampleIndex)[iter]

      list(
        resamples = resamples,
        selectedVars = sbfResults$variables,
        pred = tmpPred
      )
    }

  resamples <- rbind.fill(result[names(result) == "resamples"])
  if (ctrl$saveDetails) {
    pred <- rbind.fill(result[names(result) == "pred"])
  } else {
    pred <- NULL
  }
  performance <- MeanSD(resamples[,
    !grepl("Resample", colnames(resamples)),
    drop = FALSE
  ])

  if (ctrl$method %in% c("boot632")) {
    modelIndex <- seq_len(nrow(x))
    holdoutIndex <- modelIndex
    appResults <- sbfIter(
      x = x_tr,
      y = y_tr,
      testX = x_te,
      testY = y_te,
      testPerf = perf_te,
      ctrl,
      ...
    )
    apparent <- ctrl$functions$summary(appResults$pred, lev = lev)
    perfNames <- names(apparent)
    perfNames <- perfNames[perfNames != "Resample"]

    const <- 1 - exp(-1)

    for (p in seq(along.with = perfNames)) {
      performance[perfNames[p]] <-
        (const * performance[perfNames[p]]) +
        ((1 - const) * apparent[perfNames[p]])
    }
  }

  list(
    performance = performance,
    everything = result,
    predictions = if (ctrl$saveDetails) {
      pred
    } else {
      NULL
    }
  )
}


sbf_loo_rec <- function(rec, data, ctrl, lev, ...) {
  loadNamespace("caret")

  resampleIndex <- ctrl$index

  vars <- vector(mode = "list", length = nrow(data))

  `%op%` <- getOper(ctrl$allowParallel && getDoParWorkers() > 1)
  result <- foreach(
    iter = seq(along.with = resampleIndex),
    .combine = "c",
    .verbose = FALSE,
    .errorhandling = "stop",
    .packages = c("caret", "recipes")
  ) %op%
    {
      if (!(length(ctrl$seeds) == 1 && is.na(ctrl$seeds))) {
        set.seed(ctrl$seeds[iter])
      }

      loadNamespace("caret")
      requireNamespaceQuietStop("methods")
      modelIndex <- resampleIndex[[iter]]
      holdoutIndex <- -unique(resampleIndex[[iter]])

      # reprocess recipe
      resampled_rec <- prep(
        rec,
        training = data[modelIndex, ],
        fresh = TRUE,
        retain = TRUE,
        verbose = FALSE,
        stringsAsFactors = TRUE
      )
      x_tr <- juice(resampled_rec, all_predictors(), composition = "data.frame")
      y_tr <- juice(resampled_rec, all_outcomes(), composition = "data.frame")
      y_tr <- y_tr[[1]]
      x_te <- bake(
        resampled_rec,
        new_data = data[holdoutIndex, ],
        all_predictors(),
        composition = "data.frame"
      )
      y_te <- bake(
        resampled_rec,
        new_data = data[holdoutIndex, ],
        all_outcomes(),
        composition = "data.frame"
      )
      y_te <- y_te[[1]]
      is_perf <- summary(resampled_rec)$role == "performance var"
      if (any(is_perf)) {
        perf_tr <- juice(resampled_rec, has_role("performance var"))
        perf_te <- bake(
          resampled_rec,
          new_data = data[holdoutIndex, ],
          has_role("performance var")
        )
      } else {
        perf_tr <- NULL
        perf_te <- NULL
      }

      sbfResults <- sbfIter(
        x = x_tr,
        y = y_tr,
        testX = x_te,
        testY = y_te,
        testPerf = perf_te,
        sbfControl = ctrl,
        ...
      )

      sbfResults
    }
  resamples <- do.call("rbind", result[names(result) == "pred"])
  performance <- ctrl$functions$summary(resamples, lev = lev)

  list(
    performance = performance,
    everything = result,
    predictions = if (ctrl$saveDetails) {
      resamples
    } else {
      NULL
    }
  )
}

######################################################################

#' @export
print.sbf <- function(
  x,
  top = 5,
  digits = max(3, getOption("digits") - 3),
  ...
) {
  cat("\nSelection By Filter\n\n")

  resampleN <- unlist(lapply(x$control$index, length))
  numResamp <- length(resampleN)

  resampText <- resampName(x)
  cat("Outer resampling method:", resampText, "\n")

  cat("\nResampling performance:\n\n")
  print(format(x$results, digits = digits), row.names = FALSE)
  cat("\n")

  if (length(x$optVariables) > 0) {
    cat(
      "Using the training set, ",
      length(x$optVariables),
      ifelse(
        length(x$optVariables) > 1,
        " variables were selected:\n   ",
        " variable was selected:\n   "
      ),
      paste(
        x$optVariables[1:min(top, length(x$optVariables))],
        collapse = ", "
      ),
      ifelse(length(x$optVariables) > top, "..", ""),
      ".\n\n",
      sep = ""
    )
  } else {
    cat("No variables were selected from the training set.\n\n")
  }

  vars <- sort(table(unlist(x$variables)), decreasing = TRUE)

  top <- min(top, length(vars))

  smallVars <- vars[1:top]
  smallVars <- round(smallVars / length(x$control$index) * 100, 1)

  varText <- paste(names(smallVars), " (", smallVars, "%)", sep = "")
  varText <- paste(varText, collapse = ", ")

  if (!all(is.na(smallVars))) {
    cat(
      "During resampling, the top ",
      top,
      " selected variables (out of a possible ",
      length(vars),
      "):\n   ",
      varText,
      "\n\n",
      sep = ""
    )
    cat(
      "On average, ",
      round(mean(unlist(lapply(x$variables, length))), 1),
      " variables were selected (min = ",
      round(min(unlist(lapply(x$variables, length))), 1),
      ", max = ",
      round(max(unlist(lapply(x$variables, length))), 1),
      ")\n",
      sep = ""
    )
  } else {
    cat("During resampling, no variables were selected.\n\n")
  }

  invisible(x)
}

######################################################################
######################################################################
#' @rdname sbf
#' @export
predict.sbf <- function(object, newdata = NULL, ...) {
  if (!is.null(newdata)) {
    if (inherits(object, "sbf.formula")) {
      newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
      rn <- row.names(newdata)
      Terms <- delete.response(object$terms)
      m <- model.frame(
        Terms,
        newdata,
        na.action = na.omit,
        xlev = object$xlevels
      )
      if (!is.null(cl <- attr(Terms, "dataClasses"))) {
        .checkMFClasses(cl, m)
      }
      keep <- match(row.names(m), rn)
      newdata <- model.matrix(Terms, m, contrasts = object$contrasts)
      xint <- match("(Intercept)", colnames(newdata), nomatch = 0)
      if (xint > 0) {
        newdata <- newdata[, -xint, drop = FALSE]
      }
    } else {
      if (any(names(object) == "recipe") && !is.null(object$recipe)) {
        newdata <-
          bake(
            object$recipe,
            newdata,
            all_predictors(),
            composition = "data.frame"
          )
      }
    }
    if (!all(object$optVariables %in% colnames(newdata))) {
      stop("required columns in newdata are missing", call. = FALSE)
    }
    newdata <- newdata[, object$optVariables, drop = FALSE]
    out <- object$control$functions$pred(object$fit, newdata)
  } else {
    out <- object$control$functions$pred(object$fit)
  }
  out
}

######################################################################
######################################################################

#' Control Object for Selection By Filtering (SBF)
#'
#' Controls the execution of models with simple filters for feature selection
#'
#' More details on this function can be found at
#' <http://topepo.github.io/caret/feature-selection-using-univariate-filters.html>.
#'
#' Simple filter-based feature selection requires function to be specified for
#' some operations.
#'
#' The `fit` function builds the model based on the current data set. The
#' arguments for the function must be:
#' - `x` the current training set of predictor data with the appropriate
#'   subset of variables (i.e. after filtering)
#' - `y` the current outcome data (either a numeric or factor vector)
#' - `...` optional arguments to pass to the fit function in the call to
#'   `sbf`
#'
#' The function should return a model object that can be used to generate
#' predictions.
#'
#' The `pred` function returns a vector of predictions (numeric or factors)
#' from the current model. The arguments are:
#' - `object` the model generated by the `fit` function
#' - `x` the current set of predictor set for the held-back samples
#'
#' The `score` function is used to return scores with names for each
#' predictor (such as a p-value). Inputs are:
#' - `x` the predictors for the training samples. If
#'   `sbfControl()$multivariate` is `TRUE`, this will be the full
#'   predictor matrix. Otherwise it is a vector for a specific predictor.
#' - `y` the current training outcomes
#'
#' When `sbfControl()$multivariate` is `TRUE`, the `score` function should
#' return a named vector where `length(scores) == ncol(x)`. Otherwise, the
#' function's output should be a single value. Univariate examples are given
#' by [anovaScores()] for classification and [gamScores()] for regression
#' and the example below.
#'
#' The `filter` function is used to return a logical vector with names for
#' each predictor (`TRUE` indicates that the prediction should be retained).
#' Inputs are:
#' - `score` the output of the `score` function
#' - `x` the predictors for the training samples
#' - `y` the current training outcomes
#'
#' The function should return a named logical vector.
#'
#' Examples of these functions are included in the package: [caretSBF()],
#' [lmSBF()], [rfSBF()], [treebagSBF()], [ldaSBF()] and [nbSBF()].
#'
#' The web page <http://topepo.github.io/caret/> has more details and examples
#' related to this function.
#'
#' @param functions a list of functions for model fitting, prediction and
#'   variable filtering (see Details below)
#' @param method The external resampling method: `boot`, `cv`, `LOOCV` or
#'   `LGOCV` (for repeated training/test splits
#' @param number Either the number of folds or number of resampling iterations
#' @param repeats For repeated k-fold cross-validation only: the number of
#'   complete sets of folds to compute
#' @param saveDetails a logical to save the predictions and variable
#'   importances from the selection process
#' @param verbose a logical to print a log for each external resampling
#'   iteration
#' @param returnResamp A character string indicating how much of the resampled
#'   summary metrics should be saved. Values can be ``final'' or ``none''
#' @param p For leave-group out cross-validation: the training percentage
#' @param index a list with elements for each external resampling iteration.
#'   Each list element is the sample rows used for training at that iteration.
#' @param indexOut a list (the same length as `index`) that dictates which
#'   sample are held-out for each resample. If `NULL`, then the unique set of
#'   samples not contained in `index` is used.
#' @param timingSamps the number of training set samples that will be used to
#'   measure the time for predicting samples (zero indicates that the
#'   prediction time should not be estimated).
#' @param seeds an optional set of integers that will be used to set the seed
#'   at each resampling iteration. This is useful when the models are run in
#'   parallel. A value of `NA` will stop the seed from being set within the
#'   worker processes while a value of `NULL` will set the seeds using a random
#'   set of integers. Alternatively, a vector of integers can be used. The
#'   vector should have `B+1` elements where `B` is the number of resamples.
#'   See the Examples section below.
#' @param allowParallel if a parallel backend is loaded and available, should
#'   the function use it?
#' @param multivariate a logical; should all the columns of `x` be exposed to
#'   the `score` function at once?
#' @return a list that echos the specified arguments
#' @author Max Kuhn
#' @seealso [sbf()], [caretSBF()], [lmSBF()], [rfSBF()], [treebagSBF()],
#'   [ldaSBF()] and [nbSBF()]
#' @keywords utilities
#' @examplesIf !caret:::is_cran_check()
#'
#' data(BloodBrain)
#'
#' ## Use a GAM is the filter, then fit a random forest model
#' set.seed(1)
#' RFwithGAM <- sbf(
#'   bbbDescr,
#'   logBBB,
#'   sbfControl = sbfControl(
#'     functions = rfSBF,
#'     verbose = FALSE,
#'     seeds = sample.int(100000, 11),
#'     method = "cv"
#'   )
#' )
#' RFwithGAM
#'
#' ## A simple example for multivariate scoring
#' rfSBF2 <- rfSBF
#' rfSBF2$score <- function(x, y) apply(x, 2, rfSBF$score, y = y)
#'
#' set.seed(1)
#' RFwithGAM2 <- sbf(
#'   bbbDescr,
#'   logBBB,
#'   sbfControl = sbfControl(
#'     functions = rfSBF2,
#'     verbose = FALSE,
#'     seeds = sample.int(100000, 11),
#'     method = "cv",
#'     multivariate = TRUE
#'   )
#' )
#' RFwithGAM2
#'
#' @export sbfControl
sbfControl <- function(
  functions = NULL,
  method = "boot",
  saveDetails = FALSE,
  number = ifelse(method %in% c("cv", "repeatedcv"), 10, 25),
  repeats = ifelse(method %in% c("cv", "repeatedcv"), 1, number),
  verbose = FALSE,
  returnResamp = "final",
  p = 0.75,
  index = NULL,
  indexOut = NULL,
  timingSamps = 0,
  seeds = NA,
  allowParallel = TRUE,
  multivariate = FALSE
) {
  list(
    functions = if (is.null(functions)) caretSBF else functions,
    method = method,
    saveDetails = saveDetails,
    number = number,
    repeats = repeats,
    returnResamp = returnResamp,
    verbose = verbose,
    p = p,
    index = index,
    indexOut = indexOut,
    timingSamps = timingSamps,
    seeds = seeds,
    allowParallel = allowParallel,
    multivariate = multivariate
  )
}

######################################################################
######################################################################
## some built-in functions for certain models

#' Selection By Filtering (SBF) Helper Functions
#'
#' Ancillary functions for univariate feature selection
#'
#' More details on these functions can be found at
#' <http://topepo.github.io/caret/feature-selection-using-univariate-filters.html>.
#'
#' This page documents the functions that are used in selection by filtering
#' (SBF). The functions described here are passed to the algorithm via the
#' `functions` argument of [sbfControl()].
#'
#' See [sbfControl()] for details on how these functions should be defined.
#'
#' `anovaScores` and `gamScores` are two examples of univariate filtering
#' functions. `anovaScores` fits a simple linear model between a single feature
#' and the outcome, then the p-value for the whole model F-test is returned.
#' `gamScores` fits a generalized additive model between a single predictor and
#' the outcome using a smoothing spline basis function. A p-value is generated
#' using the whole model test from [gam::summary.Gam()] and is returned.
#'
#' If a particular model fails for `lm` or `gam`, a p-value of 1 is returned.
#'
#' @aliases caretSBF lmSBF rfSBF treebagSBF ldaSBF nbSBF gamScores anovaScores
#' @param x a matrix or data frame of numeric predictors
#' @param y a numeric or factor vector of outcomes
#' @author Max Kuhn
#' @seealso [sbfControl()], [sbf()], [gam::summary.Gam()]
#' @keywords models
#' @export caretSBF
caretSBF <- list(
  summary = defaultSummary,
  fit = function(x, y, ...) {
    if (ncol(x) > 0) {
      train(x, y, ...)
    } else {
      nullModel(y = y)
    }
  },
  pred = function(object, x) {
    if (!inherits(object, "nullModel")) {
      tmp <- predict(object, x)
      if (
        object$modelType == "Classification" &&
          !is.null(object$modelInfo$prob)
      ) {
        out <- cbind(
          data.frame(pred = tmp),
          as.data.frame(
            predict(object, x, type = "prob"),
            stringsAsFactors = TRUE
          )
        )
      } else {
        out <- tmp
      }
    } else {
      tmp <- predict(object, x)
      if (!is.null(object$levels)) {
        out <- cbind(
          data.frame(pred = tmp),
          as.data.frame(
            predict(object, x, type = "prob"),
            stringsAsFactors = TRUE
          )
        )
      } else {
        out <- tmp
      }
    }
    out
  },
  score = function(x, y) {
    ## should return a named logical vector
    if (is.factor(y)) {
      anovaScores(x, y)
    } else {
      gamScores(x, y)
    }
  },
  filter = function(score, x, y) score <= 0.05
)

#' @export
rfSBF <- list(
  summary = defaultSummary,
  fit = function(x, y, ...) {
    if (ncol(x) > 0) {
      loadNamespace("randomForest")
      randomForest::randomForest(x, y, ...)
    } else {
      nullModel(y = y)
    }
  },
  pred = function(object, x) {
    if (inherits(object, "nullModel")) {
      tmp <- predict(object, x)
      if (!is.null(object$levels)) {
        out <- cbind(
          data.frame(pred = tmp),
          as.data.frame(
            predict(object, x, type = "prob"),
            stringsAsFactors = TRUE
          )
        )
      } else {
        out <- tmp
      }
    } else {
      tmp <- predict(object, x)
      if (is.factor(object$y)) {
        out <- cbind(
          data.frame(pred = tmp),
          as.data.frame(
            predict(object, x, type = "prob"),
            stringsAsFactors = TRUE
          )
        )
      } else {
        out <- tmp
      }
    }

    out
  },
  score = function(x, y) {
    ## should return a named logical vector
    if (is.factor(y)) {
      anovaScores(x, y)
    } else {
      gamScores(x, y)
    }
  },
  filter = function(score, x, y) score <= 0.05
)

#' @export
lmSBF <- list(
  summary = defaultSummary,
  fit = function(x, y, ...) {
    if (ncol(x) > 0) {
      tmp <- as.data.frame(x, stringsAsFactors = TRUE)
      tmp$y <- y
      lm(y ~ ., data = tmp)
    } else {
      nullModel(y = y)
    }
  },
  pred = function(object, x) {
    predict(object, x)
  },
  score = function(x, y) {
    anovaScores(y, x)
  },
  filter = function(score, x, y) score <= 0.05
)

#' @export
ldaSBF <- list(
  summary = defaultSummary,
  fit = function(x, y, ...) {
    if (ncol(x) > 0) {
      loadNamespace("MASS")
      MASS::lda(x, y, ...)
    } else {
      nullModel(y = y)
    }
  },
  pred = function(object, x) {
    if (inherits(object, "nullModel")) {
      tmp <- predict(object, x)
      out <- cbind(
        data.frame(pred = tmp),
        as.data.frame(
          predict(object, x, type = "prob")
        )
      )
    } else {
      tmp <- predict(object, x)
      out <- cbind(
        data.frame(pred = tmp$class),
        as.data.frame(tmp$posterior, stringsAsFactors = FALSE)
      )
    }
    out
  },
  score = function(x, y) {
    ## should return a named logical vector
    anovaScores(x, y)
  },
  filter = function(score, x, y) score <= 0.05
)

#' @export
nbSBF <- list(
  summary = defaultSummary,
  fit = function(x, y, ...) {
    if (ncol(x) > 0) {
      loadNamespace("klaR")
      klaR::NaiveBayes(x, y, usekernel = TRUE, fL = 2, ...)
    } else {
      nullModel(y = y)
    }
  },
  pred = function(object, x) {
    if (inherits(object, "nullModel")) {
      tmp <- predict(object, x)
      out <- cbind(
        data.frame(pred = tmp),
        as.data.frame(
          predict(object, x, type = "prob")
        )
      )
    } else {
      tmp <- predict(object, x)
      out <- cbind(
        data.frame(pred = tmp$class),
        as.data.frame(tmp$posterior, stringsAsFactors = FALSE)
      )
    }
    out
  },

  pred = function(object, x) {
    predict(object, x)$class
  },
  score = function(x, y) {
    ## should return a named logical vector
    anovaScores(x, y)
  },
  filter = function(score, x, y) score <= 0.05
)

#' @export
treebagSBF <- list(
  summary = defaultSummary,
  fit = function(x, y, ...) {
    if (ncol(x) > 0) {
      loadNamespace("ipred")
      ipred::ipredbagg(y, x, ...)
    } else {
      nullModel(y = y)
    }
  },

  pred = function(object, x) {
    if (inherits(object, "nullModel")) {
      tmp <- predict(object, x)
      if (!is.null(object$levels)) {
        out <- cbind(
          data.frame(pred = tmp),
          as.data.frame(
            predict(object, x, type = "prob"),
            stringsAsFactors = TRUE
          )
        )
      } else {
        out <- tmp
      }
    } else {
      tmp <- predict(object, x)
      if (is.factor(object$y)) {
        out <- cbind(
          data.frame(pred = tmp),
          as.data.frame(
            predict(object, x, type = "prob"),
            stringsAsFactors = TRUE
          )
        )
      } else {
        out <- tmp
      }
    }
    out
  },
  score = function(x, y) {
    ## should return a named logical vector
    anovaScores(x, y)
  },
  filter = function(score, x, y) score <= 0.05
)


#' @rdname caretSBF
#' @export
anovaScores <- function(x, y) {
  if (is.factor(x)) {
    stop("The predictors should be numeric")
  }
  pv <- try(anova(lm(x ~ y), test = "F")[1, "Pr(>F)"], silent = TRUE)
  if (any(class(pv) == "try-error") || is.na(pv) || is.nan(pv)) {
    pv <- 1
  }
  pv
}

#' @rdname caretSBF
#' @export
gamScores <- function(x, y) {
  if (is.factor(x)) {
    stop("The predictors should be numeric")
  }
  requireNamespaceQuietStop("gam")
  pv <- try(anova(gam::gam(y ~ s(x)), test = "F")[2, "Pr(F)"], silent = TRUE)
  if (any(class(pv) == "try-error")) {
    pv <- try(anova(lm(x ~ y), test = "F")[1, "Pr(>F)"], silent = TRUE)
  }
  if (any(class(pv) == "try-error") || is.na(pv) || is.nan(pv)) {
    pv <- 1
  }
  pv
}


######################################################################
######################################################################
## lattice functions

#' @export
densityplot.sbf <- function(x, data = NULL, metric = x$metric[1], ...) {
  if (!is.null(match.call()$data)) {
    warning("explicit 'data' specification ignored")
  }

  if (x$control$method %in% c("oob", "LOOCV")) {
    stop(
      "Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling"
    )
  }

  data <- as.data.frame(x$resample, stringsAsFactors = TRUE)
  form <- as.formula(paste("~", metric))
  densityplot(form, data = data, ...)
}

#' @export
histogram.sbf <- function(x, data = NULL, metric = x$metric[1], ...) {
  if (!is.null(match.call()$data)) {
    warning("explicit 'data' specification ignored")
  }

  if (x$control$method %in% c("oob", "LOOCV")) {
    stop(
      "Resampling plots cannot be done with leave-out-out CV or out-of-bag resampling"
    )
  }

  data <- as.data.frame(x$resample, stringsAsFactors = TRUE)

  form <- as.formula(paste("~", metric))
  histogram(form, data = data, ...)
}


######################################################################
######################################################################
## other functions
#' @export
predictors.sbf <- function(x, ...) x$optVariables

#' @export
varImp.sbf <- function(object, onlyFinal = TRUE, ...) {
  vars <- sort(table(unlist(object$variables)), decreasing = TRUE) /
    length(object$control$index)

  out <- as.data.frame(vars, stringsAsFactors = FALSE)
  names(out) <- "Overall"
  if (onlyFinal) {
    out <- subset(out, rownames(out) %in% object$optVariables)
  }
  out[order(-out$Overall), , drop = FALSE]
}

######################################################################
## what to do when no predictors are selected?

#' Fit a simple, non-informative model
#'
#' Fit a single mean or largest class model
#'
#' `nullModel` emulates other model building functions, but returns the
#' simplest model possible given a training set: a single mean for numeric
#' outcomes and the most prevalent class for factor outcomes. When class
#' probabilities are requested, the percentage of the training set samples with
#' the most prevalent class is returned.
#'
#' @aliases nullModel nullModel.default predict.nullModel
#' @param x An optional matrix or data frame of predictors. These values are
#'   not used in the model fit
#' @param y A numeric vector (for regression) or factor (for classification) of
#'   outcomes
#' @param \dots Optional arguments (not yet used)
#' @param object An object of class `nullModel`
#' @param newdata A matrix or data frame of predictors (only used to determine
#'   the number of predictions to return)
#' @param type Either "raw" (for regression), "class" or "prob" (for
#'   classification)
#' @return The output of `nullModel` is a list of class `nullModel` with
#' elements
#' * `call`: the function call
#' * `value`: the mean of `y` or the most prevalent class
#' * `levels`: when `y` is a factor, a vector of levels. `NULL` otherwise
#' * `pct`: when `y` is a factor, a data frame with a column for each class
#'          (`NULL` otherwise). The column for the most prevalent class has the
#'          proportion of the training samples with that class (the other
#'          columns are zero).
#' * `n`: the number of elements in `y`
#'
#' `predict.nullModel` returns a either a factor or numeric vector depending on
#' the class of `y`. All predictions are always the same.
#' @keywords models
#' @examples
#'
#' outcome <- factor(sample(
#'   letters[1:2],
#'   size = 100,
#'   prob = c(.1, .9),
#'   replace = TRUE
#' ))
#' useless <- nullModel(y = outcome)
#' useless
#' predict(useless, matrix(NA, nrow = 10))
#'
#' @export nullModel
nullModel <- function(x, ...) UseMethod("nullModel")

#' @rdname nullModel
#' @export
nullModel.default <- function(x = NULL, y, ...) {
  if (is.factor(y)) {
    lvls <- levels(y)
    tab <- table(y)
    value <- names(tab)[which.max(tab)]
    pct <- tab / sum(tab)
  } else {
    lvls <- NULL
    pct <- NULL
    value <- mean(y, na.rm = TRUE)
  }
  structure(
    list(
      call = match.call(),
      value = value,
      levels = lvls,
      pct = pct,
      n = length(y)
    ),
    class = "nullModel"
  )
}

#' @export
print.nullModel <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat(
    "Null",
    ifelse(is.null(x$levels), "Regression", "Classification"),
    "Model\n"
  )
  printCall(x$call)

  cat(
    "Predicted Value:",
    ifelse(is.null(x$levels), format(x$value, digitis = digits), x$value),
    "\n"
  )
}

#' @rdname nullModel
#' @export
predict.nullModel <- function(object, newdata = NULL, type = NULL, ...) {
  if (is.null(type)) {
    if (is.null(object$levels)) {
      type <- "raw"
    } else {
      type <- "class"
    }
  }

  if (is.null(newdata)) {
    n <- object$n
  } else {
    n <- nrow(newdata)
  }
  if (!is.null(object$levels)) {
    if (type == "prob") {
      out <- matrix(rep(object$pct, n), nrow = n, byrow = TRUE)
      colnames(out) <- object$levels
      out <- as.data.frame(out, stringsAsFactors = TRUE)
    } else {
      out <- factor(rep(object$value, n), levels = object$levels)
    }
  } else {
    if (type %in% c("prob", "class")) {
      stop("ony raw predicitons are applicable to regression models")
    }
    out <- rep(object$value, n)
  }
  out
}
