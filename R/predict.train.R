#' @export
predict.list <- function(object, ...) {
  out <- lapply(object, predict, ... = ...)
  if (!is.null(names(object))) {
    names(out) <- names(object)
  }
  out
}


#' Extract predictions and class probabilities from train objects
#'
#' These functions can be used for a single `train` object or to loop through a
#' number of `train` objects to calculate the training and test data
#' predictions and class probabilities.
#'
#' These functions are wrappers for the specific prediction functions in each
#' modeling package. In each case, the optimal tuning values given in the
#' `tuneValue` slot of the `finalModel` object are used to predict.
#'
#' To get simple predictions for a new data set, the `predict` function can be
#' used. Limits can be imposed on the range of predictions. See
#' [trainControl()] for more information.
#'
#' To get predictions for a series of models at once, a list of [train()]
#' objects can be passes to the `predict` function and a list of model
#' predictions will be returned.
#'
#' The two extraction functions can be used to get the predictions and observed
#' outcomes at once for the training, test and/or unknown samples at once in a
#' single data frame (instead of a list of just the predictions). These objects
#' can then be passes to [plotObsVsPred()] or [plotClassProbs()].
#'
#' @aliases predict.list predict.train extractPrediction extractProb
#' @param object For `predict.train`, an object of class [train()]. For
#'   `predict.list`, a list of objects of class [train()].
#' @param newdata an optional set of data to predict on. If `NULL`, then the
#'   original training data are used but, if the `train` model used a recipe,
#'   an error will occur.
#' @param type either "raw" or "prob", for the number/class predictions or
#'   class probabilities, respectively. Class probabilities are not available
#'   for all classification models
#' @param models a list of objects of the class `train`. The objects must have
#'   been generated with `fitBest = FALSE` and `returnData = TRUE`.
#' @param na.action the method for handling missing data
#' @param testX an optional set of data to predict
#' @param testY an optional outcome corresponding to the data given in `testX`
#' @param unkX another optional set of data to predict without known outcomes
#' @param unkOnly a logical to bypass training and test set predictions. This
#'   is useful if speed is needed for unknown samples.
#' @param verbose a logical for printing messages
#' @param \dots only used for `sort` and `modelCor` and captures arguments to
#'   pass to `sort` or `FUN`.
#' @return
#'
#' For `predict.train`, a vector of predictions if `type = "raw"` or a data
#' frame of class probabilities for `type = "prob"`. In the latter case, there
#' are columns for each class.
#'
#' For `predict.list`, a list results. Each element is produced by
#' `predict.train`.
#'
#' For `extractPrediction`, a data frame with columns:
#' * `obs`: the observed training and test data
#' * `pred`: predicted values
#' * `model`: the type of model used to predict
#' * `object`: the names of the objects within `models`. If `models` is an
#'             un-named list, the values of `object` will be "Object1",
#'             "Object2" and so on
#' * `dataType`: "Training", "Test" or "Unknown" depending on what was
#'               specified
#'
#' For `extractProb`, a data frame. There is a column for each class containing
#' the probabilities. The remaining columns are the same as above (although the
#' `pred` column is the predicted class)
#' @author Max Kuhn
#' @seealso [plotObsVsPred()], [plotClassProbs()], [trainControl()]
#' @references Kuhn (2008), ``Building Predictive Models in R Using the caret''
#'   (\doi{10.18637/jss.v028.i05})
#' @family train
#' @keywords manip
#' @examplesIf !caret:::is_cran_check()
#' 
#'    \dontrun{
#' 
#' knnFit <- train(
#'   Species ~ .,
#'   data = iris,
#'   method = "knn",
#'   trControl = trainControl(method = "cv")
#' )
#' 
#' rdaFit <- train(
#'   Species ~ .,
#'   data = iris,
#'   method = "rda",
#'   trControl = trainControl(method = "cv")
#' )
#' 
#' predict(knnFit)
#' predict(knnFit, type = "prob")
#' 
#' bothModels <- list(knn = knnFit, tree = rdaFit)
#' 
#' predict(bothModels)
#' 
#' extractPrediction(bothModels, testX = iris[1:10, -5])
#' extractProb(bothModels, testX = iris[1:10, -5])
#'   }
#' 
#' @export predict.train
#' @export
predict.train <- function(
  object,
  newdata = NULL,
  type = "raw",
  na.action = na.omit,
  ...
) {
  if (all(names(object) != "modelInfo")) {
    object <- update(object, param = NULL)
  }
  if (!is.null(object$modelInfo$library)) {
    for (i in object$modelInfo$library) {
      do.call("requireNamespaceQuietStop", list(package = i))
    }
  }
  if (!(type %in% c("raw", "prob"))) {
    stop("type must be either \"raw\" or \"prob\"")
  }
  if (type == "prob") {
    if (is.null(object$modelInfo$prob)) {
      stop("only classification models that produce probabilities are allowed")
    }
  }

  if (!is.null(newdata)) {
    if (inherits(object, "train.formula")) {
      newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
      rn <- row.names(newdata)
      Terms <- delete.response(object$terms)
      m <- model.frame(
        Terms,
        newdata,
        na.action = na.action,
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
    }
  } else if (object$control$method != "oob") {
    if (!is.null(object$trainingData)) {
      if (object$method == "pam") {
        newdata <- object$finalModel$xData
      } else {
        newdata <- object$trainingData
        newdata$.outcome <- NULL
        if (
          "train.formula" %in%
            class(object) &&
            any(unlist(lapply(newdata, is.factor)))
        ) {
          newdata <- model.matrix(~., data = newdata)[, -1]
          newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
        }
      }
    } else {
      stop("please specify data via newdata")
    }
  }

  if (
    "xNames" %in%
      names(object$finalModel) &&
      is.null(object$preProcess$method$pca) &&
      is.null(object$preProcess$method$ica)
  ) {
    newdata <- newdata[,
      colnames(newdata) %in% object$finalModel$xNames,
      drop = FALSE
    ]
  }

  if (type == "prob") {
    out <- probFunction(
      method = object$modelInfo,
      modelFit = object$finalModel,
      newdata = newdata,
      preProc = object$preProcess
    )
    obsLevels <- levels(object)
    out <- out[, obsLevels, drop = FALSE]
  } else {
    out <- predictionFunction(
      method = object$modelInfo,
      modelFit = object$finalModel,
      newdata = newdata,
      preProc = object$preProcess
    )
    if (object$modelType == "Regression") {
      out <- trimPredictions(
        pred = out,
        mod_type = object$modelType,
        bounds = object$control$predictionBounds,
        limits = object$yLimit
      )
    } else {
      if (!("levels" %in% names(object))) {
        object$levels <- levels(object)
      }
      out <- outcome_conversion(as.character(out), lv = object$levels)
    }
  }
  out
}
