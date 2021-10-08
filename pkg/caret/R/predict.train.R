#' @importFrom stats predict
#' @export
predict.list <- function(object, ...) {
  out <- lapply(object, predict, ... = ...)
  if(!is.null(names(object))) names(out) <- names(object)
  out
}



#' Extract predictions and class probabilities from train objects
#'
#' These functions can be used for a single \code{train} object or to loop
#' through a number of \code{train} objects to calculate the training and test
#' data predictions and class probabilities.
#'
#' These functions are wrappers for the specific prediction functions in each
#' modeling package. In each case, the optimal tuning values given in the
#' \code{tuneValue} slot of the \code{finalModel} object are used to predict.
#'
#' To get simple predictions for a new data set, the \code{predict} function
#' can be used. Limits can be imposed on the range of predictions. See
#' \code{\link{trainControl}} for more information.
#'
#' To get predictions for a series of models at once, a list of
#' \code{\link{train}} objects can be passes to the \code{predict} function and
#' a list of model predictions will be returned.
#'
#' The two extraction functions can be used to get the predictions and observed
#' outcomes at once for the training, test and/or unknown samples at once in a
#' single data frame (instead of a list of just the predictions). These objects
#' can then be passes to \code{\link{plotObsVsPred}} or
#' \code{\link{plotClassProbs}}.
#'
#' @aliases predict.list predict.train extractPrediction extractProb
#' @param object For \code{predict.train}, an object of class
#' \code{\link{train}}. For \code{predict.list}, a list of objects of class
#' \code{\link{train}}.
#' @param newdata an optional set of data to predict on. If \code{NULL}, then
#' the original training data are used but, if the \code{train} model used a
#' recipe, an error will occur.
#' @param type either "raw" or "prob", for the number/class predictions or
#' class probabilities, respectively. Class probabilities are not available for
#' all classification models
#' @param models a list of objects of the class \code{train}. The objects must
#' have been generated with \code{fitBest = FALSE} and \code{returnData =
#' TRUE}.
#' @param na.action the method for handling missing data
#' @param testX an optional set of data to predict
#' @param testY an optional outcome corresponding to the data given in
#' \code{testX}
#' @param unkX another optional set of data to predict without known outcomes
#' @param unkOnly a logical to bypass training and test set predictions. This
#' is useful if speed is needed for unknown samples.
#' @param verbose a logical for printing messages
#' @param \dots only used for \code{sort} and \code{modelCor} and captures
#' arguments to pass to \code{sort} or \code{FUN}.
#' @return
#'
#' For \code{predict.train}, a vector of predictions if \code{type = "raw"} or
#' a data frame of class probabilities for \code{type = "prob"}. In the latter
#' case, there are columns for each class.
#'
#' For \code{predict.list}, a list results. Each element is produced by
#' \code{predict.train}.
#'
#' For \code{extractPrediction}, a data frame with columns: \item{obs }{the
#' observed training and test data} \item{pred }{predicted values}
#' \item{model}{the type of model used to predict} \item{object}{the names of
#' the objects within \code{models}. If \code{models} is an un-named list, the
#' values of \code{object} will be "Object1", "Object2" and so on}
#' \item{dataType }{"Training", "Test" or "Unknown" depending on what was
#' specified}
#'
#' For \code{extractProb}, a data frame. There is a column for each class
#' containing the probabilities. The remaining columns are the same as above
#' (although the \code{pred} column is the predicted class)
#' @author Max Kuhn
#' @seealso \code{\link{plotObsVsPred}}, \code{\link{plotClassProbs}},
#' \code{\link{trainControl}}
#' @references Kuhn (2008), ``Building Predictive Models in R Using the caret''
#' (\doi{10.18637/jss.v028.i05})
#' @keywords manip
#' @examples
#'
#'    \dontrun{
#'
#' knnFit <- train(Species ~ ., data = iris, method = "knn",
#'                 trControl = trainControl(method = "cv"))
#'
#' rdaFit <- train(Species ~ ., data = iris, method = "rda",
#'                 trControl = trainControl(method = "cv"))
#'
#' predict(knnFit)
#' predict(knnFit, type = "prob")
#'
#' bothModels <- list(knn = knnFit,
#'                    tree = rdaFit)
#'
#' predict(bothModels)
#'
#' extractPrediction(bothModels, testX = iris[1:10, -5])
#' extractProb(bothModels, testX = iris[1:10, -5])
#'   }
#'
#' @method predict train
#' @export predict.train
#' @export
predict.train <- function(object, newdata = NULL, type = "raw", na.action = na.omit, ...) {
  if(all(names(object) != "modelInfo")) {
    object <- update(object, param = NULL)
  }
  if(!is.null(object$modelInfo$library))
    for(i in object$modelInfo$library)
      do.call("requireNamespaceQuietStop", list(package = i))
  if(!(type %in% c("raw", "prob"))) stop("type must be either \"raw\" or \"prob\"")
  if(type == "prob") {
    if (is.null(object$modelInfo$prob))
      stop("only classification models that produce probabilities are allowed")
  }

  if(!is.null(newdata)) {
    if (inherits(object, "train.formula")) {
      newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
      rn <- row.names(newdata)
      Terms <- delete.response(object$terms)
      m <- model.frame(Terms, newdata, na.action = na.action, xlev = object$xlevels)
      if (!is.null(cl <- attr(Terms, "dataClasses")))
        .checkMFClasses(cl, m)
      keep <- match(row.names(m), rn)
      newdata <- model.matrix(Terms, m, contrasts = object$contrasts)
      xint <- match("(Intercept)", colnames(newdata), nomatch = 0)
      if (xint > 0)
        newdata <- newdata[, -xint, drop = FALSE]
    }
  }
  else if(object$control$method != "oob") {
    if(!is.null(object$trainingData)) {
      if(object$method == "pam") {
        newdata <- object$finalModel$xData
      } else {
        newdata <- object$trainingData
        newdata$.outcome <- NULL
        if("train.formula" %in% class(object) &&
           any(unlist(lapply(newdata, is.factor)))) {
          newdata <- model.matrix(~., data = newdata)[,-1]
          newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
        }
      }
    } else stop("please specify data via newdata")
  }

  if("xNames" %in% names(object$finalModel) &
     is.null(object$preProcess$method$pca) &
     is.null(object$preProcess$method$ica))
      newdata <- newdata[, colnames(newdata) %in% object$finalModel$xNames, drop = FALSE]

  if(type == "prob") {
    out <- probFunction(method = object$modelInfo,
                        modelFit = object$finalModel,
                        newdata = newdata,
                        preProc = object$preProcess)
    obsLevels <- levels(object)
    out <- out[, obsLevels, drop = FALSE]
  } else {
    out <- predictionFunction(method = object$modelInfo,
                              modelFit = object$finalModel,
                              newdata = newdata,
                              preProc = object$preProcess)
    if (object$modelType == "Regression") {
      out <- trimPredictions(pred = out,
                             mod_type =object$modelType,
                             bounds = object$control$predictionBounds,
                             limits = object$yLimit)
    } else {
      if(!("levels" %in% names(object)))
         object$levels <- levels(object)
      out <- outcome_conversion(as.character(out), lv = object$levels)
    }
  }
  out
}
