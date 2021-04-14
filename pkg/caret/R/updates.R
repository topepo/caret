#' Update or Re-fit a Model
#'
#' \code{update} allows a user to over-ride the tuning parameter selection
#' process by specifying a set of tuning parameters or to update the model
#' object to the latest version of this package.
#'
#' If the model object was created with version 5.17-7 or earlier, the
#' underlying package structure was different. To make old \code{\link{train}}
#' objects consistent with the new structure, use \code{param = NULL} to get
#' the same object back with updates.
#'
#' To update the model parameters, the training data must be stored in the
#' model object (see the option \code{returnData} in
#' \code{\link{trainControl}}. Also, all tuning parameters must be specified in
#' the \code{param} slot. All other options are held constant, including the
#' original pre-processing (if any), options passed in using code... and so on.
#' When printing, the verbiage "The tuning parameter was set manually." is used
#' to describe how the tuning parameters were created.
#'
#' @param object an object of class \code{\link{train}}
#' @param param a data frame or named list of all tuning parameters
#' @param \dots not currently used
#' @return a new \code{\link{train}} object
#' @author Max Kuhn
#' @seealso \code{\link{train}}, \code{\link{trainControl}}
#' @keywords models
#' @examples
#'
#' \dontrun{
#' data(iris)
#' TrainData <- iris[,1:4]
#' TrainClasses <- iris[,5]
#'
#' knnFit1 <- train(TrainData, TrainClasses,
#'                  method = "knn",
#'                  preProcess = c("center", "scale"),
#'                  tuneLength = 10,
#'                  trControl = trainControl(method = "cv"))
#'
#' update(knnFit1, list(.k = 3))
#' }
#' @method update train
#' @export
#' @importFrom recipes juice all_predictors all_outcomes
update.train <- function(object, param = NULL, ...) {
  if(is.null(param)) {
    if (all(names(object) != "modelInfo")) {
      funcs <- try(getModelInfo(object$method)[[1]], silent = TRUE)
      if (class(funcs)[1] == "list" && length(funcs) > 0) {
        funcs$updated <- TRUE
        object$modelInfo <- funcs
        warning(
          paste(
            "The model was updated to work with the current version of caret.",
            "Please re-create the model object since future versions will",
            "require objects to be created from caret versions >= 6.",
            "Alternatively, do not update caret beyond version 5.17-7."
          )
        )
      } else
        stop(
          paste(
            "This appears to be from an old version of caret",
            "and the model type is unknown to the new version"
          )
        )
    }
  } else {
    ## check for original data
    if (is.null(object$trainingData))
      stop("original training data is needed; use returnData = TRUE in trainControl()")

    if(is.list(param)) param <- as.data.frame(param, stringsAsFactors = TRUE)
    dotNames <- hasDots(param, object$modelInfo)
    if(dotNames) colnames(param) <- gsub("^\\.", "", colnames(param))

    if (!is.data.frame(param))
      stop("param should be a data frame or a named list")
    if (nrow(param) > 1)
      stop("only one set of parameters should be specified")

    paramNames <- as.character(object$modelInfo$parameter$parameter)
    if (length(paramNames) != ncol(param))
      stop(paste("Parameters should be", paste(paramNames, sep = "", collapse = ", ")))
    if (any(sort(names(param)) != sort(paste(paramNames, sep = ""))))
      stop(paste("Parameters should be", paste(paramNames, sep = "", collapse = ", ")))

    ## get pre-processing options
    if (!is.null(object$preProcess)) {
      ppOpt <- list(options = object$preProcess$method)
      if(length(object$control$preProcOptions) > 0) ppOpt <- c(ppOpt,object$control$preProcOptions)
    } else ppOpt <- NULL

    ## refit model with new parameters
    args <-
      list(method = object$modelInfo,
           tuneValue = param,
           obsLevels = levels(object$trainingData$.outcome),
           pp = ppOpt,
           last = TRUE,
           classProbs = object$control$classProbs)

    if (inherits(object, "train.recipe")) {
      args$x <-
        juice(
          object$recipe,
          all_predictors(),
          composition = "data.frame"
        )
      args$y <-
        juice(
          object$recipe,
          all_outcomes()
        )
      args$y <- args$y[[1]]
      if (length(levels(args$y)) > 0) {
        args$obsLevels <- levels(args$y)
      } else {
        args["obsLevels"] <- list(NULL)
      }

    } else {
      args$x <-
        object$trainingData[, !(colnames(object$trainingData) %in% c(".outcome", ".weights"))]
      args$y <- object$trainingData$.outcome
    }

    if (any(names(object$trainingData) == ".weights")) {
      args$wts <- object$trainingData$.weights
    } else args <- c(args, list(wts = NULL))

    if (length(object$dots) > 0) args <- c(args, object$dots)
    finalFinalModel <- do.call("createModel", args)
    object$finalModel <- finalFinalModel$fit
    object$preProcess <- finalFinalModel$preProc
    object$bestTune <- param


    ## modify objects so print method reflects update
    object$update <- param
  }
  ## return object
  object
}
