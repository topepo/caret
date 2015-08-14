modelInfo <- list(label = "Local Fisher Discriminant Analysis",
                  library = c("lfda"),
                  type = "Classification",
                  grid = function(x, y, len = NULL) {
                    stop("grid is not available for lfda. ")
                  },
                  fit = function(x, y, r = NULL, metric = c("orthonormalized","plain","weighted"),knn = 5, ...) {
                    theDots <- list(...)

                    if(is.data.frame(x)) x <- as.matrix(x)
                    if(is.null(r)){
                      r <- 3
                      print("Reduced dimension to 3 by default. ")
                    }

                    metric <- match.arg(metric)
                    modelArgs <- c(list(x,y,r,metric,knn))
                    out <- do.call("lfda", modelArgs)

                    out$call <- NULL
                    out
                  },
                  predict = function(modelFit, newdata = NULL, type = "raw", ...) {
                    if(is.null(newdata)){stop("You must provide data to be used for transformation. ")}
                    if(type!="raw"){stop('Types other than "raw" are currently unavailable. ')}
                    if(is.data.frame(newdata)) newdata <- as.matrix(newdata)

                    transformMatrix <- object$T

                    out <- newdata %*% transformMatrix
                    out
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    stop('type "prob" in predict function is not available for lfda.')
                  },
                  predictors = function(x, ...) {
                    # if dimensionality of original data is not reduced
                    if(dim(x$T)[1]==dim(x$T)[2]){
                      return(colnames(x$Z))
                    } else {
                      stop("predictors are not available for lfda model with dimension reduction. ")
                    }
                  },
                  tags = c("Metric Learning", "Local Metric Learning", "Dimension Reduction",
                           "Multimodality Preservance", "Fisher Discriminant Analysis",
                           "Classification", "Pre-processing")
                  )
