modelInfo <- list(label = "Local Fisher Discriminant Analysis",
                  library = c("lfda"),
                  type = "Classification",
                  grid = function(x, y, len = NULL, search = "grid") data.frame(r="none",metric="none", knn="none"),
                  parameters = data.frame(parameter = c("r", "metric", "knn"),
                                          class = c("numeric", "character", "numeric"),
                                          label = c("# Reduced Dimensions",
                                                    "Type of Transformation Metric",
                                                    "# of Nearest Neighbors")),
                  fit = function(x, y, param, ...) {
                    theDots <- list(...)

                    argList <- list(x = x, y = y, r = ifelse(is.null(param$r, 3, param$r)),
                                    metric = ifelse(is.null(param$metric), "plain", param$metric),
                                    knn = ifelse(is.null(param$knn, 5, param$knn)))
                    argList <- c(argList, theDots)

                    if(is.data.frame(x)) x <- as.matrix(x)

                    out <- do.call("lfda", argList)

                    out$call <- NULL
                    out
                  },
#                 predict = function(modelFit, newdata, submodels = NULL)
#                   predict(modelFit, newdata),
                  prob = NULL,
                  predictors = function(x, ...) {
                    # if dimensionality of original data is not reduced
                    if(dim(x$T)[1]==dim(x$T)[2]){
                      return(colnames(x$Z))
                    } else {
                      print("predictors are not available for lfda model with dimension reduction. ")
                      return(NULL)
                    }
                  },
                  tags = c("Metric Learning", "Local Metric Learning", "Dimension Reduction",
                           "Multimodality Preservance", "Fisher Discriminant Analysis",
                           "Classification", "Pre-processing")
                  )
