modelInfo <- list(
  label = "Local Fisher Discriminant Analysis",
  library = c("lfda"),
  type = "Classification",
  grid = function(x, y, len = NULL, search = "grid"){
    if(is.null(len)) len <- 1
    expand.grid(
      r=3:(min(3 - 1 + len, 5)),
      metric=c("plain", "orthonormalized", "weighted")[1:(min(len, 3))],
      knn=25:(25 - 1 + len),
      stringsAsFactors=FALSE)
  },
  parameters = data.frame(
    parameter = c("r", "metric", "knn"),
    class = c("numeric", "character", "numeric"),
    label = c("# Reduced Dimensions",
              "Type of Transformation Metric",
              "# of Nearest Neighbors")),
  fit = function(x, y, param, ...) {
    lfda(x=x, y=y, r=param$r, metric=as.character(param$metric), knn=param$k, ...)
  },
  predict = function(modelFit, newdata, submodels = NULL){
    out <- predict(modelFit, newdata, type='class')
    out <- factor(out, levels=modelFit$levels)
  },
  prob = function(modelFit, newdata, submodels = NULL){
    predict(modelFit, newdata, type='raw')
  },
  tags = c("Metric Learning", "Local Metric Learning", "Dimension Reduction",
           "Multimodality Preservance", "Fisher Discriminant Analysis",
           "Classification", "Pre-processing")
)
