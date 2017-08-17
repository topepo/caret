modelInfo <- list(label = "Binary Discriminant Analysis",
                  library = "binda",
                  loop = NULL,
                  type = c("Classification"),
                  parameters = data.frame(parameter = c("lambda.freqs"),
                                          class = c("numeric"),
                                          label = c('Shrinkage Intensity')),
                  grid = function(x, y, len = NULL, search = "grid"){
                    if(search == "grid") {
                      out <- data.frame(lambda.freqs = seq(0, 1, length = len))
                    } else {
                      out <- data.frame(lambda.freqs = runif(len, min = 0, max = 1))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    binda::binda(as.matrix(x), y, lambda.freqs = param$lambda.freqs, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    as.character(predict(modelFit, as.matrix(newdata))$class)
                  },
                  prob = function(modelFit, newdata, submodels = NULL){
                    predict(modelFit, as.matrix(newdata))$posterior
                  },
                  varImp = NULL,
                  predictors = function(x, ...) rownames(x$logp0),
                  levels = function(x) if(any(names(x) == "obsLevels")) x$obsLevels else names(x$logfreqs),
                  tags = c("Discriminant Analysis", "Two Class Only", "Binary Predictors Only"),
                  sort = function(x) x)
