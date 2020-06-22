modelInfo <- list(label = "Regularized Linear Discriminant Analysis",
                  library = "sparsediscrim",
                  loop = NULL,
                  type = "Classification",
                  parameters = data.frame(parameter = "estimator",
                                          class = "character",
                                          label = "Regularization Method"),
                  grid = function(x, y, len = NULL, search = "grid"){
                    data.frame(estimator = c("Moore-Penrose Pseudo-Inverse",
                                             "Schafer-Strimmer",
                                             "Thomaz-Kitani-Gillies"))
                  }, 
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(as.character(param$estimator == "Moore-Penrose Pseudo-Inverse")) {
                      out <- sparsediscrim::lda_pseudo(x, y, ...)
                    } else {
                      if(as.character(param$estimator == "Schafer-Strimmer")) {
                        out <- sparsediscrim::lda_schafer(x, y, ...)
                      } else out <- sparsediscrim::lda_thomaz(x, y, ...)
                    }
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) 
                    predict(modelFit, newdata)$class,
                  prob = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, newdata)$scores
                    as.data.frame(t(apply(out, 2, function(x) exp(-x)/sum(exp(-x)))), stringsAsFactors = TRUE)
                  },
                  predictors = function(x, ...) x$varnames,
                  tags = c("Discriminant Analysis", "Polynomial Model", "Regularization",
                           "Linear Classifier"),
                  levels = function(x) names(x$prior),
                  sort = function(x) x)
