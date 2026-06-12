modelInfo <- list(label = "Gradient Boosting Machines",
                  library = "h2o",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('ntrees', 'max_depth', 'min_rows',
                                                        'learn_rate', 'col_sample_rate'),
                                          class = rep("numeric", 5),
                                          label = c('# Boosting Iterations', 'Max Tree Depth',
                                                    'Min. Terminal Node Size', 'Shrinkage',
                                                    '#Randomly Selected Predictors')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(max_depth = seq(1, len),
                                         ntrees = floor((1:len) * 50),
                                         learn_rate = .1,
                                         min_rows = 10,
                                         col_sample_rate = 1)
                    } else {
                      out <- data.frame(ntrees = floor(runif(len, min = 1, max = 5000)),
                                        max_depth = sample(1:10, replace = TRUE, size = len),
                                        learn_rate = runif(len, min = .001, max = .6),
                                        min_rows = sample(5:25, replace = TRUE, size = len),
                                        col_sample_rate = runif(len))
                    }
                    out
                  },
                  loop = NULL,
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    lvs <- length(levels(y))
                    fam <- "gaussian"
                    if(lvs == 2) fam <- "bernoulli"
                    if(lvs >  2) fam <- "multinomial" ## intercept ... for family arg

                    dat <- if(!is.data.frame(x)) as.data.frame(x, stringsAsFactors = TRUE) else x
                    dat$.outcome <- y
                    frame_name <- paste0("tmp_gbm_dat_",sample.int(100000, 1))
                    tmp_train_dat = h2o::as.h2o(dat, destination_frame = frame_name)

                    out <- h2o::h2o.gbm(x = colnames(x), y = ".outcome",
                                        training_frame = tmp_train_dat,
                                        distribution = fam,
                                        ntrees = param$ntrees, max_depth = param$max_depth,
                                        learn_rate = param$learn_rate, min_rows = param$min_rows,
                                        col_sample_rate = param$col_sample_rate,
                                        ...)
                    h2o::h2o.getModel(out@model_id)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    frame_name <- paste0("new_gbm_dat_",sample.int(100000, 1))
                    newdata <- h2o::as.h2o(newdata, destination_frame = frame_name)
                    as.data.frame(predict(modelFit, newdata), stringsAsFactors = TRUE)[,1]
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    frame_name <- paste0("new_gbm_dat_",sample.int(100000, 1))
                    newdata <- h2o::as.h2o(newdata, destination_frame = frame_name)
                    as.data.frame(predict(modelFit, newdata), stringsAsFactors = TRUE)[,-1]
                  },
                  predictors = function(x, ...) {
                    out <- as.data.frame(h2o::h2o.varimp(x), stringsAsFactors = TRUE)
                    out <- subset(out, relative_importance > 0)
                    as.character(out$variable)
                  },
                  varImp = function(object, ...) {
                    out <- as.data.frame(h2o::h2o.varimp(object), stringsAsFactors = TRUE)
                    colnames(out)[colnames(out) == "relative_importance"] <- "Overall"
                    rownames(out) <- out$variable
                    out[, c("Overall"), drop = FALSE]
                  },
                  levels = function(x) x@model$training_metrics@metrics$domain,
                  tags = c("Tree-Based Model", "Boosting",
                           "Ensemble Model", "Implicit Feature Selection"),
                  sort = function(x) x[order(x$ntrees, x$max_depth, x$learn_rate),],
                  trim = NULL)
