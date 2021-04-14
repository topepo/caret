modelInfo <- list(label = "partDSA",
                  library = "partDSA",
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c('cut.off.growth', 'MPD'),
                                          class = c("numeric", "numeric"),
                                          label = c('Number of Terminal Partitions', 'Minimum Percent Difference')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(cut.off.growth = 1:len, MPD = .1)
                    } else {
                      out <- data.frame(cut.off.growth = sample(1:20, size = len, replace = TRUE),
                                        MPD = runif(len, min = 0, max = .5))
                    }
                    out
                  },
                  loop = function(grid) {   
                    grid <- grid[order(grid$MPD, grid$cut.off.growth, decreasing = TRUE),, drop = FALSE]
                    
                    uniqueMPD <- unique(grid$MPD)
                    
                    loop <- data.frame(MPD = uniqueMPD)
                    loop$cut.off.growth <- NA
                    
                    submodels <- vector(mode = "list", length = length(uniqueMPD))
                    
                    for(i in seq(along = uniqueMPD)) {
                      subCuts <- grid[grid$MPD == uniqueMPD[i],"cut.off.growth"]
                      loop$cut.off.growth[loop$MPD == uniqueMPD[i]] <- subCuts[which.max(subCuts)]
                      submodels[[i]] <- data.frame(cut.off.growth = subCuts[-which.max(subCuts)])
                    }
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    if(!is.data.frame(x) | inherits(x, "tbl_df")) 
                      x <- as.data.frame(x, stringsAsFactors = TRUE)
                    partDSA::partDSA(x, y,
                                     control = partDSA::DSA.control(
                                       cut.off.growth = param$cut.off.growth,
                                       MPD = param$MPD,
                                       vfold = 1),
                                     ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata) | inherits(newdata, "tbl_df")) 
                      newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    if(!is.null(submodels)) {
                      tmp <- c(modelFit$tuneValue$cut.off.growth, submodels$cut.off.growth)
                      
                      ## There are cases where the number of models saved by the function is
                      ## less than the values in cut.off.growth (e.g. cut.off.growth = 1:10
                      ## but partDSA only has 6 partitions). We will predict the "overage" using
                      ## the largest model in the obejct (e.g. models 7:10 predicted by model 6).
                      if(modelFit$problemType == "Classification") {
                        out <- partDSA::predict.dsa(modelFit, newdata)
                        if(max(tmp) > length(out)) tmp[tmp > length(out)] <- length(out)
                        out <- out[tmp]
                      } else {
                        out <- partDSA::predict.dsa(modelFit, newdata)
                        if(max(tmp) > ncol(out)) tmp[tmp > ncol(out)] <- ncol(out)
                        out <- out[,tmp, drop= FALSE]
                        out <- as.list(as.data.frame(out, stringsAsFactors = TRUE))
                      }
                    } else {
                      ## There maybe less items than modelFit$cut.off.growth
                      index <- min(modelFit$cut.off.growth, length(modelFit$test.set.risk.DSA))
                      ## use best Tune
                      if(modelFit$problemType == "Classification") {
                        out <- as.character(partDSA::predict.dsa(modelFit, newdata)[[index]])
                      } else {
                        out <- partDSA::predict.dsa(modelFit, newdata)[,index]
                      }
                    }
                    out        
                  },
                  predictors = function(x, cuts = NULL, ...) {
                    if(is.null(cuts) & !is.null(x$tuneValue)) {
                      cuts <- x$tuneValue$cut.off.growth[1]
                    } else {
                      if(is.null(cuts)) stop("please supply a value for 'cuts'")
                    }
                    tmp <- x$var.importance[,cuts]
                    names(tmp)[which(tmp != 0)]
                  },
                  levels = function(x) x$obsLevels,
                  tags = "",
                  prob = NULL,
                  varImp = function(object, cuts = NULL, ...) {
                    if(is.null(cuts) & !is.null(object$tuneValue)) {
                      cuts <- object$tuneValue$cut.off.growth[1]
                    } else {
                      if(is.null(cuts)) stop("please supply a value for 'cuts'")
                    }
                    tmp <- object$var.importance[,cuts]
                    out <- data.frame(Overall = tmp)
                    rownames(out) <- names(tmp)
                    out
                  },
                  sort = function(x) x[order(x$cut.off.growth, x$MPD),])
