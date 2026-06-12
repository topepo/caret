modelInfo <- list(label = "Rotation Forest",
                  library = "rotationForest",
                  type = c("Classification"),
                  parameters = data.frame(parameter = c("K", "L"),
                                          class = rep("numeric", 2),
                                          label = c("#Variable Subsets", "Ensemble Size")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    feas_k <- 1:15
                    feas_k <- feas_k[ncol(x)%%feas_k == 0]
                    if(search == "grid") {
                      out <- expand.grid(K = feas_k[1:min(len, length(feas_k))], L = (1:len)*3) 
                    } else {
                      out <- data.frame(K = sample(feas_k, size = len, replace = TRUE), 
                                        L = sample(1:100, size = len, replace = TRUE))
                    }
                    out
                  },
                  loop = function(grid) { 
                    grid <- grid[order(grid$K, -grid$L, decreasing = TRUE),, drop = FALSE]
                    unique_k <- unique(grid$K)
                    loop <- data.frame(K = unique_k, L = NA)  
                    submodels <- vector(mode = "list", length = length(unique_k))
                    for(i in seq(along = unique_k)) {
                      sub_L <- grid[grid$K == unique_k[i],"L"]
                      loop$L[loop$K == unique_k[i]] <- sub_L[which.max(sub_L)]
                      submodels[[i]] <- data.frame(L = sub_L[-which.max(sub_L)])
                    }    
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    param$K <- min(param$k, floor(ncol(x)/2))
                    if(length(lev) != 2)
                      stop("rotationForest is only implemented for binary classification")
                    y <- ifelse(y == lev[1], 1, 0)
                    if(!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
                    rotationForest::rotationForest(x, y, K = param$K, L = param$L, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    out <- predict(modelFit, newdata)
                    out <- ifelse(out >= .5, modelFit$obsLevels[1], modelFit$obsLevels[2])
                    
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      all_L <- predict(modelFit, newdata, all = TRUE)
                      for(j in seq(along = submodels$L)) {                        
                        tmp_pred <- apply(all_L[, 1:submodels$L[j],drop = FALSE], 1, mean)
                        tmp[[j+1]] <- ifelse(tmp_pred >= .5, modelFit$obsLevels[1], modelFit$obsLevels[2])
                      }
                      out <- tmp
                    }
                    out   
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                    all_L <- predict(modelFit, newdata, all = TRUE)
                    out <- apply(all_L, 1, mean)
                    out <- data.frame(x = out, y = 1 - out)
                    colnames(out) <- modelFit$obsLevels
                    if(!is.null(rownames(newdata))) rownames(out) <- rownames(newdata)
                    
                    if(!is.null(submodels)) {
                      tmp <- vector(mode = "list", length = nrow(submodels) + 1)
                      tmp[[1]] <- out
                      for(j in seq(along = submodels$L)) {  
                        tmp_pred <- apply(all_L[, 1:submodels$L[j],drop = FALSE], 1, mean)
                        tmp_pred <- data.frame(x = tmp_pred, y = 1 - tmp_pred)
                        colnames(tmp_pred) <- modelFit$obsLevels
                        if(!is.null(rownames(newdata))) rownames(tmp_pred) <- rownames(newdata)
                        tmp[[j+1]] <- tmp_pred
                      }
                      out <- tmp
                    }
                    out   
                  },
                  predictors = function(x, ...) {
                    non_zero <- function(x) {                      
                      out <- apply(x, 1, function(x) any(x != 0))
                      names(out)[out]
                    }
                    sort(unique(unlist(lapply(x$loadings, non_zero))))
                  },
                  varImp = function(object, ...) {
                    vis <- lapply(object$models, varImp, scale = FALSE)
                    wgt <- vector(mode = "list", length = length(vis))
                    for(i in seq(along = vis)) {
                      tmp <- vis[[i]]
                      vi1 <- tmp[,1]
                      names(vi1) <- rownames(tmp)
                      l1 <- object$loadings[[i]]
                      tmp2 <- vi1 %*% abs(as.matrix(l1[names(vi1),]))
                      tmp2 <- tmp2[,sort(colnames(tmp2))]
                      wgt[[i]] <- tmp2
                    }
                    wgt <- do.call("rbind", wgt)
                    vi <- apply(wgt, 2, mean)
                    out <- data.frame(Overall = vi)
                    rownames(out) <- colnames(wgt)
                    out
                  },
                  levels = function(x) x$obsLevels,
                  tags = c("Ensemble Model", "Implicit Feature Selection", 
                           "Feature Extraction Models", "Tree-Based Model", "Two Class Only"),
                  sort = function(x) x[order(x[,1]),])
