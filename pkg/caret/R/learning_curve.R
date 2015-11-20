learing_curve_dat <- function(dat, 
                              outcome = NULL,
                              proportion = (1:10)/10, 
                              test_prop = 0, 
                              verbose = TRUE, ...) {
  if(is.null(outcome))
    stop("Please give a character stirng for the outcome column name")
  proportion <- sort(unique(proportion))
  n_size <- length(proportion)
  
  if(test_prop > 0) {
    for_model <- createDataPartition(dat[, outcome], p = 1 - test_prop, list = FALSE)
  } else for_model <- 1:nrow(dat)
  
  n <- length(for_model)
  
  resampled <- vector(mode = "list", length = n_size)
  tested <- if(test_prop > 0) resampled else NULL
  apparent <- resampled
  for(i in seq(along = proportion)) {
    if(verbose) cat("Training for ", round(proportion[i]*100, 1), 
                    "% (n = ", floor(n*proportion[i]), ")\n", sep = "")
    in_mod <- if(proportion[i] < 1) sample(for_model, size = floor(n*proportion[i])) else for_model
    mod <- train(x = dat[in_mod, colnames(dat) != outcome, drop = FALSE],
                 y = dat[in_mod, outcome],
                 ...)
    if(i == 1) perf_names <- mod$perfNames
    resampled[[i]] <- merge(mod$resample, mod$bestTune)
    resampled[[i]]$Training_Size <- length(in_mod)
    
    if(test_prop > 0) {
      if(!mod$control$classProbs) {
        test_preds <- extractPrediction(list(model = mod), 
                                        testX = dat[-for_model, colnames(dat) != outcome, drop = FALSE],
                                        testY = dat[-for_model, outcome])
      } else {
        test_preds <- extractProb(list(model = mod), 
                                  testX = dat[-for_model, colnames(dat) != outcome, drop = FALSE],
                                  testY = dat[-for_model, outcome])
      }
      test_perf <- mod$control$summaryFunction(test_preds, lev = mod$finalModel$obsLevels)
      test_perf <- as.data.frame(t(test_perf))
      test_perf$Training_Size <- length(in_mod)
      tested[[i]] <- test_perf
      try(rm(test_preds, test_perf), silent = TRUE)
    }
    
    if(!mod$control$classProbs) {
      app_preds <- extractPrediction(list(model = mod), 
                                     testX = dat[in_mod, colnames(dat) != outcome, drop = FALSE],
                                     testY = dat[in_mod, outcome])
    } else {
      app_preds <- extractProb(list(model = mod), 
                               testX = dat[in_mod, colnames(dat) != outcome, drop = FALSE],
                               testY = dat[in_mod, outcome])
    }
    app_perf <- mod$control$summaryFunction(app_preds, lev = mod$finalModel$obsLevels)
    app_perf <- as.data.frame(t(app_perf))
    app_perf$Training_Size <- length(in_mod)    
    apparent[[i]] <- app_perf
    
    try(rm(mod, in_mod, app_preds, app_perf), silent = TRUE)
  }
  
  resampled <- do.call("rbind", resampled)
  resampled <- resampled[, c(perf_names, "Training_Size")]
  resampled$Data <- "Resampling"
  apparent <- do.call("rbind", apparent)
  apparent <- apparent[, c(perf_names, "Training_Size")]
  apparent$Data <- "Training"
  out <- rbind(resampled, apparent)
  if(test_prop > 0) {
    tested <- do.call("rbind", tested)
    tested <- tested[, c(perf_names, "Training_Size")]
    tested$Data <- "Testing"
    out <- rbind(out, tested)
  }
  out
}

