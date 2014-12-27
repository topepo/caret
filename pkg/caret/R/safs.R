sa_internal_names <- c('Best','Note','Random','Prob','Iter','Cycle',
                       'SinceRestart','Size','Similarity','Similarity_B','Resample')
sa_external_names <- c('Iter','Resample')

sa_func_check <- function(x) {
  fnames <- names(x) 
  required <- c('fit', 'fitness_intern', 'pred', 'fitness_extern', 
                'initial', 'perturb', 'prob', 'selectIter')
  missing <- !(required %in% fnames)
  if(any(missing)) 
    stop(paste("The following functions are missing from the 'func' argument:",
               paste(required[missing], sep = "", collapse = ",")))
  invisible(x)
  args <- lapply(x, function(x) names(formals(x)))
  expected <- list(fit = c('x', 'y', 'lev', 'last', '...'),                   
                   fitness_intern = c('object', 'x', 'y', 'maximize', 'p'),
                   pred = c('object', 'x'),
                   fitness_extern = c('data', 'lev', 'model'),
                   initial = c('vars', 'prob', '...'),
                   perturb = c('x', 'vars', 'number'),
                   prob = c('old', 'new', 'iteration'),
                   selectIter = c('x', 'metric', 'maximize'))
  
  if(is.vector(x$initial)) {
    x <- x[!(names(x) == "initial")]
    expected <- expected[!(names(expected) == "initial")]
  }
  
  for(i in names(x)) {
    .args <- names(formals(x[[i]]))
    .check <- same_args(.args, expected[[i]])
    if(!.check) {
      stop(paste("Arguments to function", i, "should be {",
                 paste(expected[[i]], sep = "", collapse = ", "),
                 "}  and these were given {",
                 paste(.args, sep = "", collapse = ", "), "}\n"))
    }
  }
}



sa_bl_correct0 <- function(x) {
  extras <- c('Best', 'Note', 'Random', 'Prob', 'Iter', 
              'Cycle', 'SinceRestart', 'Size', 'Resample')
  bl <- x[which.min(x$Iter),]
  bl <- bl[, !(names(bl) %in% extras)]
  perf_names <- names(bl)
  for(i in perf_names) x[, i] <- x[,i] - bl[,i]
  x
}

sa_bl_correct <- function(x) ddply(x, .(Resample), sa_bl_correct0 )

print.safs <- function (x, top = 5, 
                        digits = max(3, getOption("digits") - 3), 
                        ...) {
  cat("\nSimulated Annealing Feature Selection\n\n")
  
  cat(x$dims[1], 
      " samples\n", 
      x$dims[2],
      " predictor", ifelse(x$dims[2] > 1, "s\n", "\n"),
      sep = "")
  if(!is.null(x$levels)){
    cat(length(x$levels), 
        "classes:",
        paste("'", x$levels, "'", sep = "", collapse = ", "),
        "\n")
  }
  cat("\n")
  
  cat("Maximum search iterations:", max(x$iters), "\n")
  if(x$control$improve < Inf) {
    num_re <- ddply(x$internal, "Resample", function(x) sum(x$Note == "Restart"))
    cat("Restart after ", x$control$improve, " iterations without improvement (",
        round(mean(num_re$V1), 1), " restarts on average)\n", sep = "")
  }
  cat("\n")
  inames <- names(x$internal)
  inames <- inames[!(inames %in% sa_internal_names)]
  enames <- names(x$external)
  enames <- enames[!(enames %in% sa_external_names)]
  cat("Internal performance value", ifelse(length(inames) > 1, "s: ", ": "),
      paste(inames, sep = "", collapse = ", "), "\n", sep = "")
  cat("Subset selection driven to", 
      if(x$control$maximize["internal"]) "maximize internal" else "minimize internal", 
      x$control$metric["internal"],  "\n")  
  cat("\n")
  cat("External performance value", ifelse(length(enames) > 1, "s: ", ": "),
      paste(enames, sep = "", collapse = ", "), "\n", sep = "")  
  if(x$auto) {
    cat("Best iteration chose by", 
        if(x$control$maximize["external"]) "maximizing external" else "minimizing external", 
        x$control$metric["external"],  "\n")
  } else {
    cat("Best iteration chosen manually\n")
  }
  resampleN <- unlist(lapply(x$control$index, length))
  numResamp <- length(resampleN)
  resampText <- getFromNamespace("resampName", "caret")(x)
  cat("External resampling method:", resampText, "\n")
  if(x$control$holdout > 0)
    cat("Subsampling for internal fitness calculation: ", 
        round(x$control$holdout*100, digits), "%\n", sep = "")
  
  cat("\n")
  
  vars <- sort(table(unlist(x$resampled_vars)), decreasing = TRUE)
  
  top <- min(top, length(vars))
  
  smallVars <- vars[1:top]
  smallVars <- round(smallVars/length(x$control$index)*100, 1)
  
  varText <- paste0(names(smallVars), " (", smallVars, "%)")
  varText <- paste(varText, collapse = ", ")
  
  if(!all(is.na(smallVars))) {
    cat("During resampling:\n  * the top ",
        top,
        " selected variables (out of a possible ",
        x$dims[2],
        "):\n    ",
        varText,
        "\n",
        sep = "")
    cat("  * on average, ",
        round(mean(unlist(lapply(x$resampled_vars, length))), 1),
        " variables were selected (min = ",
        round(min(unlist(lapply(x$resampled_vars, length))), 1),
        ", max = ",
        round(max(unlist(lapply(x$resampled_vars, length))), 1),
        ")\n\n",
        sep = "")
  } else {
    cat("During resampling, no variables were selected.\n\n")
  }
  
  cat("In the final search using the entire training set:\n",
      "  *", length(x$optVariables), "features selected at iteration",
      x$optIter, "including:\n    ",
      paste(x$optVariables[1:min(length(x$optVariables), top)], 
            sep = "", collapse = ", "),
      if(length(x$optVariables) > top) "..." else "", 
      "\n")
  perf_dat <- subset(x$external, Iter == x$optIter)
  perf_dat <- perf_dat[!(names(perf_dat) %in% c("Iter", "Resample"))]
  perf <- colMeans(perf_dat)
  cat("   * external performance at this iteration is\n\n")
  ch_perf  <- format(perf, digits = digits, row.names = FALSE)
  ch_perf[1] <- paste("    ", ch_perf[1])
  print(ch_perf, quote = FALSE)
  cat("\n")
  
  invisible(x)
}

predict.safs <- function (object, newdata, ...) {
  newdata <- newdata[, object$optVariables, drop = FALSE]
  object$control$functions$pred(object$fit, newdata)  
}

safsControl <- function(functions = NULL,
                        method = "repeatedcv",
                        metric = NULL,
                        maximize = NULL,
                        number = ifelse(grepl("cv", method), 10, 25),
                        repeats = ifelse(grepl("cv", method), 1, 5),
                        verbose = FALSE,
                        returnResamp = "final",
                        p = .75,
                        index = NULL,
                        indexOut = NULL,
                        seeds = NULL,
                        holdout = 0,
                        improve = Inf,
                        allowParallel = TRUE) {
  if(!(method %in% c("cv", "boot", "repeatedcv", "LGOCV", "LOOCV")))
    stop('method should be one of: "cv", "boot", "repeatedcv", "LGOCV" or "LOOCV"')
  if(holdout < 0 | holdout >= 1) stop("'holdout' should be in [0, 1)")
  if(improve < 2 ) stop("'improve' should be >= 2")
  
  if(!is.null(metric)) {
    if(length(metric)  != 2)
      stop("'metric' should be a two-element named vector. See ?safsControl")
    if(is.null(names(metric)) || any(sort(names(metric)) != c("external", "internal")))
      stop("'metric' should have names 'internal' and 'external' See ?safsControl")
  }
  if(!is.null(maximize)) {
    if(length(maximize)  != 2)
      stop("'maximize' should be a two-element named vector. See ?safsControl")
    if(is.null(names(maximize)) || any(sort(names(maximize)) != c("external", "internal")))
      stop("'maximize' should have names 'internal' and 'external' See ?safsControl")
  }
  
  list(functions = if(is.null(functions)) caretFuncs else functions,
       method = method,
       metric = metric,
       maximize = maximize,
       number = number,
       repeats = repeats,
       returnResamp = returnResamp,
       verbose = verbose,
       p = p,
       index = index,
       indexOut = indexOut,
       seeds = seeds,
       holdout = holdout,
       improve = improve,
       allowParallel = allowParallel)
}

safs <- function (x, ...) UseMethod("safs")

"safs.default" <-
  function(x, y,
           iters = 10,
           differences = TRUE,
           safsControl = safsControl(), 
           ...) {
    startTime <- proc.time()
    funcCall <- match.call(expand.dots = TRUE)
    
    if(is.null(safsControl$metric)) 
      safsControl$metric <- rep(ifelse(is.factor(y), "Accuracy", "RMSE"), 2)
    if(is.null(safsControl$maximize)) 
      safsControl$maximize <- rep(ifelse(safsControl$metric == "RMSE", FALSE, TRUE), 2)
    if(is.null(names(safsControl$metric)))
      names(safsControl$metric) <- c("internal", "external")
    if(is.null(names(safsControl$maximize)))
      names(safsControl$maximize) <- c("internal", "external")
    
    if(nrow(x) != length(y)) stop("there should be the same number of samples in x and y")
    numFeat <- ncol(x)
    classLevels <- levels(y)
    
    if(is.null(safsControl$index)) 
      safsControl$index <- switch(tolower(safsControl$method),
                                  cv = createFolds(y, safsControl$number, returnTrain = TRUE),
                                  repeatedcv = createMultiFolds(y, safsControl$number, safsControl$repeats),
                                  loocv = createFolds(y, length(y), returnTrain = TRUE),
                                  boot =, boot632 = createResample(y, safsControl$number),
                                  test = createDataPartition(y, 1, safsControl$p),
                                  lgocv = createDataPartition(y, safsControl$number, safsControl$p))
    
    if(is.null(names(safsControl$index))) 
      names(safsControl$index) <- getFromNamespace("prettySeq", "caret")(safsControl$index)
    
    ## Create hold--out indicies
    if(is.null(safsControl$indexOut)){
      safsControl$indexOut <- lapply(safsControl$index,
                                     function(training, allSamples) allSamples[-unique(training)],
                                     allSamples = seq(along = y))
      names(safsControl$indexOut) <- getFromNamespace("prettySeq", "caret")(safsControl$indexOut)
    }
    
    if(!is.null(safsControl$seeds)) {
      if(length(safsControl$seeds) < length(safsControl$index) + 1)
        stop(paste("There must be at least", 
                   length(safsControl$index) + 1,
                   "random number seeds passed to safsControl"))
    } else {
      safsControl$seeds <- sample.int(100000, length(safsControl$index) + 1)
    }
    
    ## check summary function and metric
    testOutput <- data.frame(pred = sample(y, min(10, length(y))),
                             obs = sample(y, min(10, length(y))))
    
    if(is.factor(y))
      for(i in seq(along = classLevels)) testOutput[, classLevels[i]] <- runif(nrow(testOutput))
    
    test <- safsControl$functions$fitness_extern(testOutput, lev = classLevels)
    perfNames <- names(test)
    if(is.null(perfNames)) {
      warning(paste("The external fitness results should be a *named* vector;",
                    "new name(s) are",
                    paste(paste0("external", 1:length(test)), sep = "", collapse = ", ")),
              immediate. = TRUE)
      perfNames <- paste0("external", 1:length(test))
    }
    
    if(!(safsControl$metric["external"] %in% perfNames)) {
      warning(paste("The metric '", safsControl$metric["external"], "' is not created by the external summary function; '",
                    perfNames[1], "' will be used instead", sep = ""))
      safsControl$metric["external"] <- perfNames[1]
    }
    
    `%op%` <- getOper(safsControl$allowParallel && getDoParWorkers() > 1)
    #     sa_resampled <- external <- vector(mode = "list", length = length(safsControl$index))
    result <- foreach(i = seq(along = safsControl$index), .combine = "c", .verbose = FALSE, .errorhandling = "stop") %op% {   
      sa_select(x[safsControl$index[[i]],,drop=FALSE], 
                y[safsControl$index[[i]]], 
                funcs = safsControl$functions,
                sa_metric = safsControl$metric,
                sa_maximize = safsControl$maximize,
                iters = iters,
                sa_verbose = safsControl$verbose,
                testX = x[safsControl$indexOut[[i]],,drop=FALSE],
                testY = y[safsControl$indexOut[[i]]],
                sa_seed = safsControl$seeds[i],
                improve = safsControl$improve,
                Resample = names(safsControl$index)[i],
                holdout = safsControl$holdout,
                lvl = classLevels,
                ...)
    }
    
    ## TODO save only the parts you need inside of loop
    external <- result[names(result) == "external"]
    external <- do.call("rbind", external)
    rownames(external) <- NULL
    internal <- result[names(result) == "table"]
    internal <- do.call("rbind", internal)
    rownames(internal) <- NULL
    selected_vars <- result[names(result) == "final"]
    names(selected_vars) <- names(safsControl$index)
    if(differences) {
      diffs <- try(process_diffs(result[names(result) == "diffs"],
                                 colnames(x)),
                   silent = TRUE)
      if(class(diffs)[1] == "try-error") {
        diffs <- NULL
        warning("An error occured when computing the variable differences")
      }
    } else diffs <- NULL
    rm(result)
    
    if(safsControl$verbose) cat("+ final SA\n")
    
    if(safsControl$holdout > 0) {
      in_holdout <- createDataPartition(y, 
                                        p = safsControl$holdout, 
                                        list = FALSE)
      in_model <- seq(along = y)[-unique(in_holdout)]
    } else {
      in_model <- seq(along = y)
      in_holdout <- NULL
    }
    final_sa <- sa_select(x[in_model,,drop=FALSE], 
                          y[in_model], 
                          funcs = safsControl$functions,
                          sa_metric = safsControl$metric,
                          sa_maximize = safsControl$maximize,
                          iters = iters,
                          sa_verbose = safsControl$verbose,
                          testX = if(!is.null(in_holdout)) x[in_holdout,,drop=FALSE] else NULL,
                          testY = if(!is.null(in_holdout)) y[in_holdout] else NULL,
                          sa_seed = safsControl$seeds[length(safsControl$seeds)],
                          improve = safsControl$improve,
                          lvl = classLevels,
                          ...)
    
    averages <- ddply(external, .(Iter), 
                      function(x, nms) {
                        apply(x[, perfNames, drop = FALSE], 2, mean)
                      },
                      nms = perfNames)
    
    if(!is.null(safsControl$functions$selectIter)) {
      best_index <- safsControl$functions$selectIter(averages, 
                                                     metric = safsControl$metric["external"], 
                                                     maximize = safsControl$maximize["external"])
      best_iter <- averages$Iter[best_index]
      best_vars <- colnames(x)[final_sa$subsets[[best_index]]]
    } else { 
      best_index <- if(safsControl$maximize["external"]) 
        which.max(averages[,safsControl$metric["external"]]) else 
          which.min(averages[,safsControl$metric["external"]])
      best_iter <- averages$Iter[best_index]
      best_vars <- colnames(x)[final_sa$subsets[[best_index]]]
    }
    if(safsControl$verbose) cat("+ final model\n")
    
    fit <- safsControl$functions$fit(x[, best_vars, drop=FALSE], y, lev = lvls, last = TRUE, ...)
    
    endTime <- proc.time()
    res <- list(fit = fit,
                sa = final_sa,
                external = external, 
                internal = internal,
                all_vars  = colnames(x),
                resampled_vars = selected_vars,
                averages = averages,
                iters = iters,
                optVariables = best_vars,
                optIter = best_iter,
                control = safsControl,
                dims = dim(x),
                differences = diffs,
                perfNames = perfNames,
                auto = TRUE,
                the_dots = list(...),
                times = list(everything = endTime - startTime),
                levels = if(is.factor(y)) classLevels else NULL)
    
    class(res) <- "safs"
    res
  }

safs_initial <- function (vars, prob = .20, ...)  {
  sort(sample.int(vars, size = floor(vars*prob)+1))
}

safs_perturb <- function(x, vars, number = floor(vars*.01) + 1) {
  bin <- index2vec(x, vars)
  change <- sample(seq(along = bin), size = number)
  bin[change] <- ifelse(bin[change] == 1, 0, 1)
  sort(which(bin == 1))
}

safs_prob <- function(old, new, iteration = 1) {
  if(new < old) return(1)
  ediff <- as.vector(old - new)
  ediff <- ediff/abs(old)
  exp(ediff*iteration)
}

sa_wrapper <- function(ind, x, y, funcs, holdoutX, holdoutY, testX, testY, sa_metric, sa_maximize, lvl = lvl, last = FALSE, ...) {
  mod <- funcs$fit(x[, ind, drop=FALSE], y, lev = lvl, last = last,...)
  internal <- funcs$fitness_intern(mod, 
                                   x = if(!is.null(holdoutX)) holdoutX[, ind, drop=FALSE] else x[, ind, drop=FALSE], 
                                   y = if(!is.null(holdoutY)) holdoutY else y, 
                                   p = ncol(x))
  if(!is.null(testX)) {
    modelPred <- funcs$pred(mod, testX[, ind, drop=FALSE])
    if(is.data.frame(modelPred) | is.matrix(modelPred)) {
      if(is.matrix(modelPred)) modelPred <- as.data.frame(modelPred)
      modelPred$obs <- testY
      modelPred$Size <- length(ind)
    } else modelPred <- data.frame(pred = modelPred, obs = testY, Size = sum(ind == 1))
    external <- funcs$fitness_extern(modelPred, lev = levels(testY))  
    if(is.null(names(external))) {
      names(external) <- paste0("external", 1:length(external))
    }
  } else external <- NULL
  
  if(sa_maximize["internal"]) 
    internal[sa_metric["internal"]] <- -internal[sa_metric["internal"]]
  
  list(internal = internal, 
       external = external)
}

###################################################################
##

sa_select <- function(x, y,  
                      ## testX, testY: optional holdout data for computing
                      ## the fitness function
                      testX = NULL, testY = NULL, 
                      iters = 20,
                      funcs = NULL, 
                      sa_metric = NULL,
                      sa_maximize = TRUE, 
                      sa_verbose = TRUE, 
                      holdout = 0,
                      sa_seed = NULL,
                      improve = 10, 
                      lvl = NULL,
                      Resample = "",
                      ...) {
  sa_func_check(funcs)
  if(!is.null(sa_seed)) set.seed(sa_seed[1])
  dig <- options()$digits
  
  if(holdout > 0) {
    in_holdout <- createDataPartition(y, 
                                      p = holdout, 
                                      list = FALSE)
    holdout_x <- x[in_holdout,,drop = FALSE]
    holdout_y <- y[in_holdout]
    x <- x[-in_holdout,,drop = FALSE]
    y <- y[-in_holdout]    
  } else {
    holdout_x <- NULL
    holdout_y <- NULL
  }  
  
  
  p <- ncol(x)
  cycle <- 1
  since_restart <- 0
  since_improve <- 0
  last_improve <- 0
  
  restarts <- 1
  subsets <- vector(mode = "list", length = iters)
  internal <- data.frame(Best = rep(0*NA, iters),
                         Note = "",
                         Random = runif(iters),
                         Prob = rep(1, iters),
                         Iter = 1:(iters),
                         Cycle = rep(0*NA, iters),
                         SinceRestart = rep(0*NA, iters),
                         Size = rep(0*NA, iters),
                         Similarity = rep(0*NA, iters),
                         Similarity_B = rep(0*NA, iters),
                         stringsAsFactors = FALSE)
  external <- if(!is.null(testX)) data.frame(Iter = 1:(iters)) else NULL
  
  for(i in 1:iters){
    if(i == 1)  {
      if(is.function(funcs$initial)) {
        best_subset <- new_subset <- current_subset <- funcs$initial(vars = p)
      } else {
        if(max(funcs$initial) > p)
          stop(paste("The initial vector uses columns not in the data"))
        best_subset <- new_subset <- current_subset <- funcs$initial
      }
    } else  {
      new_subset <- funcs$perturb(current_subset, vars = p)
    }
    
    if(length(new_subset) == 0) new_subset <- sample.int(p, 1)
    subsets[[i]] <- new_subset
    if(i > 1) {
      internal$Similarity[i] <- jack_sim(index2vec(subsets[[i-1]], p), 
                                         index2vec(subsets[[i  ]], p))
      internal$Similarity_B[i] <- jack_sim(index2vec(best_subset, p), 
                                           index2vec(new_subset, p))
    }
    
    since_restart <- since_restart + 1
    internal$SinceRestart[i] <- since_restart
    new_obj <- sa_wrapper(ind = new_subset, 
                          x = x, y = y, 
                          funcs, 
                          holdoutX = holdout_x, holdoutY = holdout_y,
                          testX = testX, testY = testY, 
                          sa_metric = sa_metric, 
                          sa_maximize = sa_maximize, 
                          lvl = lvl,
                          last = Resample == "",
                          ...) 
    
    ## Use the initial results to setup containers for
    ## the remaining iterations
    if(i == 1) {  ##TODO check for name and modify if needed as with external
      k <- length(new_obj$internal)
      perf_names <- names(new_obj$internal)
      for(new_var in perf_names) internal[,new_var] <- NA
      nr <- ncol(internal)
      internal[1, (nr-k+1):nr] <- new_obj$internal
      if(!is.null(testX)) {
        for(new_var in names(new_obj$external)) external[,new_var] <- NA
        external[1, -1] <- new_obj$external
      }
    } else {
      internal[i, (nr-k+1):nr] <- new_obj$internal
      if(!is.null(testX)) external[i, -1] <- new_obj$external
    }
    
    if(sa_verbose){
      if(i > 1) {
        cat(Resample, " ", format(1:iters)[i], " ",
            if(sa_maximize["internal"]) 
              signif(-internal$Best[i-1], digits = dig) else 
                signif(internal$Best[i-1], digits = dig),
            "->" , 
            if(sa_maximize["internal"]) 
              signif(-new_obj$internal[sa_metric["internal"]], digits = dig) else 
                signif(new_obj$internal[sa_metric["internal"]], digits = dig),
            change_text(best_subset, new_subset, p), 
            sep = "") 
      } else {
        cat(Resample, " ", format(1:iters)[i], " ",
            if(sa_maximize["internal"]) 
              signif(-new_obj$internal[sa_metric["internal"]], digits = dig) else 
                signif(new_obj$internal[sa_metric["internal"]], digits = dig),
            " (" , length(best_subset), ")\n",
            sep = "")        
      }
    }    
    
    internal$Size[i] <- length(new_subset)  
    internal$Cycle[i] <- cycle
    if(i == 1 || new_obj$internal[sa_metric["internal"]] < internal$Best[i-1]) {
      current_subset <- new_subset
      best_subset <- new_subset
      internal$Best[i] <- new_obj$internal[sa_metric["internal"]]
      internal$Note[i] <- "Improved"
      last_improve <- i
      since_improve <- 0
      if(sa_verbose & i > 1) cat(" *\n")
    } else {
      if(i > 1) {
        internal$Prob[i] <- funcs$prob(old = internal$Best[i-1], 
                                       new = new_obj$internal[sa_metric["internal"]],
                                       iteration = since_restart)  
        since_improve <- since_improve + 1
        if(sa_verbose) 
          cat(" ", signif(internal$Prob[i], digits = dig), " ")
      } else internal$Prob[i] <- 1
      
      if(internal$Prob[i] > internal$Random[i]) {
        current_subset <- new_subset
        internal$Best[i] <- internal$Best[i-1]
        internal$Note[i] <- "Accepted"
        if(sa_verbose & i > 1) cat("A\n")
      } else {
        internal$Obj[i] <- internal$Obj[i-1]
        internal$Best[i] <- internal$Best[i-1]
        internal$Note[i] <- "Discarded"
        if(sa_verbose & i > 1) cat("\n")
      }
    }    
    
    if(since_improve == improve) {
      internal$Note[i] <- "Restart"
      current_subset <- subsets[[last_improve]]
      cycle <- cycle + 1
      since_restart <- 0
      since_improve <- 0
      if(sa_verbose) 
        cat(Resample, "restart, goto iter", last_improve, "\n")
    }
  }
  if(sa_maximize["internal"]) {
    internal[, sa_metric["internal"]] <- -internal[, sa_metric["internal"]]
    internal$Best <- -internal$Best 
  }
  mod <- funcs$fit(x[, best_subset, drop=FALSE], y, lev = lvl, last = TRUE, ...)
  if(Resample != "") internal$Resample <- Resample
  if(Resample != "" && !is.null(testX)) external$Resample <- Resample
  
  diffs <- try(get_fitness_differences(colnames(x), 
                                       subsets, 
                                       external[, !(names(external) %in% sa_external_names), drop = FALSE]),
               silent = TRUE)
  if(class(diffs)[1] == "try-error") diffs <- NULL
  list(table = internal,
       subsets = subsets, 
       external = external,
       final = names(x)[best_subset],
       fit = mod,
       diffs = diffs)
}


###################################################################
##

plot.safs <- function(x, 
                      metric = x$control$metric["external"], 
                      estimate = c("internal", "external"), 
                      output = "ggplot",
                      ...) {
  int_names <- names(x$internal)[!(names(x$internal) %in% sa_internal_names)]
  ext_names <- names(x$external)[!(names(x$external) %in% sa_external_names)]
  common <- intersect(int_names, ext_names)
  both_estimates <- length(estimate) == 2  && all(sort(estimate) == c("external", "internal"))
  
  if(both_estimates){
    if(!metric %in% common) stop(paste("'", metric, "' not computed in both estimates"))
    tmp_e <- x$external[, c("Iter", "Resample", common)]
    tmp_e$Estimate <- "External"
    tmp_i <- x$internal[, c("Iter", "Resample", common)]
    tmp_i$Estimate <- "Internal"
    plot_dat <- rbind(tmp_e, tmp_i)
  } else {
    if("internal" %in% estimate) {
      if(!metric %in% int_names) stop(paste("'", metric, "' not computed internally"))
      plot_dat <- x$internal[, c("Iter", "Resample", int_names)]
    }
    if("external" %in% estimate) {
      if(!metric %in% int_names) stop(paste("'", metric, "' not computed externally"))
      plot_dat <- x$external[, c("Iter", "Resample", ext_names)]
    }
  }
  if(output == "data") out <- plot_dat
  plot_dat <- if(both_estimates) 
    ddply(plot_dat, c("Iter", "Estimate"),
          function(x) c(Mean = mean(x[, metric]))) else 
            ddply(plot_dat, c("Iter"),
                  function(x) c(Mean = mean(x[, metric])))                  
  
  if(output == "ggplot") {
    out <- if(both_estimates) 
      ggplot(plot_dat, aes(x = Iter, y = Mean, color = Estimate)) + geom_point() else 
        ggplot(plot_dat, aes(x = Iter, y = Mean)) + geom_point() 
    out <- out + xlab("Iteration")
    
  } 
  if(output == "lattice") {
    out <- if(both_estimates) 
      xyplot(Mean ~ Iter, data = plot_dat, groups = Estimate, ...) else 
        xyplot(Mean ~ Iter, data = plot_dat, ...)
    out <- update(out, xlab = "Iteration")
  }
  out
}

###################################################################
##
  
caretSA <- list(fit = function(x, y, lev = NULL, last = FALSE, ...) train(x, y, ...),
                pred = function(object, x) {
                  tmp <- predict(object, x)
                  if(object$control$classProbs) {
                    out <- cbind(data.frame(pred = tmp),
                                 as.data.frame(predict(object, x, type = "prob")))
                  } else out <- tmp
                  out
                },
                fitness_intern = function(object, x, y, maximize, p){
                  perf_val <- getTrainPerf(object)
                  perf_val <- perf_val[names(perf_val) != "method"]
                  perf_val <- unlist(perf_val)
                  names(perf_val) <- gsub("Train", "", names(perf_val))
                  perf_val
                },
                fitness_extern = defaultSummary,
                initial = safs_initial,
                perturb = safs_perturb,
                prob = safs_prob,
                selectIter = best)

treebagSA <- list(fit = function(x, y, lev = NULL, last = FALSE, ...) {
  library(ipred)
  ipredbagg(y, x, ...)
},
pred = function(object, x) {
  tmp <- predict(object, x)
  if(is.factor(object$y)) {
    out <- cbind(data.frame(pred = tmp),
                 as.data.frame(predict(object, x, type = "prob")))
  } else out <- tmp
  out
},
fitness_intern = function(object, x, y, maximize, p)
  ipredStats(object)[1:2],
fitness_extern = defaultSummary,
initial = safs_initial,
perturb = safs_perturb,
prob = safs_prob,
selectIter = best)

rfSA <-  list(fit = function(x, y, lev = NULL, last = FALSE, ...) {
  library(randomForest)
  randomForest(x, y, ...)
},
pred = function(object, x) {
  tmp <- predict(object, x)
  if(is.factor(object$y)) {
    out <- cbind(data.frame(pred = tmp),
                 as.data.frame(predict(object, x, type = "prob")))
  } else out <- tmp
  out
},
fitness_intern = function(object, x, y, maximize, p) rfStats(object),
fitness_extern = defaultSummary,
initial = safs_initial,
perturb = safs_perturb,
prob = safs_prob,
selectIter = best)


update.safs <- function(object, iter, x, y, ...) {
  iter <- iter[1]
  if(iter > length(object$sa$subsets))
    stop(paste("iter must be less than", length(object$sa$subsets)))
  if(is.null(x) | is.null(y))
    stop("the original training data is needed to refit the model")
  args <- list(x = x[, object$sa$subsets[[iter]], drop=FALSE], 
               y = y, lev = object$levels, last = TRUE)
  if(length(object$the_dots) > 0) args <- c(args, object$the_dots)
  if(object$control$verbose)
    cat("Refitting model to use", length(object$sa$subsets[[iter]]),
        "predictors from iteration", iter, "\n")
  object$fit <- do.call(object$control$functions$fit, args)
  object$auto <- FALSE
  object$optVariables <- colnames(x)[object$sa$subsets[[iter]]]
  object$optIter <- iter
  object  
}

"varImp.safs" <- function(object, 
                          metric = object$control$metric["external"], 
                          maximize = object$control$maximize["external"],
                          ...) {
  
  if(is.null(object$differences)) 
    stop("must have used `differences = TRUE`")
  out <- object$differences[,metric, drop = FALSE]
  rownames(out) <- as.character(object$differences$Variable)
  if(!maximize) out[, metric, drop = FALSE] <- -out[, metric, drop = FALSE]
  out <- out[order(-out[, metric]),, drop = FALSE]
  out
}





