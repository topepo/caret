###################################################################
## This code is used to document the results of regression tests
## from the `caret` package. See
##
##   https://github.com/topepo/caret/tree/master/RegressionTests
## 
## The code reads in two sets of results from different runs of
## the regression tests, parses the results, compares them, and
## writes out markdown files for each test set. 

###################################################################
## Misc functions

## Create a nicely formatted list of objects
lists <- function (x, period = FALSE)  {
  if (!is.character(x)) 
    x <- as.character(x)
  numElements <- length(x)
  out <- if (length(x) > 0) {
    switch(min(numElements, 3), x, paste(x, collapse = " and "), 
           {
             x <- paste(x, c(rep(",", numElements - 2), " and", 
                             ""), sep = "")
             paste(x, collapse = " ")
           })
  }
  else ""
  if (period) 
    out <- paste(out, ".", sep = "")
  out
}


get_time <- function(x) {
  x <- x$times$everything[3]
  if(x > 60) {
    if(x > 60*60) {
      x <- paste0(round(x/60*20, 2), "h")
    } else x <- paste0(round(x/60, 2), "m")
  } else {
    x <- paste0(round(x, 2), "s")
  }
  x
}

class_list <- function(x) 
  lists(paste0("`", x, "`"))


vec2mat <- function(x, ncol = 2) {
  sizeDelta <- length(x)%%ncol
  if (sizeDelta > 0) out <- c(x, rep("", ncol - length(x)%%ncol)) else out <- x
  out <- matrix(out, ncol = ncol, byrow = TRUE)
  colnames(out) <- rep("", ncol(out))
  rownames(out) <- rep("", nrow(out))
  out
}

## For two paths, determine what tests have common results
match_objects <- function(old_path, new_path, verbose = TRUE) {
  old_rda <- list.files(old_path, pattern = "\\.RData$")
  new_rda <- list.files(new_path, pattern = "\\.RData$")
  old_rda <- gsub("\\.RData$", "", old_rda)
  new_rda <- gsub("\\.RData$", "", new_rda)  
  both <- intersect(old_rda, new_rda)
  everything <- unique(c(old_rda, new_rda))
  everything <- everything[everything != ""]
  orph <- everything[!(everything %in% both)]
  orph <- orph[orph != ""]
  if(verbose) {
    cat("  ", length(old_rda), " objects found from the old version,\n",
        "  ", length(new_rda), " objects found from the new version,\n", 
        "  ", length(both), " common tests,\n",
        "  ", length(orph), " orphans:\n",
        sep = "")
    print(vec2mat(orph))
  }
  both
}

## Read in the objects in the RData file and make them into a list
obj2list <- function(path){
  load(path)
  test_obj <- ls(pattern = "^test_")
  tests <- vector(mode = "list", length = length(test_obj))
  for(i in seq(along = tests)) tests[[i]] <- get(test_obj[[i]])
  names(tests) <- gsub("^test_", "", test_obj)
  time <- format(timestamp, "%Y_%m_%d_%H_%M")
  session <- sInfo
  if(!is.character(time)) time <- NA
  list(tests = tests, 
       times = time, 
       session = session,
       elapsed = difftime(timestamp_end, timestamp, units =  "s"))
}

## Compare all the common objects from the RData files
obj_compare <- function(old_path, new_path, verbose = TRUE) {
  old_all <- obj2list(old_path)
  new_all <- obj2list(new_path) 
  
  old_res <- old_all$tests
  new_res <- new_all$tests
  
  library(lubridate)
  
  if(!is.na(old_all$times) | !is.na(new_all$times)) {
    times <- list(old = ymd(substring(old_all$times, 1, 10)),
                  new = ymd(substring(new_all$times, 1, 10)))
  }
  
  cat("Testing Information:\n---------\n\n")
  
  cat("Old:\n\n")
  
  cat(" * ", old_all$session$platform, "\n", 
      " * ", old_all$session$R.version$version.string, "\n", 
      " * ", print_versions(old_all$session), "\n",
      sep = "")

  if(!is.na(old_all$times) | !is.na(new_all$times)) {
    cat(" * tested on ", as.character(times$old), 
        " at ", gsub("_", ":", substring(old_all$time, 12, 16)), 
        ". \n * total test time: ", 
        round(old_all$elapsed, 1),
        "s\n",
        sep = "")
  }
  cat("\n\n")
  
  cat("New:\n\n")
  
  cat(" * ", new_all$session$platform, "\n", 
      " * ", new_all$session$R.version$version.string, "\n", 
      " * ", print_versions(new_all$session), "\n",
      sep = "")
  
  if(!is.na(new_all$times) | !is.na(new_all$times)) {
    cat(" * tested on ", as.character(times$new), 
        " at ", gsub("_", ":", substring(new_all$time, 12, 16)), 
        ". \n * total test time: ", 
        round(new_all$elapsed, 1),
        "s\n",
        sep = "")
  }
  cat("\n\n")
  
  cat("Results:\n---------\n\n")  
  
  both <- intersect(names(old_res), names(new_res))
  
  # print(table(unlist(lapply(old_res, function(x) class(x)[1]))))
  
  for(i in both) {
    cat("**Test Case**: `", i, "`\n\n", sep = "")
    classes <- class(old_res[[i]])
    test_compare(old_res[[i]], new_res[[i]])
  }
}


metric_compare <- function(both, metric) {
  old_perf <- both[, paste0(metric, "_old")]
  new_perf <- both[, paste0(metric, "_new")]
  all.equal(old_perf, new_perf)
}


train_compare <- function(old_obj, new_obj) {
  old_res <- old_obj$results
  new_res <- new_obj$results
  
  pNames <- new_obj$perfNames
  
  old_res <- old_res[,!grepl("SD$", names(old_res)),drop = FALSE]
  new_res <- new_res[,!grepl("SD$", names(new_res)),drop = FALSE]
  
  names(new_res)[names(new_res) %in% new_obj$perfNames] <- 
    paste0(names(new_res)[names(new_res) %in% new_obj$perfNames], "_new")
  names(old_res)[names(old_res) %in% old_obj$perfNames] <- 
    paste0(names(old_res)[names(old_res) %in% old_obj$perfNames], "_old")
  both_res <- merge(new_res, old_res, all = TRUE)
  
  all_true <- TRUE
  
  for(i in pNames) {
    res <- metric_compare(both_res, i)
    all_true <- all_true & isTRUE(res)
    if(isTRUE(res)) {
      cat(" * _Equal results for ", i, "_\n", sep = "")
    } else {
      cat(" * ***UNequal results for ", i, "***:\n\n", sep = "")
      cat("<pre>\n")
      describe_diffs(both_res, i, pNames)
      cat("</pre>\n")
      cat("\n")
    }
  }
}

print_versions <- function(x, exclude = c("ggplot2", "lattice")) {
  x$otherPkgs <- x$otherPkgs[!(names(x$otherPkgs) %in% exclude)]
  
  lab <- lapply(x$otherPkgs, function(x) paste0("`", x$Package, "` (", x$Version, ")"))
  paste(lab[order(tolower(lab))], collapse = ", ", sep = "")
}

## When there is a difference in numeric results, characterize them
describe_diffs <- function(both, test_metric, all_metrics, digits = 4, buff = "   ") {
  old_perf <- both[, paste0(test_metric, "_old")]
  new_perf <- both[, paste0(test_metric, "_new")]
  old_na <- sum(is.na(old_perf))
  new_na <- sum(is.na(new_perf))
  diffs <- (new_perf-old_perf)/old_perf*100
  both$Diff <- round(diffs, 1)
  if(all(is.na(diffs))) { 
    cat(buff, "no pairs of results without missing values\n")
    print(both)
  } else {
    if(max(diffs, na.rm = TRUE) <= .1) {
      cat(buff, "%differences < 0.1%\n")
    } else {
      diffs <- matrix(c(mean(diffs, na.rm = TRUE), 
                        min(diffs, na.rm = TRUE),
                        max(diffs, na.rm = TRUE)),
                      ncol = 1)
      rownames(diffs) <- paste0(buff, buff, c("mean", "min", "max"))
      colnames(diffs) <- ""
      corrs <- cor(new_perf, old_perf, use = "pairwise.complete.obs")
      if(old_na > 0 | new_na > 0) {
        cat(buff, " ", old_na, " missing values in old\n", 
            buff, " ", new_na, " missing values in new\n", 
            sep = "")
      }
      cat(buff, "%differences (n-o):")
      print(diffs, digits = digits)
      cat(buff, "correlation: ", signif(corrs, digits = digits), "\n")
      if(length(old_perf) <= 10) {
        cat("\n")
        discards <- all_metrics[all_metrics != test_metric]
        old_dis <- paste0(discards, "_old")
        new_dis <- paste0(discards, "_new")
        discards <- c(old_dis, new_dis)
        tmp <- both[, !(names(both) %in% discards)]
        tmp$Flag <- ifelse(old_perf == new_perf, " ", "*")
        tmp$Flag[is.na(tmp$Flag)] <- " "
        print(tmp, digits = digits, row.names = FALSE)
        cat("\n")
      }
    }
  }
  invisible(NULL)
}

###################################################################
## A class to write out results for different object types

test_compare <- function (older, ...) UseMethod("test_compare")

test_compare.train <- function(older, newer, ...) {
  cat("Object class(es): ", class_list(class(older)), "\n\n", sep = "")
  cat("Model Configuration:\n\n")
  
  train_markdown(older)
  
  cat("\n\nExecution times: (old) ",  get_time(older),
      " (new) ",  get_time(newer),
      "\n\n", sep = "")
  
  cat("Test Results:\n\n")      
  
  train_compare(older, newer)
  cat("\n")
}

test_compare.character <- function(older, newer, ...) {
  cat("Object class(es): ", class_list(class(older)), "\n\n", sep = "")
  
  res <- all.equal(older, newer)
  if(isTRUE(res)) {
    cat(" * _Equal results_\n", sep = "")
  } else {
    agg <- older == newer
    cat(" * ***UNequal results***:\n", sep = "")
    cat("<pre>\n")
    cat("   Agreement: ", 
        round(mean(agg)*100, 1), "% (", sum(agg), "/", length(agg), ")\n",
        sep = "")
    
    print(table(older, newer))
    cat("</pre>\n")
  }
  cat("\n")
}

test_compare.factor <- test_compare.character

test_compare.numeric <- function(older, newer, buff = "   ", digits = 3, ...) {
  cat("Object class(es): ", class_list(class(older)), "\n\n", sep = "")
  res <- all.equal(as.vector(older), as.vector(newer))
  old_na <- sum(is.na(older))
  new_na <- sum(is.na(newer))
  if(!isTRUE(res)) {
    cat(" * ***UNequal results***:\n", sep = "")
    diffs <- (newer-older)/older*100
    if(all(is.na(diffs))) { 
      cat(buff, "no pairs of results without missing values\n")
      # print(both)
    } else {
      if(max(diffs, na.rm = TRUE) <= .1) {
        cat(buff, "%differences < 0.1%\n")
      } else {
        cat("<pre>\n")
        diffs <- matrix(c(mean(diffs, na.rm = TRUE), 
                          min(diffs, na.rm = TRUE),
                          max(diffs, na.rm = TRUE)),
                        ncol = 1)
        rownames(diffs) <- paste0(buff, buff, c("mean", "min", "max"))
        colnames(diffs) <- ""
        corrs <- cor(newer, older, use = "pairwise.complete.obs")
        if(old_na > 0 | new_na > 0) {
          cat(buff, " ", old_na, " missing values in old\n", 
              buff, " ", new_na, " missing values in new\n", 
              sep = "")
        }
        cat(buff, "%differences (n-o):")
        print(diffs, digits = digits)
        cat(buff, "correlation: ", signif(corrs, digits = digits), "\n")
        cat("</pre>\n")
      }
    }
  } else {
    cat(" * _Equal results_\n", sep = "")
  }
  cat("\n")
}

test_compare.data.frame <- function(older, newer, buff = "   ", digits = 3, ...) {
  cat("Object class(es): ", class_list(class(older)), "\n\n", sep = "")
  
  res <- all.equal(older, newer)
  if(!isTRUE(res)) {
    cat(" * ***UNequal results***: differences (o-n):\n", sep = "")
    cat("<pre>\n")
    print(summary(older-newer))
    cat("</pre>\n")
  } else {
    cat(" * _Equal results_\n", sep = "")
  }
  cat("\n")
}


test_compare.varImp.train <- function(older, newer, buff = "   ", digits = 3, ...) {
  cat("Object class(es): ", class_list(class(older)), "\n\n", sep = "")
  res <- all.equal(older$importance, newer$importance)
  if(!isTRUE(res)) {
    cat(" * ***UNequal results***: differences (o-n):\n", sep = "")
    cat("<pre>\n")
    print(summary(older$importance-newer$importance))
    cat("</pre>\n")
  } else {
    cat(" * _Equal results_\n", sep = "")
  }
  cat("\n")
}

test_compare.rfe <- function(older, newer, ...) {
  cat("Object class(es): ", class_list(class(older)), "\n\n", sep = "")
  cat("Model Configuration:\n\n")
  
  rfe_markdown(older)
  
  cat("\n\nExecution times: (old) ",  get_time(older),
      " (new) ",  get_time(newer),
      "\n\n", sep = "")
  
  cat("Test Results:\n\n")      
  
  old_res <- older$results
  new_res <- newer$results
  
  pNames <- newer$perfNames
  
  old_res <- old_res[,!grepl("SD$", names(old_res)),drop = FALSE]
  new_res <- new_res[,!grepl("SD$", names(new_res)),drop = FALSE]
  
  names(new_res)[names(new_res) %in% newer$perfNames] <- 
    paste0(names(new_res)[names(new_res) %in% newer$perfNames], "_new")
  names(old_res)[names(old_res) %in% older$perfNames] <- 
    paste0(names(old_res)[names(old_res) %in% older$perfNames], "_old")
  both_res <- merge(new_res, old_res, all = TRUE)
  
  all_true <- TRUE
  
  for(i in pNames) {
    res <- metric_compare(both_res, i)
    all_true <- all_true & isTRUE(res)
    if(isTRUE(res)) {
      cat(" * _Equal results for ", i, "_\n", sep = "")
    } else {
      cat(" * ***UNequal results for ", i, "***:\n", sep = "")
      cat("<pre>\n")
      describe_diffs(both_res, i, pNames)
      cat("</pre>\n")
      cat("\n")
    }
  }
  cat("\n")
}

test_compare.sbf <- function(older, newer, ...) {
  cat("Object class(es): ", class_list(class(older)), "\n\n", sep = "")
  cat("Model Configuration:\n\n")
  
  sbf_markdown(older)
  
  cat("\n\nExecution times: (old) ",  get_time(older),
      " (new) ",  get_time(newer),
      "\n\n", sep = "")
  
  cat("Test Results:\n\n")      
  
  old_res <- older$results
  new_res <- newer$results
  
  pNames <- names(older$results)
  pNames <- pNames[!grepl("SD$", pNames)]
  
  old_res <- old_res[,!grepl("SD$", names(old_res)),drop = FALSE]
  new_res <- new_res[,!grepl("SD$", names(new_res)),drop = FALSE]
  
  names(new_res)[names(new_res) %in% pNames] <- 
    paste0(names(new_res)[names(new_res) %in% pNames], "_new")
  names(old_res)[names(old_res) %in% pNames] <- 
    paste0(names(old_res)[names(old_res) %in% pNames], "_old")
  both_res <- merge(new_res, old_res, all = TRUE)
  
  all_true <- TRUE
  
  for(i in pNames) {
    res <- metric_compare(both_res, i)
    all_true <- all_true & isTRUE(res)
    if(isTRUE(res)) {
      cat(" * _Equal results for ", i, "_\n", sep = "")
    } else {
      cat(" * ***UNequal results for ", i, "***:\n", sep = "")
      cat("<pre>\n")
      describe_diffs(both_res, i, pNames)
      cat("</pre>\n")
      cat("\n")
    }
  }
  cat("\n")
}

###################################################################
## Simple markdown lists that describe model types

rfe_markdown <- function(x) {
  library(caret)
  
  cat(" * Resampling: ", caret:::resampName(x),"\n", sep = "") 
  cat(" * Subset sizes: ", paste(x$results$Variables, collapse = ", ", sep = ""),"\n", sep = "") 
}

sbf_markdown <- function(x) {
  library(caret)
  
  cat(" * Resampling: ", caret:::resampName(x),"\n", sep = "") 
  if(any(class(x) == "sbf.formula")) 
    cat(" * Formula method\n") else cat(" * Non-formula method\n")
}

train_markdown <- function(x) {
  library(caret)
  
  if(any(class(x) == "train.formula")) 
    cat(" * Formula method\n") else cat(" * Non-formula method\n")
  cat(" * Resampling: ", caret:::resampName(x),"\n", sep = "") 
  if(x$control$search == "grid") 
    cat(" * Grid search\n")  else cat(" * Random search\n")
  cat(" *", capture.output(caret:::pp_list(x$preProc$method)), "\n")
  cat(" * ", nrow(x$results), " tuning parameter",
      ifelse(nrow(x$results) == 1, " combination was", " combinations were"),
      " evaluated\n", sep = "") 
}

get_times <- function(old_stuff, new_stuff){
  
  load(old_stuff)
  res <- data.frame(test = gsub("\\.RData", "", basename(old_stuff)), 
                    old = as.numeric(difftime(timestamp_end, timestamp, units =  "s")))
  rm_list <- ls()
  rm_list <- rm_list[!(rm_list %in% c("res", "rm_list", "new_stuff"))]
  rm(list = rm_list)
  load(new_stuff)
  res$new <- as.numeric(difftime(timestamp_end, timestamp, units =  "s"))
  res
}



