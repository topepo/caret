setwd("~/tmp")

##############################################################

Old <- "2018_03_10_21__6.0-79"
New <- "2018_05_25_22__6.0-80"

oldResults <- list.files(file.path(getwd(), Old), pattern = "RData")
newResults <- list.files(file.path(getwd(), New), pattern = "RData")

oldOrphan <- oldResults[!(oldResults %in% newResults)]
newOrphan <- newResults[!(newResults %in% oldResults)]

common <- intersect(oldResults, newResults)

checkModels <- function(model, opath, npath){
  for(i in model) {
    thisMod <- gsub(".RData", "", i, fixed = TRUE)
    cat("############################################################\n")
    cat(thisMod, "\n")
    rlt <- checkResults(i,opath, npath)  
    if(!is.null(rlt))  print(rlt) else print(TRUE)
    cat("\n")
    rm(rlt)
  }
}

getObj <- function(filename){
  load(filename)
  testObj <- ls(pattern = "^test")
  testResults <- list()
  for(i in seq(along = testObj)) {
    tmp <- get(testObj[i])
    if(!is.null(tmp)) {
      testResults <- c(testResults, list(tmp))
      names(testResults)[length(testResults)] <- testObj[i]
      rm(tmp)
    }
  }
  testResults
}


checkResults <- function(model, opath, npath){
  oldResults <- getObj(file.path(getwd(), opath, model))  
  newResults <- getObj(file.path(getwd(), npath, model))   
  commonObj <- intersect(names(newResults), names(oldResults))
  
  testResults <- vector(mode = "list", length = length(commonObj))
  names(commonObj) <- commonObj
  for(i in commonObj) {
    cat("\n", i, "\n\n")
    if((class(newResults[[i]])[1] == class(oldResults[[i]])[1])) {
      if(!is.null(newResults[[i]]) & !is.null(oldResults[[i]])) {
        if(class(newResults[[i]])[1] == "train") {
          checkTrain(oldResults[[i]], newResults[[i]])
        } else {
          testResults[[i]] <- all.equal(newResults[[i]], 
                                        oldResults[[i]],
                                        tolerance = 0.001)
          print(testResults[[i]])
        }
      } else cat("skipping due to NULL", i, "\n") 
    } else cat("skipping due to conflicting classes", i, "\n")
  }
}

notEqual <- function(x) if(class(x)[1] != "logical" || !x) TRUE else FALSE


checkTrain <- function(Old, New = NULL) {
  if(!is.null(Old) & !is.null(New)) {
    oldRes <- Old$results[, !grepl("SD$", names(Old$results))]
    newRes <- New$results[, !grepl("SD$", names(New$results))]
    param <- gsub("^\\.", "", names(Old$bestTune))
    if(Old$method == "C5.0Cost") {
      names(oldRes)[names(oldRes) == "Cost"] <- "cost"
      param[param == "Cost"] <- "cost"   
    }
    pNames <- Old$perfNames
    
    names(oldRes)[names(oldRes) %in% pNames] <- paste("Old_", 
                                                      names(oldRes)[names(oldRes) %in% pNames],
                                                      sep = "")
    names(newRes)[names(newRes) %in% pNames] <- paste("New_", 
                                                      names(newRes)[names(newRes) %in% pNames],
                                                      sep = "")
    both <- merge(oldRes, newRes, all = !(Old$method %in% c("C5.0Cost", "M5Rules", "leapSeq", "leapBackward", "leapForward")))
    for(i in pNames) {
      cat(i, ":", sep = "")
      aeq <- all.equal(both[, paste("Old_", i, sep = "")],
                       both[, paste("New_", i, sep = "")])
      print(aeq)
      if(notEqual(aeq)) {
        cr <- cor(both[, paste("Old_", i, sep = "")],
                  both[, paste("New_", i, sep = "")],
                  use = "pairwise.complete.obs")
        cat("\t\t\tcorr:", round(cr, 2),
            "\n")
        if(is.na(cr) || cr< .8) print(both[, c(param, paste("Old_", i, sep = ""),
                                               paste("New_", i, sep = ""))])
      }
    }
  }
}


checkModels(common, Old, New)

q("no")
