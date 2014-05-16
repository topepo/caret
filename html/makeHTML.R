library(caret)
library(randomForest)
library(mlbench)
library(AppliedPredictiveModeling)
library(ellipse)
library(doMC)
library(rpart)
library(kernlab)
library(klaR)
library(proxy)
library(earth)
library(MASS)
library(party)
library(pls)
library(Hmisc)
library(caTools)
library(googleVis)
library(class)
library(e1071)
library(mda)
library(plyr)
library(pls)


setwd("~/Code/caret/html/")

startPath <- getwd()

###########################################################################

dPath <- paste("html_",format(Sys.time(), "%Y_%m_%d_%H_%M"), sep = "")
dir.create(dPath)
                 
rnwFiles <- c("index", "similarity", "datasets", "misc", "preprocess", "visualizations",
              "featureSelection", "training", "bytag", 
              "varImp", "parallel", "splitting", "custom_models", "other")

rnwFiles <- paste(rnwFiles, ".Rhtml", sep = "")
file.copy(rnwFiles, file.path(getwd(), dPath, rnwFiles))

file.copy(list.files(pattern = "png$"),
          file.path(getwd(), dPath, list.files(pattern = "png$")))
file.copy("TrainAlgo",
          file.path(getwd(), dPath, "TrainAlgo"))
file.copy("style.css",
          file.path(getwd(), dPath, "style.css"))
dir.create(file.path(dPath, "images"))
file.copy(list.files(path = file.path(getwd(), "images"), 
                     pattern = "^img", 
                     full.names = TRUE),
          file.path(getwd(), dPath, "images"))
file.copy("style.css",
          file.path(getwd(), dPath, "style.css"))
file.copy("parallel.pdf",
          file.path(getwd(), dPath, "parallel.pdf"))
file.copy("template.html",
          file.path(getwd(), dPath, "template.html"))
file.copy("MaxDissim.gif",
          file.path(getwd(), dPath, "MaxDissim.gif"))
file.copy("d3.v3.js",
          file.path(getwd(), dPath, "d3.v3.js"))
file.copy("d3.v3.min.js",
          file.path(getwd(), dPath, "d3.v3.min.js"))
setwd(file.path(getwd(), dPath))
pathName <- paste(file.path(getwd()), "/", sep = "")

###########################################################################

library(knitr)

for(fileIndex in seq(along = rnwFiles)) {
  cat("###########################################################################\n",
      "Knitting", rnwFiles[fileIndex], "\n")
  knit(rnwFiles[fileIndex],
       output = tolower(gsub(".Rhtml", ".html", rnwFiles[fileIndex])))
  
}


unlink(list.files(pattern = "Rnw$"))



