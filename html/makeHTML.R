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
library(C50)
library(mda)
library(plyr)
library(pls)
library(animation)
library(ggplot2)
library(googleVis)
library(desirability)
library(DMwR)
library(ROSE)

theme_set(theme_bw())

setwd("/Users/kuhna03/Code/github/caret/html")

startPath <- getwd()
dest <- "~/tmp/"

###########################################################################

dPath <- paste("html_",format(Sys.time(), "%Y_%m_%d_%H_%M"), sep = "")
dir.create(file.path(dest, dPath))
                 
rnwFiles <- c("index", "similarity", "datasets", "misc", "preprocess", "visualizations",
              "featureSelection", "training", "other", "bytag", "sampling",
              "varImp", "parallel", "splitting", "random",
              "rfe", "filters", "GA", "SA",  "custom_models", "adaptive")

rnwFiles <- paste(rnwFiles, ".Rhtml", sep = "")
file.copy(rnwFiles, file.path(dest, dPath, rnwFiles))

file.copy(list.files(pattern = "png$"),
          file.path(dest, dPath, list.files(pattern = "png$")))
file.copy("TrainAlgo",
          file.path(dest, dPath, "TrainAlgo"))
file.copy("style.css",
          file.path(dest, dPath, "style.css"))
dir.create(file.path(dPath, "images"))
file.copy(list.files(path = file.path(dest, "images"), 
                     pattern = "^img", 
                     full.names = TRUE),
          file.path(dest, dPath, "images"))
file.copy("style.css",
          file.path(dest, dPath, "style.css"))
file.copy("parallel.pdf",
          file.path(dest, dPath, "parallel.pdf"))
file.copy("template.html",
          file.path(dest, dPath, "template.html"))
file.copy("MaxDissim.gif",
          file.path(dest, dPath, "MaxDissim.gif"))
file.copy("d3.v3.js",
          file.path(dest, dPath, "d3.v3.js"))
file.copy("d3.v3.min.js",
          file.path(dest, dPath, "d3.v3.min.js"))
setwd(file.path(dest, dPath))
pathName <- paste(file.path(dest), "/", sep = "")

###########################################################################

setwd(file.path(dest, dPath))

library(knitr)

for(fileIndex in seq(along = rnwFiles)) {
  cat("###########################################################################\n",
      "Knitting", rnwFiles[fileIndex], "\n")
  knit(rnwFiles[fileIndex],
       output = tolower(gsub(".Rhtml", ".html", rnwFiles[fileIndex])))
  
}


unlink(list.files(pattern = "Rhtml$"))



