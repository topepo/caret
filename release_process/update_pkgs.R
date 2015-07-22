library(caret)

###################################################################
## Get the names of all CRAN related packages references by caret. 
## Exclude orphaned and Bioconductor packages for now

mods <- getModelInfo()

libs <- unlist(lapply(mods, function(x) x$library))
libs <- unique(sort(libs))
libs <- libs[!(libs %in% c("caret", "vbmp", "gpls", "logicFS", "SDDA"))]
libs <- c(libs, "knitr", "Hmisc", "googleVis", "animation", 
          "desirability", "networkD3", "arm", "xtable", 
          "RColorBrewer", "gplots", "iplots", "latticeExtra",
          "scatterplot3d", "vcd", "igraph", "corrplot", "ctv",
          "Cairo", "shiny", "scales", "tabplot", "tikzDevice", "odfWeave",
          "multicore", "doMC", "doMPI", "doSMP", "doBy", 
          "foreach", "doParallel", "aod", "car", "contrast", "Design", 
          "faraway",  "geepack",  "gmodels", "lme4", "tidyr", "devtools", "testthat",
          "multcomp", "multtest", "pda", "qvalue", "ChemometricsWithR", "markdown", "rmarkdown",
          ## odds and ends
          "ape", "gdata", "boot", "bootstrap", "chron", "combinat", "concord", "cluster",
          "desirability", "gsubfn", "gtools", "impute", "Matrix", "proxy", "plyr", 
          "reshape", "rJava", "SparseM", "sqldf", "XML", "lubridate", "dplyr", "GA",
          "aroma.affymetrix", "remMap", "cghFLasso", "RCurl", "QSARdata", "reshape2",
          "mapproj", "ggmap", "ggvis", "SuperLearner", "subsemble", "caretEnsemble")
libs <- unique(libs)


###################################################################
## Check to see if any packages are not on CRAN

options(repos = "http://cran.r-project.org")
all_pkg <- rownames(available.packages(type = "source"))

diffs <- libs[!(libs %in% all_pkg)]
if(length(diffs) > 0) print(diffs)

###################################################################
## Install the files. This might re-install caret so beware.

devtools::install_github('dmlc/xgboost',subdir='R-package')

install.packages(libs, repos = "http://cran.r-project.org", 
                 type = "source",
                 dependencies = c("Depends", "Suggests", "Imports"))

###################################################################
## Now get Bioconductor packages

library(BiocInstaller) 
useDevel()

source("http://www.bioconductor.org/getBioC.R")

biocLite(c("vbmp", "gpls", "logicFS"), 
        type = "source",
        dependencies = c("Depends", "Imports"))

###################################################################
## Install orphaned packages: CHAID, rknn, SDDA


