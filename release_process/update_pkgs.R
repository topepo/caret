library(caret)

###################################################################
## Get the names of all CRAN related packages references by caret. 
## Exclude orphaned and Bioconductor packages for now

mods <- getModelInfo()

libs <- unlist(lapply(mods, function(x) x$library))
libs <- unique(sort(libs))
libs <- libs[!(libs %in% c("caret", "vbmp", "gpls", "logicFS", "SDDA"))]

###################################################################
## Check to see if any packages are not on CRAN

options(repos = "http://cran.r-project.org")
all_pkg <- rownames(available.packages(type = "source"))

diffs <- libs[!(libs %in% all_pkg)]
if(length(diffs) > 0) print(diffs)

###################################################################
## Install the files. This might re-install caret so beware.

install.packages(libs, repos = "http://cran.r-project.org", 
                 type = "source",
                 dependencies = c("Depends", "Suggests", "Imports"))

###################################################################
## Now get Bioconductor packages

source("http://www.bioconductor.org/getBioC.R")

getBioC(c("vbmp", "gpls", "logicFS"), 
        type = "source",
        dependencies = c("Depends", "Suggests", "Imports"))


