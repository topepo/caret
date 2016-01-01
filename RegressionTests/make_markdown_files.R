source("markdown_functions.R")

## File in the paths to the test results for the "old" and "new" versions
op <- "/Volumes/kuhna03/tmp/2015_12_31_16__6.0-62/"
np <- "/Volumes/kuhna03/tmp/2015_12_31_19__6.0-64/"

mods <- match_objects(op, np)

## There are a few files that I have not written markdown extensions for
## yet

exclusions <- c("knnImpute", "preprocess_back", "ga", "sa", "subsampling")

mods <- mods[!(mods %in% exclusions)]

library(caret)

caret_mods <- getModelInfo()

for(mod in mods) {
  
  sink(file = file.path("~/Downloads/", paste0(mod, ".md")))
  
  if(mod %in% names(caret_mods)) {
    cat(caret_mods[[mod]]$label, " (`", mod, "`)",  sep = "") 
  } else {
    cat("`", mod, "`", sep = "")
  }
  cat("\n ===== \n\n", sep = "")
  
  cat(paste0("There are regression tests to compare model results between different versions ",
             "of `caret` and the individual packages. These test evaluate whether consistent ",
             "results can be obtained. The code used to generate the objects that are compared ",
             "can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/",
             mod, ".R).\n\n"))
      
      try(obj_compare(file.path(op, paste0(mod, ".RData")),
                      file.path(np, paste0(mod, ".RData"))),
          silent = TRUE)
      
      sink()
}




