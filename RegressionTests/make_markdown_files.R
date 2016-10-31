source("markdown_functions.R")

## File in the paths to the test results for the "old" and "new" versions
op <- "~/tmp/2016_10_31_05__6.0-71/"
np <- "~/tmp/2016_10_30_22__6.0-72/"

mods <- match_objects(op, np)

## There are a few files that I have not written markdown extensions for
## yet

exclusions <- c("knnImpute", "preprocess_back", "ga", "sa", "subsampling")

mods <- mods[!(mods %in% exclusions)]

library(caret)

caret_mods <- getModelInfo()

for(mod in mods) {
  
  sink(file = file.path("~/tmp/test_results", paste0(mod, ".md")))
  
  if(mod %in% names(caret_mods)) {
    cat(caret_mods[[mod]]$label, " (`", mod, "`)",  sep = "") 
  } else {
    cat("`", mod, "`", sep = "")
  }
  cat("\n===== \n\n", sep = "")
  
  cat(paste0("There are regression tests to compare model results between different versions ",
             "of `caret` and the individual packages. These test evaluate whether consistent ",
             "results can be obtained. The code used to generate the objects that are compared ",
             "can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/",
             mod, ".R).\nA [history of commits](https://github.com/topepo/caret/commits/master/models/files/",
             mod, ".R) for the model code is also available\n\n")) 
      
      try(obj_compare(file.path(op, paste0(mod, ".RData")),
                      file.path(np, paste0(mod, ".RData"))),
          silent = TRUE)
      
      sink()
}




