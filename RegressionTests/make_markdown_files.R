source("markdown_functions.R")

## File in the paths to the test results for the "old" and "new" versions
op <- "~/tmp/2017_01_10_17__6.0-73/"
np <- "~/tmp/2017_01_10_17__6.0-74/"

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

elapsed_time <- data.frame(test = mods, old = 0*NA, new = 0*NA)
for(i in seq_along(mods)) {
  elapsed_time[i, ] <- get_times(file.path(op, paste0(mods[i], ".RData")),
                                 file.path(np, paste0(mods[i], ".RData")))
  
}

elapsed_time <- elapsed_time[complete.cases(elapsed_time),]
elapsed_time$Geo_Mean <- sqrt(elapsed_time$old*elapsed_time$new)
elapsed_time$Fold_diff <- elapsed_time$old/elapsed_time$new

library(ggplot2)
png("~/tmp/test_results/_times.png")
ggplot(elapsed_time, aes(x = Geo_Mean, y = Fold_diff)) + 
  geom_point() + scale_y_continuous(trans = "log2") +
  geom_smooth() + 
  ylab("Fold Difference (O/N)") + xlab("Geo Mean") + 
  ggtitle("Total Test Execution Time")  +
  theme_bw()
dev.off()

exp(t.test(log(elapsed_time$Fold_diff))$conf.int)
