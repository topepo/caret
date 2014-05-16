setwd("/Users/kuhna03/Code/caret/RegressionTests")

#############################################################################

newPath <- paste(format(Sys.time(), "%Y_%m_%d_%H"), 
                 packageDescription("caret")$Version,
                 sep = "__")
                 
dir.create(newPath)

testFiles <- list.files(file.path(getwd(), "Code"),
                        full.names = TRUE)
newFiles <- gsub("/RegressionTests/Code/",
                 paste("/RegressionTests/", newPath, "/", sep = ""),
                 testFiles)

file.copy(testFiles, newFiles)

#############################################################################
## write out makefile here and save to code directory prior to copy

rFiles <- list.files(file.path(getwd(), "Code"), pattern = "R$")

header <- paste(sort(rFiles), "Data: ", sort(rFiles), "\n", sep = "")

strt <- paste("\t@date '+ %Y-%m-%d %H:%M:%S: Starting ",
              gsub(".R$", "", sort(rFiles)), "'\n", sep = "")

batch <- paste("\t@R CMD BATCH --vanilla ", sort(rFiles), "\n", sep = "")

fini <- paste("\t@date '+ %Y-%m-%d %H:%M:%S: Finished ",
              gsub(".R$", "", sort(rFiles)), "'\n\n", sep = "")

rdata <- paste(sort(rFiles), "Data", sep = "")

over <- length(rdata) %% 3
if(over > 0) rdata <- c(rdata, rep("", 3 - over))
deps <- matrix(rdata, nrow = 3)
deps <- apply(deps, 2, function(x) paste("\t", paste(x, collapse = " ")))
deps[1] <- gsub("\t", "all: ", deps[1])
deps[1:(length(deps) - 1)] <- paste(deps[1:(length(deps) - 1)], " \\\n", sep = "")
deps[length(deps)] <- paste(deps[length(deps)], "\n\n")
cat(deps, sep = "")

mf <- c("SHELL = /bin/bash\n\n", deps,
          paste(header, strt, batch, fini, sep = ""))

cat(mf, sep = "",  file = file.path(getwd(), newPath, "makefile"))

#############################################################################
## missing tests

if(FALSE){
  library(caret)
  
  mods <- names(getModelInfo())
  testfiles <- gsub(".R", "", rFiles, fixed = TRUE)
  
  testFiles <- list.files(file.path(getwd(), "Code"))
  modelFiles <- list.files("/Users/kuhna03/Code/caret/pkg/caret/inst/models")  
  
  modelFiles[!(modelFiles %in% testFiles)]
}
