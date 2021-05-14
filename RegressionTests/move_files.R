setwd("~/github/caret/RegressionTests")

## Should we just test a small, diverse set of models instead of everything?
small <- FALSE

fselect <- FALSE

#############################################################################

newPath <- paste(format(Sys.time(), "%Y_%m_%d_%H"),
                 packageDescription("caret")$Version,
                 sep = "__")

dir.create(paste0("~/tmp/", newPath))

if(!small) {
  testFiles <- list.files(file.path(getwd(), "Code"),
                          full.names = TRUE)
} else {
  testFiles <- c(## some models with sequential parameters:
                 "glmnet", "simpls", "rpart", "cubist",
                 ## nonstandard input options
                 "xgbTree",
                 ## basic models
                 "ctree", "svmRadial", "WM", "bag",
                 ## Other clases
                 "sbf_treebag", "rfe_train")
  testFiles <- paste0(testFiles, ".R")
  testFiles <- paste(file.path(getwd(), "Code", testFiles))
}
## package archived or models excluded
exclusions <- c("rknn", "rknnBel", "[mM]xnet", "sdda", "enpls.fs",
                "enpls", "Boruta", "Mlda", "RFlda", "rbf", "bdk",
                "SLAVE", "_back", "oblique\\.tree", "ordinalRF",
                "pythonKnnReg")
exclusions <- paste0("(", exclusions, ")")
exclusions <- paste0(exclusions, collapse = "|")
testFiles <- testFiles[!grepl(exclusions, testFiles)]

if(!fselect) {
  fs_excl <- grepl("(/sbf)|(/rfe)", testFiles)
  testFiles <- testFiles[!fs_excl]
}

newFiles <- paste0("~/tmp/", newPath, "/", basename(testFiles))

file.copy(testFiles, newFiles)

frbs <- c('ANFIS', 'DENFIS', 'FH.GBML', 'FIR.DM', 'FRBCS.CHI', 'FRBCS.W',
          'FS.HGD', 'GFS.FR.MOGAL', 'GFS.GCCL', 'GFS.LT.RS', 'GFS.THRIFT',
          'HYFIS', 'SBC', 'SLAVE', 'WM')
frbs <- paste0(frbs, ".RData")


#############################################################################
## write out makefile here and save to code directory prior to copy

rFiles <- basename(testFiles)

rFiles <- sample(rFiles)

file_label <- gsub(".R$", "", rFiles)
file_label <- paste0(format(seq_along(file_label)), "/",
                     length(file_label), " ",
                     file_label)

header <- paste(rFiles, "Data: ", rFiles, "\n", sep = "")

strt <- paste("\t@date '+ %Y-%m-%d %H:%M:%S Starting ",
              file_label, "'\n", sep = "")

batch <- paste("\t@R CMD BATCH --vanilla ", rFiles, "\n", sep = "")

fini <- paste("\t@date '+ %Y-%m-%d %H:%M:%S Finished ",
              file_label, "'\n\n", sep = "")

rdata <- paste(rFiles, "Data", sep = "")
rdata0 <- rdata[!(rdata %in% frbs)]

over <- length(rdata) %% 3
if(over > 0) rdata <- c(rdata, rep("", 3 - over))
deps <- matrix(rdata, nrow = 3)
deps <- apply(deps, 2, function(x) paste("\t", paste(x, collapse = " ")))
deps[1] <- gsub("\t", "all: ", deps[1])
deps[1:(length(deps) - 1)] <- paste(deps[1:(length(deps) - 1)], " \\\n", sep = "")
deps[length(deps)] <- paste(deps[length(deps)], "\n\n")
cat(deps, sep = "")


over0 <- length(rdata0) %% 3
if(over0 > 0) rdata0 <- c(rdata0, rep("", 3 - over0))
deps0 <- matrix(rdata0, nrow = 3)
deps0 <- apply(deps0, 2, function(x) paste("\t", paste(x, collapse = " ")))
deps0[1] <- gsub("\t", "norules: ", deps0[1])
deps0[1:(length(deps0) - 2)] <- paste(deps0[1:(length(deps0) - 2)], " \\\n", sep = "")
deps0[length(deps0)] <- paste(deps0[length(deps0)], "\n\n")
cat(deps0, sep = "")

mf <- c("SHELL = /bin/bash\n\n", deps, deps0,
          paste(header, strt, batch, fini, sep = ""))

cat(mf, sep = "",  file = file.path("~/tmp", newPath, "makefile"))
