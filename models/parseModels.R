setwd("~/Code/caret/models/files")
modelFiles <- list.files(pattern = "\\.R$")

models <- vector(mode = "list", length = length(modelFiles))
names(models) <- gsub("\\.R", "", modelFiles)

for(i in seq(along = modelFiles)) {
  source(modelFiles[i])
  models[[i]] <- modelInfo
  rm(modelInfo)
}

save(models, file = "~/Code/caret/pkg/caret/inst/models/models.RData")

# cat("\nYou can update your caret installation using the command:\n\n")
# cat(sprintf("  cp models.RData %s/.\n", system.file("models", package="caret")))
