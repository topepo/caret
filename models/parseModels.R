setwd("~/github/caret/models/files")

exclusions <- c("^rknn", "[mM]xnet", "^sdda", "^enpls", "Boruta", "bdk", "^oblique")
exclusions <- paste0("(", exclusions, ")")
exclusions <- paste0(exclusions, collapse = "|")

modelFiles <- list.files(pattern = "\\.R$")
modelFiles <- modelFiles[-grep(exclusions, modelFiles)]


models <- vector(mode = "list", length = length(modelFiles))
names(models) <- gsub("\\.R$", "", modelFiles)

for(i in seq(along = modelFiles)) {
  source(modelFiles[i])
  models[[i]] <- modelInfo
  rm(modelInfo)
}

save(models, file = "../../pkg/caret/inst/models/models.RData")

# cat("\nYou can update your caret installation using the command:\n\n")
# cat(sprintf("  cp models.RData %s/.\n", system.file("models", package="caret")))
