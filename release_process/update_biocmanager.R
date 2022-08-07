options(repos = "http://cran.r-project.org", width = 100)
install.packages(c("BiocManager", "cli"), type = "source")

library(BiocManager)
library(cli)
library(tools)

# ------------------------------------------------------------------------------

## Get the names of all CRAN related packages references by caret.
## Exclude orphaned and Bioconductor packages for now

# install.packages(c("devtools"), repos = "http://cran.r-project.org", type = "source")
#
# library(devtools)
# install_github("topepo/caret", subdir = "pkg/caret")

library(caret)

mods <- getModelInfo()

libs <- unlist(lapply(mods, function(x) x$library))
libs <- libs[!(libs %in% c("caret", "vbmp", "gpls", "logicFS", "SDDA"))]
libs <- c(libs, package_dependencies("caret", reverse = TRUE)$caret)
libs <- unique(libs)

# Don't install rJava in case it borks the existing system
libs <- libs[libs != "rJava"]

# ------------------------------------------------------------------------------

## Check to see if any packages are not on CRAN

options(repos = "http://cran.r-project.org")
all_pkg <- rownames(available.packages(type = "source"))

diffs <- libs[!(libs %in% all_pkg)]
if (length(diffs) > 0) print(diffs)

# ------------------------------------------------------------------------------
## Install the files. This might re-install caret so beware.

libs <- sort(libs)
n <- length(libs)
iters <- format(1:n)
libs_chr <- format(libs)

for (i in seq_along(libs)) {
  cli::cli_rule(paste0(libs_chr[i], " (", iters[i], "/", n, ")"))

  res <- try(
    BiocManager::install(
      libs[i],
      type = "both",
      dependencies = c("Depends", "Imports"),
      update = FALSE,
      ask = FALSE
    ),
    silent = TRUE
  )

  if (inherits(res, "try-error")) {
    cat("Failed to install", libs[i], "\n\n")
    print(as.character(res))
  }

  cat("\n\n")
}

# ------------------------------------------------------------------------------
# exceptions:

install.packages("CHAID", repos="http://R-Forge.R-project.org")

# ------------------------------------------------------------------------------

q("no")
