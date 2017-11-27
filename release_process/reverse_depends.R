library(tools)
library(devtools)
library(diffr)
options(repos = "http://cran.r-project.org")

##############################################################
###  Make a time stamped path for the source code

devel_path <- paste0("~/tmp/rev_deps_devel_", format(Sys.time(), "%Y_%m_%d_%H"))
prod_path  <- paste0("~/tmp/rev_deps_prod_",  format(Sys.time(), "%Y_%m_%d_%H"))

if(!dir.exists(devel_path))
  dir.create(devel_path)
if(!dir.exists(prod_path))
  dir.create(prod_path)

##############################################################
### Copy the current tar file of caret sources to this path

setwd(devel_path)

src <- build("~/github/caret/pkg/caret")
file.copy(src, devel_path)

check_packages_in_dir(devel_path, reverse = list())

summarize_check_packages_in_dir_results(devel_path)

##############################################################
### Now do the same with the current version

setwd(prod_path)

download.packages("caret", destdir = prod_path)

check_packages_in_dir(prod_path, reverse = list())

summarize_check_packages_in_dir_results(prod_path)

##############################################################
### Look at differences

has_error <- function(x) {
  txt <- read.delim(x, sep = "\n", stringsAsFactors = FALSE)[,1]
  status_txt <- grep("^Status", txt, value = TRUE)
  pkg_issue <- any(grepl("but not available:", txt))
  isTRUE(grepl("ERROR", status_txt)) & !pkg_issue
}

check_dirs <- list.dirs(prod_path, recursive = FALSE)
check_dirs <- grep("Rcheck$", basename(check_dirs), value = TRUE)

for(i in check_dirs) {
  old_check <- file.path(prod_path, i, "00check.log")
  new_check <- file.path(devel_path, i, "00check.log")
  if(file.exists(old_check) && file.exists(new_check)) {
    # look for errors in either
    if(has_error(old_check) || has_error(new_check)) {
      file_diff <- diffr(old_check, new_check)
      print(file_diff)
    }
  }
}




# For the future:
if(FALSE) {
  source("https://install-github.me/r-lib/revdepcheck")
  library(revdepcheck)
  library(parallel)
  
  setwd("../pkg/caret/")
  
  revdep_check(num_workers = detectCores() - 1)
}


if(!interactive()) q("no")



