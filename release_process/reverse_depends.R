library(tools)
library(devtools)
options(repos = "http://cran.r-project.org")

###################################################################
## Make a time stamped path for the source code

pkg_path <- paste0("~/tmp/rev_deps_", format(Sys.time(), "%Y_%m_%d_%H"))
if(!dir.exists(pkg_path))
  dir.create(pkg_path)
setwd(pkg_path)

###################################################################
## Copy the current tar file of caret sources to this path

src <- build("~/github/caret/pkg/caret")
file.copy(src, pkg_path)


check_packages_in_dir(pkg_path, reverse = list())

summarize_check_packages_in_dir_results(pkg_path)

if(!interactive()) q("no")



