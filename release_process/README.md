Pre-Release Work
---


 0. Install latest devel version of R ([here](http://r.research.att.com/snowleopard/R-devel/R-devel-snowleopard-signed.pkg) for OS X) 
 0. Sync `git` project
 0. Run `models/parseModels.R` to get new model results into package
 0. Run code to update all `caret` related packages (see `release_process/update_pkgs.R`) _outside of RStudio_. Beware that this might reinstall `caret`  
 0. Install current CRAN version of `caret` and run regression tests (via `RegressionTests/move_files.R`)
 0. Install devel version of `caret` 
 1. Run the check for reverse dependencies using `revdepcheck::revdep_check(num_workers = 4)`
 1. Run regression tests (via `move_files.R` again)
 0. Compare results to see if any models fail or substantively change results using `RegressionTests/compare.R`
 0. Run `release_process/make_model_Rd.R` to make the models man file and move this to `pkg/caret/man`
 1. Look to find any CRAN policy changes [here](https://github.com/eddelbuettel/crp/commits/master/texi/CRAN_policies.texi).
 2. Look at the [CRAN Package Check Results](https://cran.r-project.org/web/checks/check_results_caret.html)
 0. Run `devtools::check()` 
 0. (Max only) use `devtools::build_win()` to check on CRAN servers
 0. Run `html/makeHTML.R` to make new HTML files for github
 0. Send to CRAN via [web form](https://xmpalantir.wu.ac.at/cransubmit/)
 1. Once accepted, use `git tag -a 	6.0-xx -m "Version 	6.0-xx release"` to mark the release. 

 