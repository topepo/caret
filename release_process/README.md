Pre-Release Work
---


 0. Install latest devel version of R ([here](http://r.research.att.com/snowleopard/R-devel/R-devel-snowleopard-signed.pkg) for OS X) 
 0. Sync `git` project
 0. Run `models/parseModels.R` to get new model results into package
 0. Run code to update all `caret` related packages (see `release_process/update_pkgs.R`) _outside of RStudio_. Beware that this might reinstall `caret`  
 0. Install current CRAN version of `caret` and run regression tests (via `RegressionTests/move_files.R`)
 0. Install devel version of `caret` and run regression tests (via `move_files.R` again)
 0. Compare results to see if any models fail or substantively change results using `RegressionTests/compare.R`
 0. Run `release_process/make_model_Rd.R` to make the models man file and move this to `pkg/caret/man`
 0. Run `R CMD build`
 0. Run `R CMD check --as-cran` 
 0. Run `html/makeHTML.R` to make new HTML files for github
 0. Send to CRAN with associated email with subject `CRAN submission caret XXXXXX`. Sacrifice a virgin chicken to ensure that there are no issues. 

 