Package Maintenance for caret
===


1. The caret package
   * what it does
   * large number of package dependencies
   * potential issues with package versions, naming conflicts, bugs, etc
   * large number of complex functions require a lot of documentation
   * most model code stored outside of the reach of `R CMD check` 
   
2. Version control
   * originally subversion via R-Forge
   * shortcomings of R-Forge
   * move to github: why? 
   
3. github and R packages

4. How we release versions
   * create certain man pages
   * sync code and use `R CMD check --as-cran` to ensure passing tests (+unit)
   * update all packages (and R)
   * run regression tests and evaluate results
   * send to CRAN (repeat as needed)
   * install `caret` locally
   * generate HTML documentation and sync github io branch
   * profit
   