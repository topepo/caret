Building New R Packages
========================================================
author: Zach Mayer
date: 2015-04-25

Building New R Packages
========================================================
- Context switching is bad
  - Between R and the command line
  - Between R and the file system
  - Between R and version control
  - Between .R files and .Rd files
- Minimizing context switching maximizes productivity
- Hadley Wickham has kinldy built a package development toolkit to minimize context switching
  - Will save you all kinds of CRAN-related trouble
  - No excuse not to use it for new packages
  - Very easy to move over parts of old packages

Single context package development:
========================================================
- github for source control
- RStudio projects for a standard development environment
- devtools for R CMD BUILD / CHECK
- testthat for automated unit testing
- roxygen2 for code documentation

github
========================================================

- "Social Network" for code
- Non-CRAN code distribtion
  - `devtools::install_github()`
  - Can also use this to distribute private packages
- Pull Requests as a mechanism for code review
- Automated unit testing through travis-CI
  - `devtools::use_travis()`
  - Test converage reports through coveralls

devtools
========================================================
Automates tedious package development tasks
- Old release process
  - R CMD BUILD
  - R CMD CHECK
  - Release via email to CRAN
- New release process
  - `devtools::check()`
  - `devtools::release()`
- This is the entire release process for most packages
  - 80% of the process for caret

github + travis
========================================================
Automates the boring parts of code review

- Contributer submits code via a pull request
  - Travis run R CMD CHECK
  - Travis notifies them of test failures
  - Contributer can iterate several times without your input
- You start code review once tests pass
  - github supports line-by-line comments

testthat
========================================================
Automated unit testing

- If you don't have unit tests, you should
  - `devtools::use_testhat()`
- Unit tests prevent new features from breaking old code
- All functions should have associated tests
- Can specify that certain tests be skipped on CRAN

Caret is slowly adding more testthat tests

roxygen2
========================================================
Simplified package documentation

- Code and documentation are in the same file
  - No context switching
- A must have for new packages
- Hard to convert existing packages
  - caret has 92 .Rd files
  - I'm not in a hury to re-write them all in roxygen2 format

Wrapup
========================================================
- New packages:
  - Build it in a RStudio project
  - Test it with testthat
  - Document it with Roxygen2
  - Store the code on github
  - Submit it to CRAN with devtools
  - https://github.com/zachmayer/kaggleNCAA
- Existing packages:
  - As many of the above as you can reasonably support
  - https://github.com/topepo/caret
