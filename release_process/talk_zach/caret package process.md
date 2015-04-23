Building New R Packages
========================================================
author: Zach Mayer
date: 2015-04-25

R Packages
========================================================
- Context switching is bad
  - Between R and the command line
  - Between R and the file system
  - Between R and version control
  - Between .R files and .Rd files
- Minimum context switching maximizes productivity
- Hadley Wickham has kinldy built a package development toolkit to minimize context switching
  - Will save you all kinds of CRAN-related trouble
  - No excuse not to use it for new packages
  - Very easy to move over parts of old packages

Package development tools:
========================================================

- RStudio projects: development environment
- devtools: automate boring tasks
- testthat: automated unit testing
- roxygen2: combine source code with documentation
- github: source control

Package development tools:
========================================================

- Use Hadley's toolkit to build your next package
  - Hadley will send you any apology card if a bug in devtools causes you grief from the r-core team
- Consider re-structuring existing packages
  - Rstudio, devtools, and testthat are easy
  - Roxygen is more work

RStudio projects
========================================================
A clean, self contained package development environment

- No more `setw('/path/to/some/folder')` in scripts
- Keep track of project-wide standards, e.g. code formatting

An RStudio project was the first thing we added after moving the caret repository to github

devtools
========================================================
Automates tedious package development tasks

- `devtools::install()`
  - Builds package and installs it locally
- `devtools::check()`
  - Builds documentation
  - Runs unit tests
  - Builds tarball
  - Runs R CMD CHECK
- `devtools::release()`
  - Builds package and submits it to CRAN
- This is the entire release process for most packages
  - 80% of the process for caret

testthat
========================================================
Automated unit testing

- If you're not yet unit testing, you should be
- `devtools::use_testhat()`
- Unit tests prevent new features from breaking old code
- All functions should have associated tests
- Can specify that certain tests be skipped on CRAN

Caret is slowly adding more testthat tests

roxygen2
========================================================
Simplified package documentation

- Automates many parts of the documentation process
  - Special comment block above each function
  - Name, description, arguments, etc.
  - Code and documentation are in the same source file
- A must have for new packages
- Hard to convert existing packages
  - caret has 92 .Rd files
  - I'm not in a hury to re-write them all in roxygen2 format

github
========================================================
Version control for cool kids

- "Social Network" for code
- Non-CRAN code distribtion
  - `devtools::install_github()`
  - Can also use this to distribute private packages
- Pull Requests as a mechanism for code review
- Automated unit testing through travis-CI
  - `devtools::use_travis()`
  - Test converage reports through coveralls

github + travis + coveralls
========================================================
Automates the boring parts of code review

- Contributer submits code via a pull request
  - Travis notifies them of test failures
  - Coveralls notifies to write tests for new functions
  - Iterate several times
  - Can run unit tests that are skipped on CRAN
- You start code review once tests pass
  - github supports line-by-line comments
  - Usually several more iterations here

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
