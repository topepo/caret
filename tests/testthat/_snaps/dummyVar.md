# dummyVars print method

    Code
      print(dummyVars(~ Species + Sepal.Length, data = iris))
    Output
      Dummy Variable Object
      
      Formula: ~Species + Sepal.Length
      <environment>
      2 variables, 1 factors
      Variables and levels will be separated by '.'
      A less than full rank encoding is used

# dummyVars rejects levelsOnly when levels are shared

    Code
      dummyVars(~., data = dat, levelsOnly = TRUE)
    Condition
      Error in `dummyVars.default()`:
      ! You requested `levelsOnly = TRUE` but the following levels are not unique across predictors: x, y

# print.dummyVars describes the encoding

    Code
      print(dummyVars(~., data = dat))
    Output
      Dummy Variable Object
      
      Formula: ~.
      <environment>
      2 variables, 1 factors
      Variables and levels will be separated by '.'
      A less than full rank encoding is used

---

    Code
      print(dummyVars(~., data = dat, sep = "."))
    Output
      Dummy Variable Object
      
      Formula: ~.
      <environment>
      2 variables, 1 factors
      Variables and levels will be separated by '.'
      A less than full rank encoding is used

---

    Code
      print(dummyVars(~., data = dat, levelsOnly = TRUE))
    Output
      Dummy Variable Object
      
      Formula: ~.
      <environment>
      2 variables, 1 factors
      Factor variable names will be removed
      A less than full rank encoding is used

---

    Code
      print(dummyVars(~., data = dat, fullRank = TRUE))
    Output
      Dummy Variable Object
      
      Formula: ~.
      <environment>
      2 variables, 1 factors
      Variables and levels will be separated by '.'
      A full rank encoding is used

# predict.dummyVars validates its newdata

    Code
      predict(dv, newdata = NULL)
    Condition
      Error in `predict.dummyVars()`:
      ! newdata must be supplied

---

    Code
      predict(dv, newdata = dat[, "n", drop = FALSE])
    Condition
      Error in `predict.dummyVars()`:
      ! Variable(s) 'size' are not in newdata

# contr.ltfr needs at least two levels

    Code
      contr.ltfr(1)
    Condition
      Error in `contr.ltfr()`:
      ! not enough degrees of freedom to define contrasts

---

    Code
      contr.ltfr("a")
    Condition
      Error in `contr.ltfr()`:
      ! contrasts not defined for 0 degrees of freedom

# contr.dummy builds an identity contrast matrix

    Code
      caret:::contr.dummy(1)
    Condition
      Error in `caret:::contr.dummy()`:
      ! not enough degrees of freedom to define contrasts

# class2ind converts a factor to indicator columns

    Code
      class2ind(1:4)
    Condition
      Error in `class2ind()`:
      ! 'x' should be a factor

