# splsda validates its outcome

    Code
      caret:::splsda(x, as.numeric(iris$Species), K = 2, eta = 0.5)
    Condition
      Error in `splsda.default()`:
      ! y must be a matrix or a factor

---

    Code
      caret:::splsda(x, unnamed, K = 2, eta = 0.5)
    Condition
      Error in `splsda.default()`:
      ! the y matrix must have column names

---

    Code
      caret:::splsda(x, not_one, K = 2, eta = 0.5)
    Condition
      Error in `splsda.default()`:
      ! the rows of y must be 0/1 and sum to 1

# splsda ignores priors unless the Bayes method is used

    Priors are ignored unless probMethod = "Bayes"

# print.splsda reports the parameters and selected variables

    Code
      caret:::print.splsda(fit)
    Output
      
      Sparse Partial Least Squares for discriminant analysis
      ----
      Parameters: eta = 0.5, K = 2, kappa = 0.5
      PLS algorithm:
      pls2 for variable selection, simpls for model fitting
      The softmax function was used to compute class probabilities.
      
      SPLS chose 4 variables among 4 variables
      
      Selected variables: 
      Sepal.Length	Sepal.Width	Petal.Length	Petal.Width	

# print.splsda names the Bayes probability method

    Code
      caret:::print.splsda(fit)
    Output
      
      Sparse Partial Least Squares for discriminant analysis
      ----
      Parameters: eta = 0.5, K = 2, kappa = 0.5
      PLS algorithm:
      pls2 for variable selection, simpls for model fitting
      Bayes rule was used to compute class probabilities.
      
      SPLS chose 4 variables among 4 variables
      
      Selected variables: 
      Sepal.Length	Sepal.Width	Petal.Length	Petal.Width	

# print.splsda handles a single outcome column

    Code
      caret:::print.splsda(fit)
    Output
      
      Sparse Partial Least Squares for discriminant analysis
      ----
      Parameters: eta = 0.5, K = 2
      PLS algorithm:
      pls2 for variable selection, simpls for model fitting
      The softmax function was used to compute class probabilities.
      
      SPLS chose 4 variables among 4 variables
      
      Selected variables: 
      Sepal.Length	Sepal.Width	Petal.Length	Petal.Width	

# print.splsda falls back to variable positions without names

    Code
      caret:::print.splsda(fit)
    Output
      
      Sparse Partial Least Squares for discriminant analysis
      ----
      Parameters: eta = 0.5, K = 2, kappa = 0.5
      PLS algorithm:
      pls2 for variable selection, simpls for model fitting
      The softmax function was used to compute class probabilities.
      
      SPLS chose 4 variables among 4 variables
      
      Selected variables: 
      1	2	3	4	

