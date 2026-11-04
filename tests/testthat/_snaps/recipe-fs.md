# safs and gafs need a seed per resample plus one from a recipe

    Code
      safs(rec, data = recipe_fs_dat, safsControl = safsControl(functions = caretSA,
        method = "cv", number = 3, seeds = 1:2), iters = 2, method = "lm", trControl = trainControl(
        method = "cv"))
    Condition
      Error in `safs.recipe()`:
      ! There must be at least 4 random number seeds passed to safsControl

---

    Code
      gafs(rec, data = recipe_fs_dat, gafsControl = gafsControl(functions = caretGA,
        method = "cv", number = 3, seeds = 1:2), popSize = 4, iters = 2, method = "lm",
      trControl = trainControl(method = "cv"))
    Condition
      Error in `gafs.recipe()`:
      ! There must be at least 4 random number seeds passed to gafsControl

# the recipe searches fall back when the external metric is missing

    The metric 'Bogus' is not created by the external summary function; 'RMSE' will be used instead

