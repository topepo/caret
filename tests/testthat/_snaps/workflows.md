# progress reports the parameters of the current resample

    Code
      caret:::progress(params, names = c("Fold1", "Fold2"), iter = 1)
    Output
      + Fold1: k=5 

---

    Code
      caret:::progress(params, names = c("Fold1", "Fold2"), iter = 2, start = FALSE)
    Output
      - Fold2: k=5 

