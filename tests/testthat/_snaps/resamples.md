# resample calculations

    'rlm' failed to converge in 20 steps

# as.matrix.resamples returns a resample-by-model matrix

    Code
      as.matrix(rs_fixture, metric = "nope")
    Condition
      Error in `as.matrix.resamples()`:
      ! no columns fit that metric

# resamples print methods render

    Code
      print(rs_fixture)
    Output
      
      Call:
      resamples(list(A = A, B = B, C = C))
      
      Models: A, B, C 
      Number of resamples: 5 
      Performance metrics: RMSE, Rsquared 
      Time estimates for: everything, final model fit, prediction 

---

    Code
      print(summary(rs_fixture))
    Output
      
      Call:
      summary.resamples(object = rs_fixture)
      
      Models: A, B, C 
      Number of resamples: 5 
      
      RMSE 
        Min. 1st Qu. Median Mean 3rd Qu. Max. NA's
      A    1       2      3    3       4    5    0
      B    3       3      4    4       5    5    0
      C    3       4      5    5       6    7    0
      
      Rsquared 
        Min. 1st Qu. Median Mean 3rd Qu. Max. NA's
      A 0.70    0.75   0.80 0.80    0.85 0.90    0
      B 0.63    0.66   0.71 0.72    0.78 0.82    0
      C 0.50    0.55   0.60 0.60    0.65 0.70    0
      

---

    Code
      print(diff(rs_fixture))
    Output
      
      Call:
      diff.resamples(x = rs_fixture)
      
      Models: A, B, C 
      Metrics: RMSE, Rsquared 
      Number of differences: 3 
      p-value adjustment: bonferroni 

---

    Code
      print(summary(diff(rs_fixture)))
    Output
      
      Call:
      summary.diff.resamples(object = diff(rs_fixture))
      
      p-value adjustment: bonferroni 
      Upper diagonal: estimates of the difference
      Lower diagonal: p-value for H0: difference = 0
      
      RMSE 
        A      B      C 
      A        -1     -2
      B 0.1023        -1
      C 0.6906 1.0000   
      
      Rsquared 
        A         B         C   
      A           0.08      0.20
      B 0.0001722           0.12
      C 0.1422620 0.4967353     
      

# xyplot.resamples checks what it was asked to draw

    Code
      xyplot(rs_fixture, units = "fortnight")
    Condition
      Error in `xyplot.resamples()`:
      ! units should be 'sec', 'min' or 'hour'

---

    Code
      xyplot(rs_fixture, what = "bogus")
    Condition
      Error in `xyplot.resamples()`:
      ! the what arg should be 'scatter', 'BlandAltman', 'tTime', 'mTime' or 'pTime'

---

    Code
      xyplot(rs_fixture, metric = c("RMSE", "Rsquared"))
    Condition
      Error in `xyplot.resamples()`:
      ! exactly one metric must be given

---

    Code
      xyplot(rs_fixture, what = "BlandAltman", models = c("A", "B", "C"))
    Condition
      Error in `xyplot.resamples()`:
      ! exactly two model names must be given

# splom.resamples draws each of its variants

    Code
      splom(rs_fixture, variables = "bogus")
    Condition
      Error in `splom.resamples()`:
      ! 'variables' should be either 'models' or 'metrics'

# levelplot.diff.resamples draws p-values and differences

    Code
      levelplot(d, metric = c("RMSE", "Rsquared"))
    Condition
      Error in `levelplot.diff.resamples()`:
      ! exactly one metric must be given

