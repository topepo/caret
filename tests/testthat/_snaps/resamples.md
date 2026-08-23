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

# resamples refuses models that cannot be compared

    Code
      resamples(list(a = a, b = other_folds))
    Condition
      Error in `resamples.default()`:
      ! The samples indices are not equal across resamples

---

    Code
      resamples(list(a = a, b = fewer))
    Condition
      Error in `resamples.default()`:
      ! There are different numbers of resamples in each model

---

    Code
      resamples(list(a = loo))
    Condition
      Error in `resamples.default()`:
      ! at least two train objects are needed

# resamples warns when a model kept every candidate

    Code
      rs <- resamples(list(a = a, b = b))
    Condition
      Warning in `resamples.default()`:
      'a' did not have 'returnResamp="final"; the optimal tuning parameters are used
      Warning in `resamples.default()`:
      'b' did not have 'returnResamp="final"; the optimal tuning parameters are used

# resamples reports models with different metrics

    Some performance measures were not computed for each model: Accuracy, Custom, Kappa

# the resamples methods each want a single metric

    Code
      as.matrix(rs_fixture, metric = "Bogus")
    Condition
      Error in `as.matrix.resamples()`:
      ! no columns fit that metric

---

    Code
      prcomp(rs_fixture, metric = both)
    Condition
      Error in `prcomp.resamples()`:
      ! exactly one metric must be given

---

    Code
      cluster(rs_fixture, metric = both)
    Condition
      Error in `cluster.resamples()`:
      ! exactly one metric must be given

---

    Code
      parallelplot(rs_fixture, metric = both)
    Condition
      Error in `parallelplot.resamples()`:
      ! exactly one metric must be given

---

    Code
      splom(rs_fixture, metric = both)
    Condition
      Error in `splom.resamples()`:
      ! exactly one metric must be given

---

    Code
      cluster(1:10)
    Condition
      Error in `cluster.default()`:
      ! only implemented for resamples objects

---

    Code
      plot(pc, what = c("scree", "loadings"))
    Condition
      Error in `plot.prcomp.resamples()`:
      ! one plot at a time please

# splom.resamples needs two metrics for a metric panel

    Code
      splom(rs_fixture, variables = "metrics", metric = "RMSE")
    Condition
      Error in `splom.resamples()`:
      ! There should be at least two metrics

# the distribution plots can show a single metric

    Sorry Dave, only one value of metric is allowed right now. I'll use the first value

# plot.prcomp.resamples draws the remaining plot types

    Code
      print(pc, digits = 2)
    Output
      
      Call:
      prcomp.resamples(x = rs_fixture)
      
      Metric: RMSE 
                                        
      Std. Dev.          3.98 0.71 3e-16
      Cum. Percent Var.  0.97 1.00 1e+00
      
      Rotation:
                   PC1   PC2    PC3
      Resample1  0.767 -0.19  0.127
      Resample2  0.523  0.14  0.437
      Resample3  0.244 -0.33 -0.676
      Resample4 -0.034 -0.79 -0.016
      Resample5 -0.278 -0.46  0.580

# resamples warns for rfe and sbf models that kept every subset

    Code
      rs <- resamples(list(rfe = rf, sbf = sf))
    Condition
      Warning in `resamples.default()`:
      'rfe' did not have 'returnResamp="final"; the optimal subset is used
      Warning in `resamples.default()`:
      'sbf' did not have 'returnResamp="final"; the optimal subset is used

