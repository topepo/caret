Ensemble Partial Least Squares Regression (`enpls`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/enpls.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2015-11-18 r69655)
 * `caret` (6.0-62), `enpls` (1.1)
 * tested on 2015-12-31 at 16:58


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2015-11-18 r69655)
 * `caret` (6.0-64), `enpls` (1.1)
 * tested on 2016-01-04 at 13:41


Results:
---------

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 1 tuning parameter combination was evaluated


Execution times: (old) 25.97s (new) 35.35s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    no pairs of results without missing values
  maxcomp RMSE_new Rsquared_new RMSE_old Rsquared_old Diff
1       4 16.38294    0.2456603       NA           NA   NA
2      20       NA           NA 17.58545    0.1743396   NA
</pre>

 * ***UNequal results for Rsquared***:

<pre>
    no pairs of results without missing values
  maxcomp RMSE_new Rsquared_new RMSE_old Rsquared_old Diff
1       4 16.38294    0.2456603       NA           NA   NA
2      20       NA           NA 17.58545    0.1743396   NA
</pre>


**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 1 tuning parameter combination was evaluated


Execution times: (old) 26.4s (new) 36.05s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    no pairs of results without missing values
  maxcomp RMSE_new Rsquared_new RMSE_old Rsquared_old Diff
1       4 16.38294    0.2456603       NA           NA   NA
2      20       NA           NA 17.58545    0.1743396   NA
</pre>

 * ***UNequal results for Rsquared***:

<pre>
    no pairs of results without missing values
  maxcomp RMSE_new Rsquared_new RMSE_old Rsquared_old Diff
1       4 16.38294    0.2456603       NA           NA   NA
2      20       NA           NA 17.58545    0.1743396   NA
</pre>


**Test Case**: `reg_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 1 tuning parameter combination was evaluated


Execution times: (old) 6.67m (new) 4.56m

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    no pairs of results without missing values
  maxcomp RMSE_new Rsquared_new RMSE_old Rsquared_old Diff
1       4  16.2473    0.1585766       NA           NA   NA
2      20       NA           NA  16.3246    0.1548541   NA
</pre>

 * ***UNequal results for Rsquared***:

<pre>
    no pairs of results without missing values
  maxcomp RMSE_new Rsquared_new RMSE_old Rsquared_old Diff
1       4  16.2473    0.1585766       NA           NA   NA
2      20       NA           NA  16.3246    0.1548541   NA
</pre>


**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 12.8s (new) 8.63s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `numeric`

 * ***UNequal results***:
<pre>
    %differences (n-o):                  
      mean  0.0349
      min  -0.2570
      max   0.7408
    correlation:  1 
</pre>

**Test Case**: `reg_pred`

Object class(es): `numeric`

 * ***UNequal results***:
<pre>
    %differences (n-o):                  
      mean  0.0753
      min  -0.4359
      max   1.6546
    correlation:  1 
</pre>

**Test Case**: `reg_pred_form`

Object class(es): `numeric`

 * ***UNequal results***:
<pre>
    %differences (n-o):                  
      mean  0.0753
      min  -0.4359
      max   1.6546
    correlation:  1 
</pre>

**Test Case**: `reg_predictors1`

