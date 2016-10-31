Self-Organizing Maps (`xyf`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/xyf.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/xyf.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-71), `class` (7.3-14), `kohonen` (2.0.19), `MASS` (7.3-45)
 * tested on 2016-10-31 at 06:51


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-72), `class` (7.3-14), `kohonen` (2.0.19), `MASS` (7.3-45)
 * tested on 2016-10-31 at 00:19


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 24 tuning parameter combinations were evaluated


Execution times: (old) 2.09s (new) 1.91s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 24 tuning parameter combinations were evaluated


Execution times: (old) 4s (new) 4.23s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 24 tuning parameter combinations were evaluated


Execution times: (old) 39.87s (new) 39.59s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                  
      mean -0.1985
      min  -2.7143
      max   0.4988
    correlation:  0.9988 
</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.48s (new) 0.5s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_pred`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_none_prob`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `class_pred`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_prob`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `class_prob_form`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: centered (7), scaled (7)  
 * 24 tuning parameter combinations were evaluated


Execution times: (old) 2.96s (new) 1.26s

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    0 missing values in old
    20 missing values in new
    %differences (n-o):                
      mean 5.575
      min  0.000
      max  9.776
    correlation:  0.9507 
</pre>

 * ***UNequal results for Kappa***:

<pre>
    0 missing values in old
    20 missing values in new
    %differences (n-o):                 
      mean 35.445
      min   1.217
      max  75.214
    correlation:  0.9407 
</pre>


**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 24 tuning parameter combinations were evaluated


Execution times: (old) 3.95s (new) 3.84s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 24 tuning parameter combinations were evaluated


Execution times: (old) 30.88s (new) 31.24s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.52s (new) 0.47s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `matrix`

 * _Equal results_

**Test Case**: `reg_pred`

Object class(es): `matrix`

 * _Equal results_

**Test Case**: `reg_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: centered (20), scaled (20)  
 * 29 tuning parameter combinations were evaluated


Execution times: (old) 4.43s (new) 1.45s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Rsquared***:

<pre>
    2 missing values in old
    25 missing values in new
    %differences (n-o):                  
      mean 282.056
      min   -7.412
      max  557.616
    correlation:  0.5293 
</pre>


