Self-Organizing Map (`bdk`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/bdk.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/bdk.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-71), `class` (7.3-14), `kohonen` (2.0.19), `MASS` (7.3-45)
 * tested on 2016-10-31 at 05:25


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-72), `class` (7.3-14), `kohonen` (2.0.19), `MASS` (7.3-45)
 * tested on 2016-10-30 at 22:32


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


Execution times: (old) 2.3s (new) 2.12s

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


Execution times: (old) 4.49s (new) 4.64s

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


Execution times: (old) 46.54s (new) 45.51s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                  
      mean -0.2810
      min  -2.5641
      max   0.2681
    correlation:  0.9984 
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


Execution times: (old) 0.62s (new) 0.72s

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


Execution times: (old) 3.26s (new) 1.32s

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    0 missing values in old
    20 missing values in new
    %differences (n-o):                 
      mean  2.671
      min  -2.712
      max   6.517
    correlation:  0.9718 
</pre>

 * ***UNequal results for Kappa***:

<pre>
    0 missing values in old
    20 missing values in new
    %differences (n-o):                 
      mean 20.228
      min  -9.271
      max  56.239
    correlation:  0.9439 
</pre>


**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 24 tuning parameter combinations were evaluated


Execution times: (old) 2.19s (new) 2.09s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 24 tuning parameter combinations were evaluated


Execution times: (old) 5.96s (new) 5.19s

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


Execution times: (old) 46.1s (new) 47.46s

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


Execution times: (old) 0.69s (new) 0.75s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `reg_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `reg_pred_form`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `reg_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 29 tuning parameter combinations were evaluated


Execution times: (old) 2.04s (new) 1.21s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    0 missing values in old
    25 missing values in new
    %differences (n-o):                 
      mean 2.5047
      min  0.4092
      max  6.4062
    correlation:  0.9902 
</pre>

 * ***UNequal results for Rsquared***:

<pre>
    2 missing values in old
    25 missing values in new
    %differences (n-o):                  
      mean  -6.521
      min  -48.403
      max   17.232
    correlation:  0.9991 
</pre>


