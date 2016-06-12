glmnet (`glmnet`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/glmnet.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `foreach` (1.4.3), `glmnet` (2.0-5), `Matrix` (1.2-6)
 * tested on 2016-06-10 at 17:06


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `foreach` (1.4.3), `glmnet` (2.0-5), `Matrix` (1.2-6)
 * tested on 2016-06-12 at 12:41


Results:
---------

**Test Case**: `class_cv_3_model`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (4), scaled (4)  
 * 75 tuning parameter combinations were evaluated


Execution times: (old) 6.38s (new) 6.24s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 75 tuning parameter combinations were evaluated


Execution times: (old) 3.23s (new) 3.21s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                 
      mean  3.434
      min   0.000
      max  17.308
    correlation:  0.8911 
</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 75 tuning parameter combinations were evaluated


Execution times: (old) 4.87s (new) 4.61s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                 
      mean  3.434
      min   0.000
      max  17.308
    correlation:  0.8911 
</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_loo_3_model`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (4), scaled (4)  
 * 75 tuning parameter combinations were evaluated


Execution times: (old) 32.36s (new) 30.97s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 75 tuning parameter combinations were evaluated


Execution times: (old) 51.78s (new) 50.01s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences < 0.1%
</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_3_model`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (4), scaled (4)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.49s (new) 0.49s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.48s (new) 0.49s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_pred`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_pred`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_predictors1`

Object class(es): `character`

 * _Equal results_

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 0.95s (new) 0.95s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing:  (None)  
 * 138 tuning parameter combinations were evaluated


Execution times: (old) 1.42s (new) 1.39s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing:  (None)  
 * 138 tuning parameter combinations were evaluated


Execution times: (old) 3.25m (new) 3.14m

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing:  (None)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.52s (new) 0.53s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `reg_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `reg_predictors1`

Object class(es): `character`

 * _Equal results_

**Test Case**: `reg_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 0.94s (new) 0.93s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `sparse_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: None  
 * 114 tuning parameter combinations were evaluated


Execution times: (old) 1.4s (new) 1.33s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `sparse_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: None  
 * 114 tuning parameter combinations were evaluated


Execution times: (old) 2.83m (new) 2.8m

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `sparse_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: None  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.54s (new) 0.52s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `sparse_none_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `sparse_pred`

Object class(es): `numeric`

 * _Equal results_

