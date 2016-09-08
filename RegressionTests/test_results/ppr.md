Projection Pursuit Regression (`ppr`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/ppr.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-71)
 * tested on 2016-09-06 at 11:49


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-72)
 * tested on 2016-09-08 at 10:51


Results:
---------

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.08s (new) 0.99s

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
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.41s (new) 1.33s

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
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 4.59s (new) 4.31s

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


Execution times: (old) 0.51s (new) 0.5s

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

**Test Case**: `reg_predictors1`

Object class(es): `character`

 * _Equal results_

**Test Case**: `reg_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: centered (20), scaled (20)  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 1.27s (new) 1.25s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

