Neural Network (`neuralnet`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/neuralnet.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `MASS` (7.3-45), `neuralnet` (1.32)
 * tested on 2016-06-10 at 17:21


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `MASS` (7.3-45), `neuralnet` (1.32)
 * tested on 2016-06-12 at 12:57


Results:
---------

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 8.19s (new) 8.06s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 11.61s (new) 11.4s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_loo_model`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 2.36m (new) 2.34m

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 7.38s (new) 7.28s

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
 * Pre-processing: centered (3), scaled (3)  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 4.38m (new) 4.36m

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

