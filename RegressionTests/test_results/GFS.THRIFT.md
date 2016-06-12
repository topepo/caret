Fuzzy Rules via Thrift (`GFS.THRIFT`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/GFS.THRIFT.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `frbs` (3.1-0)
 * tested on 2016-06-10 at 17:04


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `frbs` (3.1-0)
 * tested on 2016-06-12 at 12:40


Results:
---------

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 7.73s (new) 7.43s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 8.51s (new) 8.11s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_imp`

Object class(es): `varImp.train`

 * _Equal results_

**Test Case**: `reg_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 1.43m (new) 1.38m

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (2), scaled (2)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 1.43s (new) 1.47s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `matrix`

 * _Equal results_

**Test Case**: `reg_pred`

Object class(es): `matrix`

 * _Equal results_

**Test Case**: `reg_pred_form`

Object class(es): `matrix`

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


Execution times: (old) 1.83m (new) 1.76m

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

