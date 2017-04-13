Linear Regression (`lm`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/lm.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/lm.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin15.6.0 (64-bit)
 * R Under development (unstable) (2017-04-10 r72501)
 * `caret` (6.0-75)
 * tested on 2017-04-12 at 18:42. 
 * total test time: 9.2s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75)
 * tested on 2017-04-11 at 22:44. 
 * total test time: 5.7s


Results:
---------

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: None  
 * 1 tuning parameter combination was evaluated


Execution times: (old) 1.4s (new) 0.48s

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
 * 1 tuning parameter combination was evaluated


Execution times: (old) 1.22s (new) 0.69s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_weights`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (10 fold)
 * Grid search
 * Pre-processing: None  
 * 1 tuning parameter combination was evaluated


Execution times: (old) 0.67s (new) 0.58s

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
 * 1 tuning parameter combination was evaluated


Execution times: (old) 0.75s (new) 0.62s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_loo_weights`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: None  
 * 1 tuning parameter combination was evaluated


Execution times: (old) 0.73s (new) 0.6s

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


Execution times: (old) 0.29s (new) 0.19s

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

