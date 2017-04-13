`sbf_lm`
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/sbf_lm.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/sbf_lm.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73)
 * tested on 2017-04-12 at 18:27. 
 * total test time: 12.7s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75)
 * tested on 2017-04-11 at 21:26. 
 * total test time: 10.9s


Results:
---------

**Test Case**: `cv_model`

Object class(es): `sbf`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Non-formula method


Execution times: (old) 0.73s (new) 0.62s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `cv_model_form`

Object class(es): `sbf` and `sbf.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Formula method


Execution times: (old) 0.53s (new) 0.5s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `cv_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `loo_model`

Object class(es): `sbf`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Non-formula method


Execution times: (old) 4.15s (new) 3.47s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `loo_model_form`

Object class(es): `sbf` and `sbf.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Formula method


Execution times: (old) 4.45s (new) 3.85s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `loo_pred`

Object class(es): `numeric`

 * _Equal results_

