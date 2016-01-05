`sbf_lm`
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/sbf_lm.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2015-11-18 r69655)
 * `caret` (6.0-62)
 * tested on 2015-12-31 at 17:45


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2015-11-18 r69655)
 * `caret` (6.0-64)
 * tested on 2016-01-04 at 14:30


Results:
---------

**Test Case**: `cv_model`

Object class(es): `sbf`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Non-formula method


Execution times: (old) 0.62s (new) 0.61s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `cv_model_form`

Object class(es): `sbf` and `sbf.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Formula method


Execution times: (old) 0.55s (new) 0.56s

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


Execution times: (old) 4.35s (new) 4.3s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `loo_model_form`

Object class(es): `sbf` and `sbf.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Formula method


Execution times: (old) 4.54s (new) 4.57s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `loo_pred`

Object class(es): `numeric`

 * _Equal results_

