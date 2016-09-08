`sbf_treebag`
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/sbf_treebag.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-71)
 * tested on 2016-09-06 at 12:01


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-72)
 * tested on 2016-09-08 at 11:05


Results:
---------

**Test Case**: `cv_model`

Object class(es): `sbf`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Non-formula method


Execution times: (old) 1.29s (new) 1.3s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `cv_model_class`

Object class(es): `sbf`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Non-formula method


Execution times: (old) 1.01s (new) 0.97s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `cv_model_form`

Object class(es): `sbf` and `sbf.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Formula method


Execution times: (old) 1.14s (new) 1.11s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `cv_model_form_class`

Object class(es): `sbf` and `sbf.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Formula method


Execution times: (old) 1.03s (new) 1s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `cv_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `cv_pred_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `cv_pred_form`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `loo_model`

Object class(es): `sbf`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Non-formula method


Execution times: (old) 12.16s (new) 11.77s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `loo_model_class`

Object class(es): `sbf`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Non-formula method


Execution times: (old) 5.28s (new) 5.21s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `loo_model_form`

Object class(es): `sbf` and `sbf.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Formula method


Execution times: (old) 12.63s (new) 12.25s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `loo_model_form_class`

Object class(es): `sbf` and `sbf.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Formula method


Execution times: (old) 5.75s (new) 5.59s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `loo_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `loo_pred_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `loo_pred_form`

Object class(es): `numeric`

 * _Equal results_

