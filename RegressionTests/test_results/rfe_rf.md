`rfe_rf`
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rfe_rf.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-71), `plyr` (1.8.4), `randomForest` (4.6-12)
 * tested on 2016-09-06 at 11:53


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-72), `plyr` (1.8.4), `randomForest` (4.6-12)
 * tested on 2016-09-08 at 10:56


Results:
---------

**Test Case**: `cv_model`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 20


Execution times: (old) 1.83s (new) 1.67s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `cv_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 1.38s (new) 1.31s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `cv_model_form`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 22


Execution times: (old) 1.31s (new) 1.23s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `cv_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 1.39s (new) 1.3s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `cv_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `cv_pred_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `cv_pred_form_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `loo_model`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 20


Execution times: (old) 21.11s (new) 19.49s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `loo_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 21.23s (new) 19.63s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `loo_model_form`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 22


Execution times: (old) 31.88s (new) 30.39s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `loo_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 19.46s (new) 18.14s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `loo_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `loo_pred_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `loo_pred_form_class`

Object class(es): `data.frame`

 * _Equal results_

