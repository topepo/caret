`rfe_treebag`
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rfe_treebag.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/rfe_treebag.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `e1071` (1.6-8), `ipred` (0.9-6), `plyr` (1.8.4), `rpart` (4.1-10)
 * tested on 2017-04-12 at 18:48. 
 * total test time: 3681.6s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `e1071` (1.6-8), `ipred` (0.9-6), `plyr` (1.8.4), `rpart` (4.1-10)
 * tested on 2017-04-11 at 22:40. 
 * total test time: 496.2s


Results:
---------

**Test Case**: `cv_model`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 20


Execution times: (old) 48.42s (new) 7.61s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `cv_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 1.33m (new) 6.68s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `cv_model_form`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 22


Execution times: (old) 43.39s (new) 6.94s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `cv_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 48.37s (new) 7.65s

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

Object class(es): `rfe`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 20


Execution times: (old) 12.46m (new) 1.91m

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `loo_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 15.3m (new) 1.85m

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `loo_model_form`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 22


Execution times: (old) 16.72m (new) 1.9m

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `loo_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 13.15m (new) 2.08m

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

