`rfe_lda`
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rfe_lda.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2015-11-18 r69655)
 * `caret` (6.0-62), `plyr` (1.8.3)
 * tested on 2015-12-31 at 17:37


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2015-11-18 r69655)
 * `caret` (6.0-64), `plyr` (1.8.3)
 * tested on 2016-01-04 at 14:22


Results:
---------

**Test Case**: `cv_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 0.69s (new) 0.78s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `cv_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 0.43s (new) 0.48s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `cv_pred_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `cv_pred_form_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `loo_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 2.58s (new) 2.95s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `loo_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 2.96s (new) 3.38s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `loo_pred_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `loo_pred_form_class`

Object class(es): `data.frame`

 * _Equal results_

