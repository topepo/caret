Tree Models from Genetic Algorithms (`evtree`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/evtree.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-71), `evtree` (1.0-0), `partykit` (1.0-5)
 * tested on 2016-09-06 at 11:13


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-72), `evtree` (1.0-0), `partykit` (1.0-5)
 * tested on 2016-09-08 at 10:08


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.28s (new) 1.75s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.78s (new) 2.26s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 11.35s (new) 16.48s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.55s (new) 0.75s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_none_pred`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_pred`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_prob`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `class_prob_form`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 9.32s (new) 13.42s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (5), scaled (5)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.24s (new) 1.82s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (5), scaled (5)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.34s (new) 1.87s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (5), scaled (5)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 24.97s (new) 35.68s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (5), scaled (5)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.54s (new) 0.75s

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
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 8.67s (new) 11.49s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

