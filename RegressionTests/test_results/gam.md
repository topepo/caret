Generalized Additive Model using Splines (`gam`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/gam.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/gam.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `mgcv` (1.8-17), `nlme` (3.1-131)
 * tested on 2017-04-12 at 18:22. 
 * total test time: 69.7s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `mgcv` (1.8-17), `nlme` (3.1-131)
 * tested on 2017-04-11 at 20:28. 
 * total test time: 60s


Results:
---------

**Test Case**: `class_cv_dist`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (2), scaled (2)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 1.8s (new) 1.3s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (2), scaled (2)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 2.07s (new) 1.46s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (2), scaled (2)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 2.67s (new) 1.83s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_imp`

Object class(es): `varImp.train`

 * _Equal results_

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (2), scaled (2)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 36.57s (new) 34.13s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (2), scaled (2)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.87s (new) 0.54s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_pred`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_none_prob`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `class_pred`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_predictors1`

Object class(es): `character`

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
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 1.31s (new) 1s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

**Test Case**: `reg_cv_dist`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 6.41s (new) 5.8s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 1.8s (new) 1.38s

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


Execution times: (old) 1.77s (new) 1.52s

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
 * Pre-processing: centered (3), scaled (3)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 8.97s (new) 6.65s

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


Execution times: (old) 0.58s (new) 0.25s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `array`

 * _Equal results_

**Test Case**: `reg_pred`

Object class(es): `array`

 * _Equal results_

**Test Case**: `reg_pred_form`

Object class(es): `array`

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
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 1.71s (new) 1.32s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

