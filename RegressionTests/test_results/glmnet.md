glmnet (`glmnet`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/glmnet.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/glmnet.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `foreach` (1.4.3), `glmnet` (2.0-5), `Matrix` (1.2-8)
 * tested on 2017-04-12 at 18:34. 
 * total test time: 495.8s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `foreach` (1.4.3), `glmnet` (2.0-5), `Matrix` (1.2-8)
 * tested on 2017-04-12 at 10:07. 
 * total test time: 258.2s


Results:
---------

**Test Case**: `class_cv_3_model`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (4), scaled (4)  
 * 75 tuning parameter combinations were evaluated


Execution times: (old) 6.52s (new) 3.15s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 75 tuning parameter combinations were evaluated


Execution times: (old) 3.21s (new) 1.47s

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
 * Pre-processing: centered (7), scaled (7)  
 * 75 tuning parameter combinations were evaluated


Execution times: (old) 4.12s (new) 2.07s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_loo_3_model`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (4), scaled (4)  
 * 75 tuning parameter combinations were evaluated


Execution times: (old) 33.08s (new) 15.74s

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
 * 75 tuning parameter combinations were evaluated


Execution times: (old) 47.41s (new) 23.57s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_3_model`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (4), scaled (4)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.55s (new) 0.15s

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


Execution times: (old) 0.53s (new) 0.12s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_pred`

Object class(es): `factor`

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

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 0.99s (new) 0.4s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (30), scaled (30)  
 * 138 tuning parameter combinations were evaluated


Execution times: (old) 1.6s (new) 0.73s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (30), scaled (30)  
 * 138 tuning parameter combinations were evaluated


Execution times: (old) 3.61m (new) 1.99m

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (30), scaled (30)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.56s (new) 0.16s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `reg_pred`

Object class(es): `numeric`

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


Execution times: (old) 1.05s (new) 0.44s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `sparse_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: None  
 * 114 tuning parameter combinations were evaluated


Execution times: (old) 1.29s (new) 0.68s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `sparse_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: None  
 * 114 tuning parameter combinations were evaluated


Execution times: (old) 2.91m (new) 1.46m

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `sparse_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: None  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.64s (new) 0.15s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `sparse_none_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `sparse_pred`

Object class(es): `numeric`

 * _Equal results_

