Conditional Inference Random Forest (`cforest`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/cforest.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-71), `modeltools` (0.2-21), `mvtnorm` (1.0-5), `party` (1.0-25), `sandwich` (2.3-4), `strucchange` (1.5-1), `zoo` (1.7-12)
 * tested on 2016-09-06 at 11:12


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-72), `modeltools` (0.2-21), `mvtnorm` (1.0-5), `party` (1.0-25), `sandwich` (2.3-4), `strucchange` (1.5-1), `zoo` (1.7-12)
 * tested on 2016-09-08 at 10:03


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


Execution times: (old) 1.72s (new) 1.13s

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
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 3.25s (new) 2.19s

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
 * Pre-processing: centered (7), scaled (7)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 10.29s (new) 9.3s

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
 * Pre-processing: centered (7), scaled (7)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.61s (new) 0.59s

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

**Test Case**: `class_oob_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Out of Bag Resampling
 * Grid search
 * Pre-processing: None  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 0.66s (new) 0.67s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

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


Execution times: (old) 1.8s (new) 1.45s

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
 * Pre-processing: centered (20), scaled (20)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.18s (new) 1.26s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.61s (new) 1.53s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 8.07s (new) 7.49s

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


Execution times: (old) 0.6s (new) 0.59s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `reg_oob_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Out of Bag Resampling
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 0.76s (new) 0.73s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `reg_pred_form`

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


Execution times: (old) 1.59s (new) 1.49s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

