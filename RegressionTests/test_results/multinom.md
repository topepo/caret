Penalized Multinomial Regression (`multinom`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/multinom.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-71), `nnet` (7.3-12)
 * tested on 2016-09-06 at 11:33


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-72), `nnet` (7.3-12)
 * tested on 2016-09-08 at 10:36


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


Execution times: (old) 0.88s (new) 0.86s

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


Execution times: (old) 1.37s (new) 1.29s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_cv_weight`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 0.93s (new) 0.87s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

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


Execution times: (old) 6.02s (new) 5.58s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_loo_weight`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 3.39s (new) 3.18s

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


Execution times: (old) 0.56s (new) 0.5s

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
 * Pre-processing: centered (7), scaled (7)  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 1.13s (new) 1.09s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

