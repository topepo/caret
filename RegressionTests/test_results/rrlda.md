Robust Regularized Linear Discriminant Analysis (`rrlda`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rrlda.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/rrlda.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `glasso` (1.8), `matrixcalc` (1.0-3), `mvoutlier` (2.0.6), `pcaPP` (1.9-61), `rrlda` (1.1), `sgeostat` (1.0-27)
 * tested on 2017-04-12 at 20:55. 
 * total test time: 140.4s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `glasso` (1.8), `matrixcalc` (1.0-3), `mvoutlier` (2.0.6), `pcaPP` (1.9-61), `rrlda` (1.1), `sgeostat` (1.0-27)
 * tested on 2017-04-11 at 20:50. 
 * total test time: 160.6s


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 7.21s (new) 7.89s

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
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 10.63s (new) 11.61s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 1.89m (new) 2.2m

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


Execution times: (old) 0.94s (new) 0.73s

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


Execution times: (old) 5.2s (new) 5.26s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

