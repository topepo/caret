High-Dimensional Regularized Discriminant Analysis (`hdrda`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/hdrda.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/hdrda.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin15.6.0 (64-bit)
 * R Under development (unstable) (2017-04-10 r72501)
 * `caret` (6.0-75), `sparsediscrim` (0.2.3)
 * tested on 2017-04-12 at 18:41. 
 * total test time: 12.8s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `sparsediscrim` (0.2.3)
 * tested on 2017-04-11 at 20:55. 
 * total test time: 9.3s


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 18 tuning parameter combinations were evaluated


Execution times: (old) 1.98s (new) 1.42s

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
 * 18 tuning parameter combinations were evaluated


Execution times: (old) 4.64s (new) 3.52s

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


Execution times: (old) 0.28s (new) 0.27s

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

