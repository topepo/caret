Sparse Mixture Discriminant Analysis (`smda`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/smda.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-71), `sparseLDA` (0.1-7)
 * tested on 2016-09-06 at 12:02


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-72), `sparseLDA` (0.1-7)
 * tested on 2016-09-08 at 11:06


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 8 tuning parameter combinations were evaluated


Execution times: (old) 34.69s (new) 34.31s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 8 tuning parameter combinations were evaluated


Execution times: (old) 36.73s (new) 36.75s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 1.27s (new) 1.32s

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

**Test Case**: `class_predictors1`

Object class(es): `character`

 * _Equal results_

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: centered (3), scaled (3)  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 20.34s (new) 20.75s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

