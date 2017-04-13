Multilayer Perceptron Network by Stochastic Gradient Descent (`mlpSGD`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/mlpSGD.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/mlpSGD.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `FCNN4R` (0.6.2), `plyr` (1.8.4), `Rcpp` (0.12.10)
 * tested on 2017-04-12 at 20:40. 
 * total test time: 163.3s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `FCNN4R` (0.6.2), `plyr` (1.8.4), `Rcpp` (0.12.10)
 * tested on 2017-04-11 at 21:53. 
 * total test time: 139s


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 1.22s (new) 0.85s

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
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 1.46s (new) 1.05s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_form`

Object class(es): `factor`

 * _Equal results_

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 13.25s (new) 11.52s

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


Execution times: (old) 0.53s (new) 0.21s

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

**Test Case**: `class_predictors1`

