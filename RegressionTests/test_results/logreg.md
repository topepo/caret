Logic Regression (`logreg`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/logreg.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/logreg.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-71), `LogicReg` (1.5.9), `survival` (2.39-5)
 * tested on 2016-10-31 at 05:58


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-72), `LogicReg` (1.5.9), `survival` (2.39-5)
 * tested on 2016-10-30 at 23:17


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: None  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 10.08s (new) 10.58s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: None  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 10.63s (new) 11.5s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: None  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 2.11m (new) 2.13m

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: None  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.84s (new) 0.92s

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
 * Pre-processing: None  
 * 12 tuning parameter combinations were evaluated


Execution times: (old) 13.88s (new) 14.57s

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
 * Pre-processing: None  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 5.34s (new) 5.77s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: None  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 5.32s (new) 5.71s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: None  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 49.57s (new) 50.05s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    %differences (n-o):                  
      mean  0.4749
      min  -2.5731
      max   7.2949
    correlation:  0.9815 

 treesize ntrees RMSE_new RMSE_old Diff Flag
       16      2    21.01    21.11 -0.4    *
       16      3    13.91    13.91  0.0     
       16      4    19.19    19.08  0.6    *
        4      2    17.69    17.69  0.0     
        4      3    22.41    22.41  0.0     
        4      4    20.99    21.55 -2.6    *
        8      2    21.02    20.92  0.5    *
        8      3    21.20    19.76  7.3    *
        8      4    15.99    16.16 -1.0    *

</pre>

 * ***UNequal results for Rsquared***:

<pre>
    %differences (n-o):                  
      mean  -5.006
      min  -64.826
      max   32.375
    correlation:  0.995 

 treesize ntrees Rsquared_new Rsquared_old  Diff Flag
       16      2     0.002391     0.002042  17.1    *
       16      3     0.402023     0.402023   0.0     
       16      4     0.080046     0.060469  32.4    *
        4      2     0.155808     0.155808   0.0     
        4      3     0.028279     0.028279   0.0     
        4      4     0.050388     0.054240  -7.1    *
        8      2     0.025058     0.034248 -26.8    *
        8      3     0.017632     0.050129 -64.8    *
        8      4     0.278847     0.267560   4.2    *

</pre>


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
 * 12 tuning parameter combinations were evaluated


Execution times: (old) 8.05s (new) 8.17s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

