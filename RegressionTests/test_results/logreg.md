Logic Regression (`logreg`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/logreg.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/logreg.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `LogicReg` (1.5.9), `survival` (2.40-1)
 * tested on 2017-04-12 at 20:33. 
 * total test time: 241.4s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `LogicReg` (1.5.9), `survival` (2.40-1)
 * tested on 2017-04-11 at 20:56. 
 * total test time: 235.9s


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


Execution times: (old) 10.72s (new) 11.27s

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


Execution times: (old) 11.43s (new) 11.34s

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


Execution times: (old) 2.17m (new) 2.28m

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


Execution times: (old) 0.88s (new) 0.6s

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


Execution times: (old) 14.93s (new) 4.22s

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    0 missing values in old
    8 missing values in new
    %differences (n-o):                 
      mean -2.404
      min  -9.856
      max   9.178
    correlation:  0.6488 
</pre>

 * ***UNequal results for Kappa***:

<pre>
    0 missing values in old
    8 missing values in new
    %differences (n-o):                 
      mean -14.15
      min  -61.73
      max   46.54
    correlation:  0.5513 
</pre>


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


Execution times: (old) 5.43s (new) 5.53s

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


Execution times: (old) 5.6s (new) 5.89s

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


Execution times: (old) 51.42s (new) 54.44s

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
 * 12 tuning parameter combinations were evaluated


Execution times: (old) 8.04s (new) 2.3s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    0 missing values in old
    8 missing values in new
    %differences (n-o):                 
      mean  4.290
      min  -2.993
      max   9.521
    correlation:  0.8894 
</pre>

 * ***UNequal results for Rsquared***:

<pre>
    0 missing values in old
    8 missing values in new
    %differences (n-o):                 
      mean -24.91
      min  -44.97
      max   15.98
    correlation:  0.7063 
</pre>


