Quantile Regression Neural Network (`qrnn`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/qrnn.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/qrnn.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `qrnn` (1.1.3)
 * tested on 2017-04-12 at 19:38. 
 * total test time: 209.6s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `qrnn` (1.1.3)
 * tested on 2017-04-11 at 21:42. 
 * total test time: 173.6s


Results:
---------

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 12.26s (new) 10.55s

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
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 13.31s (new) 11.06s

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
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 2.66m (new) 2.41m

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


Execution times: (old) 1.24s (new) 0.74s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `reg_pred`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `reg_pred_form`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `reg_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 16 tuning parameter combinations were evaluated


Execution times: (old) 20.48s (new) 4.3s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    0 missing values in old
    12 missing values in new
    %differences (n-o):                  
      mean -3.7788
      min  -7.4574
      max   0.2439
    correlation:  0.9691 
</pre>

 * ***UNequal results for Rsquared***:

<pre>
    0 missing values in old
    12 missing values in new
    %differences (n-o):                 
      mean -16.71
      min  -49.27
      max   38.44
    correlation:  0.5801 
</pre>


