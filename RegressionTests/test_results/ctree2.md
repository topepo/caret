Conditional Inference Tree (`ctree2`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/ctree2.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `modeltools` (0.2-21), `mvtnorm` (1.0-5), `party` (1.0-25), `sandwich` (2.3-4), `strucchange` (1.5-1), `zoo` (1.7-12)
 * tested on 2016-06-10 at 16:53


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `modeltools` (0.2-21), `mvtnorm` (1.0-5), `party` (1.0-25), `sandwich` (2.3-4), `strucchange` (1.5-1), `zoo` (1.7-12)
 * tested on 2016-06-12 at 12:28


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


Execution times: (old) 1.07s (new) 2.05s

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences < 0.1%
</pre>


**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 2.34s (new) 3.31s

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences < 0.1%
</pre>


**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 7.7s (new) 21.62s

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences < 0.1%
</pre>


**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.55s (new) 0.61s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_none_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 71.6% (358/500)
        newer
older    Class1 Class2
  Class1    358      0
  Class2    142      0
</pre>

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
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 1.14s (new) 1.3s

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences < 0.1%
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
 * Pre-processing: centered (20), scaled (20)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.17s (new) 2.05s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Rsquared***:

<pre>
    %differences < 0.1%
</pre>


**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.53s (new) 3.03s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Rsquared***:

<pre>
    %differences < 0.1%
</pre>


**Test Case**: `reg_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 8.04s (new) 23s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    %differences (n-o):                
      mean 2.796
      min  0.000
      max  8.389
    correlation:  NA 

 maxdepth mincriterion RMSE_new RMSE_old Diff Flag
        1         0.01    15.72    15.72  0.0     
        1         0.50    15.72    15.72  0.0     
        1         0.99    17.04    15.72  8.4    *
        2         0.01    15.72    15.72  0.0     
        2         0.50    15.72    15.72  0.0     
        2         0.99    17.04    15.72  8.4    *
        3         0.01    15.72    15.72  0.0     
        3         0.50    15.72    15.72  0.0     
        3         0.99    17.04    15.72  8.4    *

</pre>

 * ***UNequal results for Rsquared***:

<pre>
    %differences (n-o):                
      mean 197.0
      min    0.0
      max  591.1
    correlation:  NA 

 maxdepth mincriterion Rsquared_new Rsquared_old  Diff Flag
        1         0.01       0.1447       0.1447   0.0     
        1         0.50       0.1447       0.1447   0.0     
        1         0.99       1.0000       0.1447 591.1    *
        2         0.01       0.1447       0.1447   0.0     
        2         0.50       0.1447       0.1447   0.0     
        2         0.99       1.0000       0.1447 591.1    *
        3         0.01       0.1447       0.1447   0.0     
        3         0.50       0.1447       0.1447   0.0     
        3         0.99       1.0000       0.1447 591.1    *

</pre>


**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.56s (new) 0.65s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `numeric`

 * ***UNequal results***:
    %differences < 0.1%

**Test Case**: `reg_pred`

Object class(es): `numeric`

 * ***UNequal results***:
    %differences < 0.1%

**Test Case**: `reg_pred_form`

Object class(es): `numeric`

 * ***UNequal results***:
    %differences < 0.1%

**Test Case**: `reg_predictors1`

Object class(es): `character`

 * ***UNequal results***:
<pre>
   Agreement: NaN% (0/0)
