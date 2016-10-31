Rule-Based Classifier (`JRip`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/JRip.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/JRip.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-71), `RWeka` (0.4-26)
 * tested on 2016-10-31 at 05:54


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-72), `RWeka` (0.4-26)
 * tested on 2016-10-30 at 23:13


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


Execution times: (old) 1.77s (new) 9s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                  
      mean  -5.505
      min  -16.139
      max    1.935
    correlation:  -0.1138 
</pre>

 * ***UNequal results for Sens***:

<pre>
    %differences (n-o):                  
      mean  -4.333
      min  -18.023
      max    9.697
    correlation:  -0.101 
</pre>

 * ***UNequal results for Spec***:

<pre>
    %differences (n-o):                  
      mean  -6.287
      min  -29.801
      max    9.420
    correlation:  -0.3414 
</pre>


**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 3.15s (new) 12.75s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                  
      mean  -5.505
      min  -16.139
      max    1.935
    correlation:  -0.1138 
</pre>

 * ***UNequal results for Sens***:

<pre>
    %differences (n-o):                  
      mean  -4.333
      min  -18.023
      max    9.697
    correlation:  -0.101 
</pre>

 * ***UNequal results for Spec***:

<pre>
    %differences (n-o):                  
      mean  -6.287
      min  -29.801
      max    9.420
    correlation:  -0.3414 
</pre>


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


Execution times: (old) 17.93s (new) 2.77m

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                 
      mean  5.718
      min  -6.870
      max  29.245
    correlation:  0.1632 
</pre>

 * ***UNequal results for Sens***:

<pre>
    %differences (n-o):                 
      mean  -1.44
      min  -11.11
      max   11.11
    correlation:  NA 
</pre>

 * ***UNequal results for Spec***:

<pre>
    %differences (n-o):                 
      mean  3.227
      min  -5.882
      max  25.000
    correlation:  0.2369 
</pre>


**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.57s (new) 0.59s

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

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

