Bagged AdaBoost (`AdaBag`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/AdaBag.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `adabag` (4.1), `caret` (6.0-68), `mlbench` (2.1-1), `plyr` (1.8.4), `rpart` (4.1-10)
 * tested on 2016-06-10 at 16:46


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `adabag` (4.1), `caret` (6.0-70), `mlbench` (2.1-1), `plyr` (1.8.4), `rpart` (4.1-10)
 * tested on 2016-06-12 at 12:22


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 6 tuning parameter combinations were evaluated


Execution times: (old) 26.21s (new) 35.59s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                
      mean  3.31
      min   0.00
      max  15.71
    correlation:  0.9272 

 maxdepth mfinal ROC_new ROC_old Diff Flag
        1      3  0.6904  0.5966 15.7    *
        1      6  0.7358  0.7358  0.0     
        1      9  0.6973  0.6973  0.0     
        3      3  0.6973  0.6696  4.1    *
        3      6  0.7948  0.7948  0.0     
        3      9  0.8281  0.8281  0.0     

</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 6 tuning parameter combinations were evaluated


Execution times: (old) 27.16s (new) 36.54s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                
      mean  3.31
      min   0.00
      max  15.71
    correlation:  0.9272 

 maxdepth mfinal ROC_new ROC_old Diff Flag
        1      3  0.6904  0.5966 15.7    *
        1      6  0.7358  0.7358  0.0     
        1      9  0.6973  0.6973  0.0     
        3      3  0.6973  0.6696  4.1    *
        3      6  0.7948  0.7948  0.0     
        3      9  0.8281  0.8281  0.0     

</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 6 tuning parameter combinations were evaluated


Execution times: (old) 5.65m (new) 5.8m

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


Execution times: (old) 16.7s (new) 16.89s

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


Execution times: (old) 35.78m (new) 36.91m

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

