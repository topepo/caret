Oblique Trees (`oblique.tree`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/oblique.tree.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `foreach` (1.4.3), `glmnet` (2.0-5), `Matrix` (1.2-6), `oblique.tree` (1.1.1), `tree` (1.0-37)
 * tested on 2016-06-10 at 17:26


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `foreach` (1.4.3), `glmnet` (2.0-5), `Matrix` (1.2-6), `oblique.tree` (1.1.1), `tree` (1.0-37)
 * tested on 2016-06-12 at 13:01


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


Execution times: (old) 3.72s (new) 3.69s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                 
      mean  6.933
      min   0.000
      max  20.799
    correlation:  0.9295 

 oblique.splits  variable.selection ROC_new ROC_old Diff Flag
            off model.selection.aic  0.7375  0.6105 20.8    *
            off                none  0.7375  0.6105 20.8    *
             on model.selection.aic  0.8086  0.8086  0.0     
             on                none  0.8245  0.8245  0.0     
           only model.selection.aic  0.7742  0.7742  0.0     
           only                none  0.7742  0.7742  0.0     

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


Execution times: (old) 4.68s (new) 4.65s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                 
      mean  6.933
      min   0.000
      max  20.799
    correlation:  0.9295 

 oblique.splits  variable.selection ROC_new ROC_old Diff Flag
            off model.selection.aic  0.7375  0.6105 20.8    *
            off                none  0.7375  0.6105 20.8    *
             on model.selection.aic  0.8086  0.8086  0.0     
             on                none  0.8245  0.8245  0.0     
           only model.selection.aic  0.7742  0.7742  0.0     
           only                none  0.7742  0.7742  0.0     

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


Execution times: (old) 1.47m (new) 1.46m

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


Execution times: (old) 0.81s (new) 0.72s

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
 * Pre-processing: None  
 * 6 tuning parameter combinations were evaluated


Execution times: (old) 3.18s (new) 3.14s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

