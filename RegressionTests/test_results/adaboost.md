AdaBoost Classification Trees (`adaboost`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/adaboost.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `fastAdaboost` (1.0.0)
 * tested on 2016-06-10 at 16:46


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `fastAdaboost` (1.0.0)
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


Execution times: (old) 23.27s (new) 23.27s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                  
      mean  -2.060
      min  -11.480
      max    3.579
    correlation:  0.99 

 nIter        method ROC_new ROC_old  Diff Flag
   100   Adaboost.M1  0.9300  0.9300   0.0     
   100 Real adaboost  0.6030  0.5822   3.6    *
   150   Adaboost.M1  0.9485  0.9485   0.0     
   150 Real adaboost  0.5087  0.5747 -11.5    *
    50   Adaboost.M1  0.9537  0.9537   0.0     
    50 Real adaboost  0.6696  0.7008  -4.5    *

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


Execution times: (old) 24.18s (new) 24.77s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                  
      mean  -2.060
      min  -11.480
      max    3.579
    correlation:  0.99 

 nIter        method ROC_new ROC_old  Diff Flag
   100   Adaboost.M1  0.9300  0.9300   0.0     
   100 Real adaboost  0.6030  0.5822   3.6    *
   150   Adaboost.M1  0.9485  0.9485   0.0     
   150 Real adaboost  0.5087  0.5747 -11.5    *
    50   Adaboost.M1  0.9537  0.9537   0.0     
    50 Real adaboost  0.6696  0.7008  -4.5    *

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


Execution times: (old) 7.68m (new) 7.8m

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


Execution times: (old) 1.86s (new) 1.95s

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


Execution times: (old) 1.08m (new) 1.11m

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

