Regularized Discriminant Analysis (`rda`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rda.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `klaR` (0.6-12), `MASS` (7.3-45)
 * tested on 2016-06-10 at 17:35


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `klaR` (0.6-12), `MASS` (7.3-45)
 * tested on 2016-06-12 at 13:13


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 1.32s (new) 1.36s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                
      mean  5.25
      min   0.00
      max  13.97
    correlation:  0.9667 

 gamma lambda ROC_new ROC_old Diff Flag
   0.0    0.0  0.6800  0.5966 14.0    *
   0.0    0.5  0.7106  0.7106  0.0     
   0.0    1.0  0.7477  0.7477  0.0     
   0.5    0.0  0.8003  0.8003  0.0     
   0.5    0.5  0.7517  0.7517  0.0     
   0.5    1.0  0.6644  0.5995 10.8    *
   1.0    0.0  0.6846  0.6383  7.3    *
   1.0    0.5  0.6603  0.6233  5.9    *
   1.0    1.0  0.6545  0.5990  9.3    *

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
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 2.49s (new) 2.44s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                
      mean  5.25
      min   0.00
      max  13.97
    correlation:  0.9667 

 gamma lambda ROC_new ROC_old Diff Flag
   0.0    0.0  0.6800  0.5966 14.0    *
   0.0    0.5  0.7106  0.7106  0.0     
   0.0    1.0  0.7477  0.7477  0.0     
   0.5    0.0  0.8003  0.8003  0.0     
   0.5    0.5  0.7517  0.7517  0.0     
   0.5    1.0  0.6644  0.5995 10.8    *
   1.0    0.0  0.6846  0.6383  7.3    *
   1.0    0.5  0.6603  0.6233  5.9    *
   1.0    1.0  0.6545  0.5990  9.3    *

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
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 18.89s (new) 18.4s

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


Execution times: (old) 0.48s (new) 0.53s

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

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 0.96s (new) 0.97s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

