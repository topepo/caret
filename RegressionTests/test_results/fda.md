Flexible Discriminant Analysis (`fda`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/fda.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `class` (7.3-14), `earth` (4.4.4), `mda` (0.4-8), `plotmo` (3.1.4), `plotrix` (3.6-1), `TeachingDemos` (2.10)
 * tested on 2016-06-10 at 16:57


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `class` (7.3-14), `earth` (4.4.4), `mda` (0.4-8), `plotmo` (3.1.4), `plotrix` (3.6-1), `TeachingDemos` (2.10)
 * tested on 2016-06-12 at 12:32


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


Execution times: (old) 1.08s (new) 1.07s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                
      mean 1.413
      min  0.000
      max  4.239
    correlation:  1 

 nprune degree ROC_new ROC_old Diff Flag
     14      1  0.8565  0.8565  0.0     
      2      1  0.6403  0.6143  4.2    *
      8      1  0.8565  0.8565  0.0     

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
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 2.03s (new) 1.98s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                
      mean 1.413
      min  0.000
      max  4.239
    correlation:  1 

 nprune degree ROC_new ROC_old Diff Flag
     14      1  0.8565  0.8565  0.0     
      2      1  0.6403  0.6143  4.2    *
      8      1  0.8565  0.8565  0.0     

</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

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


Execution times: (old) 8.65s (new) 8.4s

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


Execution times: (old) 0.57s (new) 0.61s

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
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.01s (new) 1.18s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

