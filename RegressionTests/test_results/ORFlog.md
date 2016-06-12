Oblique Random Forest (`ORFlog`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/ORFlog.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `class` (7.3-14), `e1071` (1.6-7), `gplots` (3.0.1), `mda` (0.4-8), `obliqueRF` (0.3), `pls` (2.5-0), `ROCR` (1.0-7)
 * tested on 2016-06-10 at 17:26


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `class` (7.3-14), `e1071` (1.6-7), `gplots` (3.0.1), `mda` (0.4-8), `obliqueRF` (0.3), `pls` (2.5-0), `ROCR` (1.0-7)
 * tested on 2016-06-12 at 13:03


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


Execution times: (old) 8.46s (new) 8.38s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                 
      mean  4.213
      min   0.000
      max  12.640
    correlation:  0.8174 

 mtry ROC_new ROC_old Diff Flag
    2  0.8611  0.8611  0.0     
    4  0.7839  0.6959 12.6    *
    7  0.7269  0.7269  0.0     

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


Execution times: (old) 9.1s (new) 9.25s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                 
      mean  4.213
      min   0.000
      max  12.640
    correlation:  0.8174 

 mtry ROC_new ROC_old Diff Flag
    2  0.8611  0.8611  0.0     
    4  0.7839  0.6959 12.6    *
    7  0.7269  0.7269  0.0     

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
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 2.64m (new) 2.54m

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


Execution times: (old) 1.51s (new) 1.46s

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
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 6.1s (new) 6.13s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

