Cumulative Probability Model for Ordinal Data (`vglmCumulative`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/vglmCumulative.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/vglmCumulative.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `VGAM` (1.0-3)
 * tested on 2017-04-12 at 19:57. 
 * total test time: 43.3s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `VGAM` (1.0-3)
 * tested on 2017-04-11 at 20:50. 
 * total test time: 46.7s


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (15), scaled (15)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 1.23s (new) 1.11s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (15), scaled (15)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 2.19s (new) 1.87s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_cv_weight`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (15), scaled (15)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 1.32s (new) 1.14s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_imp`

Object class(es): `varImp.train`

 * ***UNequal results***: differences (o-n):
<pre>
      low               med              high       
 Min.   :-86.008   Min.   :-12.42   Min.   :-50.89  
 1st Qu.: -2.026   1st Qu.:  1.67   1st Qu.:-10.90  
 Median : 25.448   Median : 14.15   Median : 26.25  
 Mean   :  8.858   Mean   : 14.66   Mean   : 10.07  
 3rd Qu.: 32.520   3rd Qu.: 28.01   3rd Qu.: 34.66  
 Max.   : 35.200   Max.   : 36.16   Max.   : 36.86  
</pre>

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (15), scaled (15)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 18.61s (new) 21.16s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_loo_weight`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (15), scaled (15)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 16.48s (new) 17.86s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (15), scaled (15)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.61s (new) 0.3s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_none_pred`

Object class(es): `ordered` and `factor`

 * _Equal results_

**Test Case**: `class_none_prob`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `class_pred`

Object class(es): `ordered` and `factor`

 * _Equal results_

**Test Case**: `class_pred_form`

Object class(es): `ordered` and `factor`

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

