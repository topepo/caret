Generalized Additive Model using Splines (`gamSpline`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/gamSpline.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `foreach` (1.4.3), `gam` (1.12)
 * tested on 2016-06-10 at 17:01


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `foreach` (1.4.3), `gam` (1.12)
 * tested on 2016-06-12 at 12:36


Results:
---------

**Test Case**: `class_cv_dist`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (2), scaled (2)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.14s (new) 1.18s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences < 0.1%
</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (2), scaled (2)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.03s (new) 1.01s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences < 0.1%
</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (2), scaled (2)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.66s (new) 1.68s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences < 0.1%
</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_imp`

Object class(es): `varImp.train`

 * _Equal results_

**Test Case**: `class_imp2`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
    Overall      
 Min.   :-3.233  
 1st Qu.:-3.118  
 Median :-3.003  
 Mean   :-3.003  
 3rd Qu.:-2.887  
 Max.   :-2.772  
</pre>

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (2), scaled (2)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 7.77s (new) 7.76s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                
      mean 12.55
      min   0.00
      max  37.64
    correlation:  0.09141 

 df ROC_new ROC_old Diff Flag
  1  0.5952  0.5952  0.0     
  2  0.5520  0.5520  0.0     
  3  0.5792  0.4208 37.6    *

</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (2), scaled (2)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.52s (new) 0.49s

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

 * ***UNequal results***:
<pre>
   Agreement: 80.8% (404/500)
        newer
older    Class1 Class2
  Class1    215     80
  Class2     16    189
</pre>

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 80.8% (404/500)
        newer
older    Class1 Class2
  Class1    215     80
  Class2     16    189
</pre>

**Test Case**: `class_predictors1`

Object class(es): `character`

 * _Equal results_

**Test Case**: `class_predictors2`

Object class(es): `character`

 * _Equal results_

**Test Case**: `class_prob`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1              Class2         
 Min.   :-0.266998   Min.   :-0.315527  
 1st Qu.:-0.096462   1st Qu.:-0.092417  
 Median :-0.012532   Median : 0.012532  
 Mean   : 0.005707   Mean   :-0.005707  
 3rd Qu.: 0.092417   3rd Qu.: 0.096462  
 Max.   : 0.315527   Max.   : 0.266998  
</pre>

**Test Case**: `class_prob_form`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1              Class2         
 Min.   :-0.266998   Min.   :-0.315527  
 1st Qu.:-0.096462   1st Qu.:-0.092417  
 Median :-0.012532   Median : 0.012532  
 Mean   : 0.005707   Mean   :-0.005707  
 3rd Qu.: 0.092417   3rd Qu.: 0.096462  
 Max.   : 0.315527   Max.   : 0.266998  
</pre>

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 1.14s (new) 1.12s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

**Test Case**: `reg_cv_dist`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.05s (new) 1.07s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 0.94s (new) 0.84s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 0.98s (new) 0.93s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_imp`

Object class(es): `varImp.train`

 * _Equal results_

**Test Case**: `reg_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 3.33s (new) 3.3s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.47s (new) 0.48s

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

**Test Case**: `reg_predictors1`

Object class(es): `character`

 * _Equal results_

**Test Case**: `reg_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 0.91s (new) 0.91s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

