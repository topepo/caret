Nearest Shrunken Centroids (`pam`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/pam.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `cluster` (2.0.4), `pamr` (1.55), `survival` (2.39-2)
 * tested on 2016-06-10 at 17:29


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `cluster` (2.0.4), `pamr` (1.55), `survival` (2.39-2)
 * tested on 2016-06-12 at 13:06


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


Execution times: (old) 0.85s (new) 0.81s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                
      mean 3.143
      min  0.000
      max  9.430
    correlation:  0.9219 

 threshold ROC_new ROC_old Diff Flag
   0.04848  0.6447  0.5891  9.4    *
   0.70302  0.6377  0.6377  0.0     
   1.35755  0.5000  0.5000  0.0     

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


Execution times: (old) 1.48s (new) 1.39s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                
      mean 3.143
      min  0.000
      max  9.430
    correlation:  0.9219 

 threshold ROC_new ROC_old Diff Flag
   0.04848  0.6447  0.5891  9.4    *
   0.70302  0.6377  0.6377  0.0     
   1.35755  0.5000  0.5000  0.0     

</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_imp`

Object class(es): `varImp.train`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1            Class2       
 Min.   :-44.332   Min.   :-44.332  
 1st Qu.:-27.586   1st Qu.:-27.586  
 Median : -4.124   Median : -4.124  
 Mean   :-15.209   Mean   :-15.209  
 3rd Qu.: -1.416   3rd Qu.: -1.416  
 Max.   :  0.000   Max.   :  0.000  
</pre>

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 2.74s (new) 2.68s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences < 0.1%
</pre>

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


Execution times: (old) 0.49s (new) 0.5s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 86.2% (431/500)
        newer
older    Class1 Class2
  Class1    200     31
  Class2     38    231
</pre>

**Test Case**: `class_none_prob`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1               Class2          
 Min.   :-0.1183529   Min.   :-0.1041396  
 1st Qu.:-0.0221984   1st Qu.:-0.0247535  
 Median : 0.0015757   Median :-0.0015757  
 Mean   : 0.0004728   Mean   :-0.0004728  
 3rd Qu.: 0.0247535   3rd Qu.: 0.0221984  
 Max.   : 0.1041396   Max.   : 0.1183529  
</pre>

**Test Case**: `class_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 86.2% (431/500)
        newer
older    Class1 Class2
  Class1    200     31
  Class2     38    231
</pre>

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 86.2% (431/500)
        newer
older    Class1 Class2
  Class1    200     31
  Class2     38    231
</pre>

**Test Case**: `class_predictors1`

Object class(es): `character`

 * ***UNequal results***:
<pre>
   Agreement: 14.3% (1/7)
