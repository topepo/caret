Boosted Logistic Regression (`LogitBoost`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/LogitBoost.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `caTools` (1.17.1)
 * tested on 2016-06-10 at 17:15


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `caTools` (1.17.1)
 * tested on 2016-06-12 at 12:50


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


Execution times: (old) 0.82s (new) 0.78s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                 
      mean  6.497
      min   0.000
      max  19.491
    correlation:  -0.9035 

 nIter ROC_new ROC_old Diff Flag
    11  0.7869  0.7869  0.0     
    21  0.8109  0.6786 19.5    *
    31  0.7958  0.7958  0.0     

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


Execution times: (old) 1.38s (new) 1.36s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                 
      mean  6.497
      min   0.000
      max  19.491
    correlation:  -0.9035 

 nIter ROC_new ROC_old Diff Flag
    11  0.7869  0.7869  0.0     
    21  0.8109  0.6786 19.5    *
    31  0.7958  0.7958  0.0     

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


Execution times: (old) 3.08s (new) 3.03s

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


Execution times: (old) 0.49s (new) 0.48s

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
   Agreement: 92.2% (461/500)
        newer
older    Class1 Class2
  Class1    230     19
  Class2     20    231
</pre>

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 92.2% (461/500)
        newer
older    Class1 Class2
  Class1    230     19
  Class2     20    231
</pre>

**Test Case**: `class_predictors1`

Object class(es): `character`

 * _Equal results_

**Test Case**: `class_prob`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1               Class2          
 Min.   :-0.6836327   Min.   :-0.6836327  
 1st Qu.:-0.0057818   1st Qu.:-0.0007877  
 Median : 0.0000000   Median : 0.0000000  
 Mean   :-0.0034082   Mean   : 0.0034082  
 3rd Qu.: 0.0007877   3rd Qu.: 0.0057818  
 Max.   : 0.6836327   Max.   : 0.6836327  
</pre>

**Test Case**: `class_prob_form`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1               Class2          
 Min.   :-0.6836327   Min.   :-0.6836327  
 1st Qu.:-0.0057818   1st Qu.:-0.0007877  
 Median : 0.0000000   Median : 0.0000000  
 Mean   :-0.0034082   Mean   : 0.0034082  
 3rd Qu.: 0.0007877   3rd Qu.: 0.0057818  
 Max.   : 0.6836327   Max.   : 0.6836327  
</pre>

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 0.88s (new) 0.9s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

