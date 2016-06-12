Robust Regularized Linear Discriminant Analysis (`rrlda`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rrlda.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `glasso` (1.8), `matrixcalc` (1.0-3), `mvoutlier` (2.0.6), `pcaPP` (1.9-60), `rrlda` (1.1), `sgeostat` (1.0-27)
 * tested on 2016-06-10 at 17:42


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `glasso` (1.8), `matrixcalc` (1.0-3), `mvoutlier` (2.0.6), `pcaPP` (1.9-60), `rrlda` (1.1), `sgeostat` (1.0-27)
 * tested on 2016-06-12 at 13:20


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


Execution times: (old) 9.51s (new) 9.35s

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
 * Pre-processing: centered (7), scaled (7)  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 11.88s (new) 11.52s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences < 0.1%
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


Execution times: (old) 2.46m (new) 2.43m

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                  
      mean -19.089
      min  -44.913
      max    5.592
    correlation:  -0.7635 

 lambda   hp penalty ROC_new ROC_old  Diff Flag
   0.25 0.50      L2  0.5536  0.5536   0.0     
   0.25 0.75      L2  0.4400  0.5600 -21.4    *
   0.25 1.00      L2  0.3552  0.6448 -44.9    *
   0.50 0.50      L2  0.5136  0.4864   5.6    *
   0.50 0.75      L2  0.4528  0.5472 -17.3    *
   0.50 1.00      L2  0.3648  0.6352 -42.6    *
   0.75 0.50      L2  0.5616  0.5616   0.0     
   0.75 0.75      L2  0.4672  0.5328 -12.3    *
   0.75 1.00      L2  0.3792  0.6208 -38.9    *

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


Execution times: (old) 0.98s (new) 0.92s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 94.8% (474/500)
        newer
older    Class1 Class2
  Class1    286     18
  Class2      8    188
</pre>

**Test Case**: `class_none_prob`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1            Class2       
 Min.   :-1.8162   Min.   :-2.0891  
 1st Qu.:-0.4500   1st Qu.:-0.3251  
 Median : 0.1501   Median : 0.2781  
 Mean   : 0.1555   Mean   : 0.2959  
 3rd Qu.: 0.7355   3rd Qu.: 0.9088  
 Max.   : 2.3343   Max.   : 2.3260  
</pre>

**Test Case**: `class_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 94.8% (474/500)
        newer
older    Class1 Class2
  Class1    286     18
  Class2      8    188
</pre>

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 94.8% (474/500)
        newer
older    Class1 Class2
  Class1    286     18
  Class2      8    188
</pre>

**Test Case**: `class_prob`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1            Class2       
 Min.   :-1.8162   Min.   :-2.0891  
 1st Qu.:-0.4500   1st Qu.:-0.3251  
 Median : 0.1501   Median : 0.2781  
 Mean   : 0.1555   Mean   : 0.2959  
 3rd Qu.: 0.7355   3rd Qu.: 0.9088  
 Max.   : 2.3343   Max.   : 2.3260  
</pre>

**Test Case**: `class_prob_form`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1            Class2       
 Min.   :-1.8162   Min.   :-2.0891  
 1st Qu.:-0.4500   1st Qu.:-0.3251  
 Median : 0.1501   Median : 0.2781  
 Mean   : 0.1555   Mean   : 0.2959  
 3rd Qu.: 0.7355   3rd Qu.: 0.9088  
 Max.   : 2.3343   Max.   : 2.3260  
</pre>

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: centered (7), scaled (7)  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 6.15s (new) 5.95s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

