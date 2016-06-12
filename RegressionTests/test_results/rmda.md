Robust Mixture Discriminant Analysis (`rmda`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rmda.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `MASS` (7.3-45), `mclust` (5.2), `robustDA` (1.1), `Rsolnp` (1.16)
 * tested on 2016-06-10 at 17:39


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `MASS` (7.3-45), `mclust` (5.2), `robustDA` (1.1), `Rsolnp` (1.16)
 * tested on 2016-06-12 at 13:18


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


Execution times: (old) 4.55s (new) 4.36s

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
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 5.37s (new) 5.29s

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
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 1.23m (new) 1.19m

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


Execution times: (old) 0.95s (new) 1.11s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 90.4% (452/500)
        newer
older    Class1 Class2
  Class1    294     31
  Class2     17    158
</pre>

**Test Case**: `class_none_prob`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1               Class2          
 Min.   :-0.9999942   Min.   :-0.9999944  
 1st Qu.: 0.0000000   1st Qu.:-0.0005471  
 Median : 0.0000000   Median : 0.0000000  
 Mean   : 0.0248565   Mean   :-0.0248565  
 3rd Qu.: 0.0005471   3rd Qu.: 0.0000000  
 Max.   : 0.9999944   Max.   : 0.9999942  
</pre>

**Test Case**: `class_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 90.4% (452/500)
        newer
older    Class1 Class2
  Class1    294     31
  Class2     17    158
</pre>

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 90.4% (452/500)
        newer
older    Class1 Class2
  Class1    294     31
  Class2     17    158
</pre>

**Test Case**: `class_prob`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1               Class2          
 Min.   :-0.9999942   Min.   :-0.9999944  
 1st Qu.: 0.0000000   1st Qu.:-0.0005471  
 Median : 0.0000000   Median : 0.0000000  
 Mean   : 0.0248565   Mean   :-0.0248565  
 3rd Qu.: 0.0005471   3rd Qu.: 0.0000000  
 Max.   : 0.9999944   Max.   : 0.9999942  
</pre>

**Test Case**: `class_prob_form`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1               Class2          
 Min.   :-0.9999942   Min.   :-0.9999944  
 1st Qu.: 0.0000000   1st Qu.:-0.0005471  
 Median : 0.0000000   Median : 0.0000000  
 Mean   : 0.0248565   Mean   :-0.0248565  
 3rd Qu.: 0.0005471   3rd Qu.: 0.0000000  
 Max.   : 0.9999944   Max.   : 0.9999942  
</pre>

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

