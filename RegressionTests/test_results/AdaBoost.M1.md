AdaBoost.M1 (`AdaBoost.M1`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/AdaBoost.M1.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `adabag` (4.1), `caret` (6.0-68), `mlbench` (2.1-1), `plyr` (1.8.4), `rpart` (4.1-10)
 * tested on 2016-06-10 at 16:46


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `adabag` (4.1), `caret` (6.0-70), `mlbench` (2.1-1), `plyr` (1.8.4), `rpart` (4.1-10)
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
 * 18 tuning parameter combinations were evaluated


Execution times: (old) 1.2m (new) 1.67m

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                  
      mean   2.494
      min  -25.256
      max   39.450
    correlation:  0.5126 
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
 * 18 tuning parameter combinations were evaluated


Execution times: (old) 1.24m (new) 1.59m

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                  
      mean   2.494
      min  -25.256
      max   39.450
    correlation:  0.5126 
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
 * 18 tuning parameter combinations were evaluated


Execution times: (old) 18.72m (new) 18.79m

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


Execution times: (old) 4.36s (new) 4.15s

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
   Agreement: 56.2% (281/500)
        newer
older    Class1 Class2
  Class1    133    130
  Class2     89    148
</pre>

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 56.2% (281/500)
        newer
older    Class1 Class2
  Class1    133    130
  Class2     89    148
</pre>

**Test Case**: `class_prob`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1             Class2        
 Min.   :-1.00000   Min.   :-1.00000  
 1st Qu.:-0.13296   1st Qu.:-0.31812  
 Median : 0.10396   Median :-0.10396  
 Mean   : 0.09788   Mean   :-0.09788  
 3rd Qu.: 0.31812   3rd Qu.: 0.13296  
 Max.   : 1.00000   Max.   : 1.00000  
</pre>

**Test Case**: `class_prob_form`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1             Class2        
 Min.   :-1.00000   Min.   :-1.00000  
 1st Qu.:-0.13296   1st Qu.:-0.31812  
 Median : 0.10396   Median :-0.10396  
 Mean   : 0.09788   Mean   :-0.09788  
 3rd Qu.: 0.31812   3rd Qu.: 0.13296  
 Max.   : 1.00000   Max.   : 1.00000  
</pre>

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: centered (7), scaled (7)  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 42.52m (new) 42.77m

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

