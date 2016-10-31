Rotation Forest (`rotationForestCp`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rotationForestCp.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/rotationForestCp.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-71), `plyr` (1.8.4), `rotationForest` (0.1), `rpart` (4.1-10)
 * tested on 2016-10-31 at 06:33


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-72), `plyr` (1.8.4), `rotationForest` (0.1), `rpart` (4.1-10)
 * tested on 2016-10-30 at 23:58


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 27 tuning parameter combinations were evaluated


Execution times: (old) 7.28s (new) 3.11s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Sens***:

<pre>
    9 missing values in old
    27 missing values in new
    %differences (n-o):                  
      mean  -1.194
      min  -15.951
      max   15.126
    correlation:  0.4023 
</pre>

 * ***UNequal results for Spec***:

<pre>
    %differences < 0.1%
</pre>


**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 27 tuning parameter combinations were evaluated


Execution times: (old) 8.67s (new) 4.28s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Sens***:

<pre>
    9 missing values in old
    27 missing values in new
    %differences (n-o):                  
      mean  -1.194
      min  -15.951
      max   15.126
    correlation:  0.4023 
</pre>

 * ***UNequal results for Spec***:

<pre>
    %differences < 0.1%
</pre>


**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.84s (new) 0.61s

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
   Agreement: 86.4% (432/500)
        newer
older    Class1 Class2
  Class1    186     58
  Class2     10    246
</pre>

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 86.4% (432/500)
        newer
older    Class1 Class2
  Class1    186     58
  Class2     10    246
</pre>

**Test Case**: `class_prob`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1             Class2        
 Min.   :-0.31465   Min.   :-0.36365  
 1st Qu.:-0.07004   1st Qu.:-0.10059  
 Median : 0.02335   Median :-0.02335  
 Mean   : 0.02024   Mean   :-0.02024  
 3rd Qu.: 0.10059   3rd Qu.: 0.07004  
 Max.   : 0.36365   Max.   : 0.31465  
</pre>

**Test Case**: `class_prob_form`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1             Class2        
 Min.   :-0.31465   Min.   :-0.36365  
 1st Qu.:-0.07004   1st Qu.:-0.10059  
 Median : 0.02335   Median :-0.02335  
 Mean   : 0.02024   Mean   :-0.02024  
 3rd Qu.: 0.10059   3rd Qu.: 0.07004  
 Max.   : 0.36365   Max.   : 0.31465  
</pre>

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: centered (7), scaled (7)  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 21.07s (new) 12.19s

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences < 0.1%
</pre>


**Test Case**: `imp`

Object class(es): `varImp.train`

 * ***UNequal results***: differences (o-n):
<pre>
    Overall        
 Min.   :-2.91079  
 1st Qu.:-1.60425  
 Median :-0.70849  
 Mean   :-1.06426  
 3rd Qu.:-0.26985  
 Max.   :-0.08233  
</pre>

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

