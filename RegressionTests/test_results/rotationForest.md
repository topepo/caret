Rotation Forest (`rotationForest`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rotationForest.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/rotationForest.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-71), `rotationForest` (0.1), `rpart` (4.1-10)
 * tested on 2016-10-31 at 06:33


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-72), `rotationForest` (0.1), `rpart` (4.1-10)
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
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 3.27s (new) 1.5s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    3 missing values in old
    6 missing values in new
    %differences (n-o):                
      mean 5.890
      min  3.218
      max  9.749
    correlation:  0.4405 
</pre>

 * ***UNequal results for Sens***:

<pre>
    3 missing values in old
    9 missing values in new
    %differences (n-o):                 
      mean  1.851
      min  -6.452
      max   6.164
    correlation:  0 
</pre>

 * ***UNequal results for Spec***:

<pre>
    3 missing values in old
    9 missing values in new
    %differences (n-o):                  
      mean   6.602
      min  -15.753
      max   21.739
    correlation:  -0.9686 
</pre>


**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 3.75s (new) 2.47s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    3 missing values in old
    6 missing values in new
    %differences (n-o):                
      mean 5.890
      min  3.218
      max  9.749
    correlation:  0.4405 
</pre>

 * ***UNequal results for Sens***:

<pre>
    3 missing values in old
    9 missing values in new
    %differences (n-o):                 
      mean  1.851
      min  -6.452
      max   6.164
    correlation:  0 
</pre>

 * ***UNequal results for Spec***:

<pre>
    3 missing values in old
    9 missing values in new
    %differences (n-o):                  
      mean   6.602
      min  -15.753
      max   21.739
    correlation:  -0.9686 
</pre>


**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.61s (new) 0.61s

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
 * Pre-processing: centered (7), scaled (7)  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 13.9s (new) 8.26s

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

 * _Equal results_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

