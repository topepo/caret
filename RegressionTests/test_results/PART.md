Rule-Based Classifier (`PART`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/PART.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/PART.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-71), `RWeka` (0.4-26)
 * tested on 2016-10-31 at 06:19


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-72), `RWeka` (0.4-26)
 * tested on 2016-10-30 at 23:43


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 1 tuning parameter combination was evaluated


Execution times: (old) 1.22s (new) 2.91s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    no pairs of results without missing values
  threshold pruned   ROC_new  Sens_new  Spec_new   ROC_old  Sens_old
1     0.010    yes 0.7042824 0.8009259 0.6388889        NA        NA
2     0.010     no 0.7042824 0.8009259 0.6388889        NA        NA
3     0.250    yes        NA        NA        NA 0.7042824 0.8009259
4     0.255    yes 0.7042824 0.8009259 0.6388889        NA        NA
5     0.255     no 0.7042824 0.8009259 0.6388889        NA        NA
6     0.500    yes 0.7042824 0.8009259 0.6388889        NA        NA
7     0.500     no 0.7042824 0.8009259 0.6388889        NA        NA
   Spec_old Diff
1        NA   NA
2        NA   NA
3 0.6388889   NA
4        NA   NA
5        NA   NA
6        NA   NA
7        NA   NA
</pre>

 * ***UNequal results for Sens***:

<pre>
    no pairs of results without missing values
  threshold pruned   ROC_new  Sens_new  Spec_new   ROC_old  Sens_old
1     0.010    yes 0.7042824 0.8009259 0.6388889        NA        NA
2     0.010     no 0.7042824 0.8009259 0.6388889        NA        NA
3     0.250    yes        NA        NA        NA 0.7042824 0.8009259
4     0.255    yes 0.7042824 0.8009259 0.6388889        NA        NA
5     0.255     no 0.7042824 0.8009259 0.6388889        NA        NA
6     0.500    yes 0.7042824 0.8009259 0.6388889        NA        NA
7     0.500     no 0.7042824 0.8009259 0.6388889        NA        NA
   Spec_old Diff
1        NA   NA
2        NA   NA
3 0.6388889   NA
4        NA   NA
5        NA   NA
6        NA   NA
7        NA   NA
</pre>

 * ***UNequal results for Spec***:

<pre>
    no pairs of results without missing values
  threshold pruned   ROC_new  Sens_new  Spec_new   ROC_old  Sens_old
1     0.010    yes 0.7042824 0.8009259 0.6388889        NA        NA
2     0.010     no 0.7042824 0.8009259 0.6388889        NA        NA
3     0.250    yes        NA        NA        NA 0.7042824 0.8009259
4     0.255    yes 0.7042824 0.8009259 0.6388889        NA        NA
5     0.255     no 0.7042824 0.8009259 0.6388889        NA        NA
6     0.500    yes 0.7042824 0.8009259 0.6388889        NA        NA
7     0.500     no 0.7042824 0.8009259 0.6388889        NA        NA
   Spec_old Diff
1        NA   NA
2        NA   NA
3 0.6388889   NA
4        NA   NA
5        NA   NA
6        NA   NA
7        NA   NA
</pre>


**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 1 tuning parameter combination was evaluated


Execution times: (old) 2.55s (new) 5.08s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    no pairs of results without missing values
  threshold pruned   ROC_new  Sens_new  Spec_new   ROC_old  Sens_old
1     0.010    yes 0.7042824 0.8009259 0.6388889        NA        NA
2     0.010     no 0.7042824 0.8009259 0.6388889        NA        NA
3     0.250    yes        NA        NA        NA 0.7042824 0.8009259
4     0.255    yes 0.7042824 0.8009259 0.6388889        NA        NA
5     0.255     no 0.7042824 0.8009259 0.6388889        NA        NA
6     0.500    yes 0.7042824 0.8009259 0.6388889        NA        NA
7     0.500     no 0.7042824 0.8009259 0.6388889        NA        NA
   Spec_old Diff
1        NA   NA
2        NA   NA
3 0.6388889   NA
4        NA   NA
5        NA   NA
6        NA   NA
7        NA   NA
</pre>

 * ***UNequal results for Sens***:

<pre>
    no pairs of results without missing values
  threshold pruned   ROC_new  Sens_new  Spec_new   ROC_old  Sens_old
1     0.010    yes 0.7042824 0.8009259 0.6388889        NA        NA
2     0.010     no 0.7042824 0.8009259 0.6388889        NA        NA
3     0.250    yes        NA        NA        NA 0.7042824 0.8009259
4     0.255    yes 0.7042824 0.8009259 0.6388889        NA        NA
5     0.255     no 0.7042824 0.8009259 0.6388889        NA        NA
6     0.500    yes 0.7042824 0.8009259 0.6388889        NA        NA
7     0.500     no 0.7042824 0.8009259 0.6388889        NA        NA
   Spec_old Diff
1        NA   NA
2        NA   NA
3 0.6388889   NA
4        NA   NA
5        NA   NA
6        NA   NA
7        NA   NA
</pre>

 * ***UNequal results for Spec***:

<pre>
    no pairs of results without missing values
  threshold pruned   ROC_new  Sens_new  Spec_new   ROC_old  Sens_old
1     0.010    yes 0.7042824 0.8009259 0.6388889        NA        NA
2     0.010     no 0.7042824 0.8009259 0.6388889        NA        NA
3     0.250    yes        NA        NA        NA 0.7042824 0.8009259
4     0.255    yes 0.7042824 0.8009259 0.6388889        NA        NA
5     0.255     no 0.7042824 0.8009259 0.6388889        NA        NA
6     0.500    yes 0.7042824 0.8009259 0.6388889        NA        NA
7     0.500     no 0.7042824 0.8009259 0.6388889        NA        NA
   Spec_old Diff
1        NA   NA
2        NA   NA
3 0.6388889   NA
4        NA   NA
5        NA   NA
6        NA   NA
7        NA   NA
</pre>


**Test Case**: `class_imp`

Object class(es): `varImp.train`

 * _Equal results_

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 1 tuning parameter combination was evaluated


Execution times: (old) 7.19s (new) 41.2s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    no pairs of results without missing values
  threshold pruned ROC_new Sens_new Spec_new ROC_old Sens_old
1     0.010    yes  0.6480     0.76      0.6      NA       NA
2     0.010     no  0.6032     0.76      0.6      NA       NA
3     0.250    yes      NA       NA       NA   0.648     0.76
4     0.255    yes  0.6480     0.76      0.6      NA       NA
5     0.255     no  0.6032     0.76      0.6      NA       NA
6     0.500    yes  0.6480     0.76      0.6      NA       NA
7     0.500     no  0.6032     0.76      0.6      NA       NA
  Spec_old Diff
1       NA   NA
2       NA   NA
3      0.6   NA
4       NA   NA
5       NA   NA
6       NA   NA
7       NA   NA
</pre>

 * ***UNequal results for Sens***:

<pre>
    no pairs of results without missing values
  threshold pruned ROC_new Sens_new Spec_new ROC_old Sens_old
1     0.010    yes  0.6480     0.76      0.6      NA       NA
2     0.010     no  0.6032     0.76      0.6      NA       NA
3     0.250    yes      NA       NA       NA   0.648     0.76
4     0.255    yes  0.6480     0.76      0.6      NA       NA
5     0.255     no  0.6032     0.76      0.6      NA       NA
6     0.500    yes  0.6480     0.76      0.6      NA       NA
7     0.500     no  0.6032     0.76      0.6      NA       NA
  Spec_old Diff
1       NA   NA
2       NA   NA
3      0.6   NA
4       NA   NA
5       NA   NA
6       NA   NA
7       NA   NA
</pre>

 * ***UNequal results for Spec***:

<pre>
    no pairs of results without missing values
  threshold pruned ROC_new Sens_new Spec_new ROC_old Sens_old
1     0.010    yes  0.6480     0.76      0.6      NA       NA
2     0.010     no  0.6032     0.76      0.6      NA       NA
3     0.250    yes      NA       NA       NA   0.648     0.76
4     0.255    yes  0.6480     0.76      0.6      NA       NA
5     0.255     no  0.6032     0.76      0.6      NA       NA
6     0.500    yes  0.6480     0.76      0.6      NA       NA
7     0.500     no  0.6032     0.76      0.6      NA       NA
  Spec_old Diff
1       NA   NA
2       NA   NA
3      0.6   NA
4       NA   NA
5       NA   NA
6       NA   NA
7       NA   NA
</pre>


**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.56s (new) 0.62s

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

