Bagged CART (`treebag`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/treebag.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `e1071` (1.6-7), `ipred` (0.9-5), `plyr` (1.8.4), `rpart` (4.1-10)
 * tested on 2016-06-10 at 17:52


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `e1071` (1.6-7), `ipred` (0.9-5), `plyr` (1.8.4), `rpart` (4.1-10)
 * tested on 2016-06-12 at 13:34


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


Execution times: (old) 1s (new) 1.03s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                
      mean 0.672
      min  0.672
      max  0.672
    correlation:  NA 

 parameter ROC_new ROC_old Diff Flag
      none  0.6936  0.6889  0.7    *

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
 * 1 tuning parameter combination was evaluated


Execution times: (old) 1.67s (new) 1.76s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                
      mean 0.672
      min  0.672
      max  0.672
    correlation:  NA 

 parameter ROC_new ROC_old Diff Flag
      none  0.6936  0.6889  0.7    *

</pre>

 * _Equal results for Sens_
 * _Equal results for Spec_

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


Execution times: (old) 5.55s (new) 5.83s

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


Execution times: (old) 0.53s (new) 0.59s

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

**Test Case**: `class_oob_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Out of Bag Resampling
 * Grid search
 * Pre-processing: None  
 * 1 tuning parameter combination was evaluated


Execution times: (old) 0.63s (new) 0.75s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_
