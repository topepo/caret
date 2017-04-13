Ensembles of Generalized Lienar Models (`randomGLM`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/randomGLM.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/randomGLM.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `doParallel` (1.0.10), `foreach` (1.4.3), `iterators` (1.0.8), `MASS` (7.3-45), `randomGLM` (1.02-1)
 * tested on 2017-04-12 at 19:31. 
 * total test time: 410.7s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `doParallel` (1.0.10), `foreach` (1.4.3), `iterators` (1.0.8), `MASS` (7.3-45), `randomGLM` (1.02-1)
 * tested on 2017-04-11 at 21:10. 
 * total test time: 629.8s


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (4), scaled (4)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 32.87s (new) 53.4s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (4), scaled (4)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 33.17s (new) 59.01s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_imp`

Object class(es): `varImp.train`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1             Class2        
 Min.   :-25.0000   Min.   :-25.0000  
 1st Qu.: -6.2500   1st Qu.: -6.2500  
 Median :  0.4288   Median :  0.4288  
 Mean   : -4.0582   Mean   : -4.0582  
 3rd Qu.:  2.6206   3rd Qu.:  2.6206  
 Max.   :  7.9096   Max.   :  7.9096  
</pre>

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (4), scaled (4)  
 * 1 tuning parameter combination was evaluated


Execution times: (old) 3.53m (new) 5.33m

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
 * Pre-processing: centered (4), scaled (4)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 5.58s (new) 7.72s

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

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (4), scaled (4)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 18.82s (new) 27.45s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (4), scaled (4)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 18.96s (new) 27.64s

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
 * Pre-processing: centered (4), scaled (4)  
 * 1 tuning parameter combination was evaluated


Execution times: (old) 1.12m (new) 1.67m

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (4), scaled (4)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 2.59s (new) 3.21s

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

