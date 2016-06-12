eXtreme Gradient Boosting (`xgbTree`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/xgbTree.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `plyr` (1.8.4), `xgboost` (0.4-3)
 * tested on 2016-06-10 at 17:54


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `plyr` (1.8.4), `xgboost` (0.4-3)
 * tested on 2016-06-12 at 13:37


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 8 tuning parameter combinations were evaluated


Execution times: (old) 0.9s (new) 0.99s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                 
      mean  7.998
      min   0.000
      max  38.837
    correlation:  0.966 

 eta max_depth gamma colsample_bytree min_child_weight nrounds ROC_new ROC_old Diff Flag
 0.1         1     0              0.7                1       1  0.5813  0.4187 38.8    *
 0.1         1     0              0.7                1      10  0.7667  0.7667  0.0     
 0.1         4     0              0.7                1       1  0.6958  0.6958  0.0     
 0.1         4     0              0.7                1      10  0.7684  0.7684  0.0     
 0.4         1     0              0.7                1       1  0.5558  0.4442 25.1    *
 0.4         1     0              0.7                1      10  0.8566  0.8566  0.0     
 0.4         4     0              0.7                1       1  0.6307  0.6307  0.0     
 0.4         4     0              0.7                1      10  0.8327  0.8327  0.0     

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
 * 8 tuning parameter combinations were evaluated


Execution times: (old) 1.82s (new) 2.05s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                 
      mean  7.998
      min   0.000
      max  38.837
    correlation:  0.966 

 eta max_depth gamma colsample_bytree min_child_weight nrounds ROC_new ROC_old Diff Flag
 0.1         1     0              0.7                1       1  0.5813  0.4187 38.8    *
 0.1         1     0              0.7                1      10  0.7667  0.7667  0.0     
 0.1         4     0              0.7                1       1  0.6958  0.6958  0.0     
 0.1         4     0              0.7                1      10  0.7684  0.7684  0.0     
 0.4         1     0              0.7                1       1  0.5558  0.4442 25.1    *
 0.4         1     0              0.7                1      10  0.8566  0.8566  0.0     
 0.4         4     0              0.7                1       1  0.6307  0.6307  0.0     
 0.4         4     0              0.7                1      10  0.8327  0.8327  0.0     

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
 * 8 tuning parameter combinations were evaluated


Execution times: (old) 14.17s (new) 15.2s

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


Execution times: (old) 0.48s (new) 0.52s

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

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 1.36s (new) 1.52s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 8 tuning parameter combinations were evaluated


Execution times: (old) 0.84s (new) 0.89s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 8 tuning parameter combinations were evaluated


Execution times: (old) 1.29s (new) 1.39s

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
 * Pre-processing: centered (20), scaled (20)  
 * 8 tuning parameter combinations were evaluated


Execution times: (old) 14.54s (new) 15.49s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.5s (new) 0.5s

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

**Test Case**: `reg_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 1.5s (new) 1.66s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

