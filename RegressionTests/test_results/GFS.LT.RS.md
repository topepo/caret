Genetic Lateral Tuning and Rule Selection of Linguistic Fuzzy Systems (`GFS.LT.RS`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/GFS.LT.RS.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/GFS.LT.RS.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-71), `frbs` (3.1-0)
 * tested on 2016-10-31 at 05:44


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-72), `frbs` (3.1-0)
 * tested on 2016-10-30 at 22:52


Results:
---------

**Test Case**: `reg_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 6.71m (new) 6.84m

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 6.66m (new) 6.99m

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
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 1733.98h (new) 1956.6h

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


Execution times: (old) 39.38s (new) 38.09s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `matrix`

 * _Equal results_

**Test Case**: `reg_pred`

Object class(es): `matrix`

 * _Equal results_

**Test Case**: `reg_pred_form`

Object class(es): `matrix`

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


Execution times: (old) 3.53m (new) 8.34m

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    no pairs of results without missing values
  max.gen popu.size num.labels RMSE_new Rsquared_new RMSE_old
1       1        16          8       NA           NA 32.91748
2       1        38          8 32.91748          NaN       NA
3       8        20         20       NA           NA 35.31841
4       8        48         20 35.31841          NaN       NA
5       9         4          4       NA           NA 28.05816
6       9        16          4 28.05816          NaN       NA
7      18         6          7       NA           NA 32.29310
8      18        18          7 32.29310          NaN       NA
  Rsquared_old Diff
1          NaN   NA
2           NA   NA
3          NaN   NA
4           NA   NA
5          NaN   NA
6           NA   NA
7          NaN   NA
8           NA   NA
</pre>

 * _Equal results for Rsquared_

