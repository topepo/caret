glmnet (`gbm_h2o`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/gbm_h2o.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/gbm_h2o.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `h2o` (3.10.3.6)
 * tested on 2017-04-12 at 20:38. 
 * total test time: 3264.6s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `h2o` (3.10.3.6)
 * tested on 2017-04-11 at 21:38. 
 * total test time: 3007.7s


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


Execution times: (old) 31.6s (new) 28.2s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                  
      mean -0.1874
      min  -0.8715
      max   0.2232
    correlation:  0.996 

 max_depth learn_rate ntrees min_rows col_sample_rate ROC_new ROC_old Diff Flag
        11        0.1     10       10             0.5  0.9111  0.9191 -0.9    *
        11        0.1    100       10             0.5  0.9294  0.9286  0.1    *
        11        0.1     50       10             0.5  0.9352  0.9331  0.2    *

</pre>

 * ***UNequal results for Sens***:

<pre>
    %differences (n-o):                   
      mean  0.47335
      min  -0.01555
      max   1.41865
    correlation:  0.9975 

 max_depth learn_rate ntrees min_rows col_sample_rate Sens_new Sens_old Diff Flag
        11        0.1     10       10             0.5   0.8359   0.8357  0.0    *
        11        0.1    100       10             0.5   0.9726   0.9590  1.4    *
        11        0.1     50       10             0.5   0.9111   0.9113  0.0    *

</pre>

 * ***UNequal results for Spec***:

<pre>
    %differences (n-o):                  
      mean -0.6182
      min  -3.0757
      max   3.5127
    correlation:  0.9728 

 max_depth learn_rate ntrees min_rows col_sample_rate Spec_new Spec_old Diff Flag
        11        0.1     10       10             0.5   0.8307   0.8502 -2.3    *
        11        0.1    100       10             0.5   0.6100   0.6293 -3.1    *
        11        0.1     50       10             0.5   0.7593   0.7335  3.5    *

</pre>


**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 45.84s (new) 37.3s

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Sens***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Spec***:

<pre>
    %differences (n-o):                 
      mean  2.491
      min  -2.165
      max   7.878
    correlation:  0.9998 

 max_depth learn_rate ntrees min_rows col_sample_rate Spec_new Spec_old Diff Flag
        11        0.1     10       10             0.5   0.8635   0.8826 -2.2    *
        11        0.1    100       10             0.5   0.6231   0.5776  7.9    *
        11        0.1     50       10             0.5   0.7269   0.7143  1.8    *

</pre>


**Test Case**: `class_imp`

Object class(es): `varImp.train`

 * ***UNequal results***: differences (o-n):
<pre>
    Overall     
 Min.   :0.000  
 1st Qu.:1.360  
 Median :3.961  
 Mean   :3.258  
 3rd Qu.:4.291  
 Max.   :7.545  
</pre>

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 41.37m (new) 36.81m

Test Results:

 * ***UNequal results for ROC***:

<pre>
    %differences (n-o):                  
      mean -0.2773
      min  -1.2511
      max   0.3439
    correlation:  0.994 

 ntrees max_depth min_rows learn_rate col_sample_rate ROC_new ROC_old Diff Flag
     10        11       10        0.1             0.5  0.9162  0.9278 -1.3    *
    100        11       10        0.1             0.5  0.9474  0.9441  0.3    *
     50        11       10        0.1             0.5  0.9466  0.9459  0.1    *

</pre>

 * ***UNequal results for Sens***:

<pre>
    %differences (n-o):                  
      mean -0.6475
      min  -4.1322
      max   2.1898
    correlation:  0.9863 

 ntrees max_depth min_rows learn_rate col_sample_rate Sens_new Sens_old Diff Flag
     10        11       10        0.1             0.5   0.7945   0.8288 -4.1    *
    100        11       10        0.1             0.5   0.9795   0.9795  0.0     
     50        11       10        0.1             0.5   0.9589   0.9384  2.2    *

</pre>

 * ***UNequal results for Spec***:

<pre>
    %differences (n-o):                 
      mean -1.068
      min  -4.380
      max   2.062
    correlation:  1 

 ntrees max_depth min_rows learn_rate col_sample_rate Spec_new Spec_old Diff Flag
     10        11       10        0.1             0.5   0.8506   0.8896 -4.4    *
    100        11       10        0.1             0.5   0.6429   0.6299  2.1    *
     50        11       10        0.1             0.5   0.7273   0.7338 -0.9    *

</pre>


**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 3.19s (new) 1.97s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_none_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 96.8% (484/500)
        newer
older    Class1 Class2
  Class1    337      5
  Class2     11    147
</pre>

**Test Case**: `class_none_prob`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1               Class2          
 Min.   :-0.4214561   Min.   :-0.3500998  
 1st Qu.:-0.0048267   1st Qu.:-0.0030660  
 Median :-0.0000180   Median : 0.0000180  
 Mean   : 0.0006944   Mean   :-0.0006944  
 3rd Qu.: 0.0030660   3rd Qu.: 0.0048267  
 Max.   : 0.3500998   Max.   : 0.4214561  
</pre>

**Test Case**: `class_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 94.2% (471/500)
        newer
older    Class1 Class2
  Class1    289      8
  Class2     21    182
</pre>

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 95% (475/500)
        newer
older    Class1 Class2
  Class1    291      2
  Class2     23    184
</pre>

**Test Case**: `class_predictors1`

Object class(es): `character`

 * _Equal results_

**Test Case**: `class_prob`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1              Class2         
 Min.   :-0.355917   Min.   :-0.339178  
 1st Qu.:-0.025264   1st Qu.:-0.010068  
 Median :-0.001312   Median : 0.001312  
 Mean   :-0.013171   Mean   : 0.013171  
 3rd Qu.: 0.010068   3rd Qu.: 0.025264  
 Max.   : 0.339178   Max.   : 0.355917  
</pre>

**Test Case**: `class_prob_form`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1               Class2          
 Min.   :-0.2810130   Min.   :-0.2639623  
 1st Qu.:-0.0085770   1st Qu.:-0.0165296  
 Median : 0.0007296   Median :-0.0007296  
 Mean   : 0.0013703   Mean   :-0.0013703  
 3rd Qu.: 0.0165296   3rd Qu.: 0.0085770  
 Max.   : 0.2639623   Max.   : 0.2810130  
</pre>

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 2.1m (new) 1.94m

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences (n-o):                  
      mean  1.0357
      min  -0.4356
      max   2.2695
    correlation:  0.9538 

 ntrees max_depth learn_rate min_rows col_sample_rate Accuracy_new Accuracy_old Diff Flag
   3430         8    0.16542       15          0.8520       0.7496       0.7329  2.3    *
   3480         2    0.23465       12          0.2095       0.7297       0.7165  1.9    *
   3972         5    0.01304       18          0.4968       0.7697       0.7730 -0.4    *
    672         4    0.47686       25          0.9989       0.7397       0.7363  0.5    *

</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences (n-o):                 
      mean  3.232
      min  -1.184
      max   6.861
    correlation:  0.9536 

 ntrees max_depth learn_rate min_rows col_sample_rate Kappa_new Kappa_old Diff Flag
   3430         8    0.16542       15          0.8520    0.5056    0.4732  6.9    *
   3480         2    0.23465       12          0.2095    0.4669    0.4410  5.9    *
   3972         5    0.01304       18          0.4968    0.5446    0.5511 -1.2    *
    672         4    0.47686       25          0.9989    0.4862    0.4796  1.4    *

</pre>


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
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 16.81s (new) 17.03s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    %differences (n-o):                  
      mean  0.5009
      min  -0.5590
      max   2.0290
    correlation:  0.9877 

 max_depth learn_rate ntrees min_rows col_sample_rate RMSE_new RMSE_old Diff Flag
        11        0.1     10       10             0.5    20.55    20.66 -0.6    *
        11        0.1    100       10             0.5    24.11    24.10  0.0    *
        11        0.1     50       10             0.5    23.16    22.70  2.0    *

</pre>

 * ***UNequal results for Rsquared***:

<pre>
    %differences (n-o):                 
      mean 27.111
      min  -2.676
      max  54.534
    correlation:  0.9066 

 max_depth learn_rate ntrees min_rows col_sample_rate Rsquared_new Rsquared_old Diff Flag
        11        0.1     10       10             0.5      0.03338      0.02160 54.5    *
        11        0.1    100       10             0.5      0.06154      0.06323 -2.7    *
        11        0.1     50       10             0.5      0.07077      0.05466 29.5    *

</pre>


**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 16.76s (new) 18.57s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    %differences (n-o):                  
      mean -1.0807
      min  -2.3217
      max   0.2132
    correlation:  0.9986 

 max_depth learn_rate ntrees min_rows col_sample_rate RMSE_new RMSE_old Diff Flag
        11        0.1     10       10             0.5    20.78    20.73  0.2    *
        11        0.1    100       10             0.5    23.62    24.18 -2.3    *
        11        0.1     50       10             0.5    23.04    23.30 -1.1    *

</pre>

 * ***UNequal results for Rsquared***:

<pre>
    %differences (n-o):                  
      mean   8.431
      min  -10.473
      max   27.836
    correlation:  0.8369 

 max_depth learn_rate ntrees min_rows col_sample_rate Rsquared_new Rsquared_old  Diff Flag
        11        0.1     10       10             0.5      0.03823      0.04270 -10.5    *
        11        0.1    100       10             0.5      0.06532      0.06052   7.9    *
        11        0.1     50       10             0.5      0.06406      0.05011  27.8    *

</pre>


**Test Case**: `reg_imp`

Object class(es): `varImp.train`

 * ***UNequal results***: differences (o-n):
<pre>
    Overall         
 Min.   :-43.53327  
 1st Qu.: -7.92093  
 Median : -2.56602  
 Mean   : -8.17382  
 3rd Qu.: -0.94754  
 Max.   :  0.07102  
</pre>

**Test Case**: `reg_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 6.62m (new) 7.13m

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    %differences (n-o):                  
      mean -0.6983
      min  -2.1467
      max   0.2428
    correlation:  0.9714 

 ntrees max_depth min_rows learn_rate col_sample_rate RMSE_new RMSE_old Diff Flag
     10        11       10        0.1             0.5    21.22    21.26 -0.2    *
    100        11       10        0.1             0.5    23.02    23.52 -2.1    *
     50        11       10        0.1             0.5    23.03    22.97  0.2    *

</pre>

 * ***UNequal results for Rsquared***:

<pre>
    %differences (n-o):                  
      mean  -6.331
      min  -52.266
      max   63.267
    correlation:  0.8242 

 ntrees max_depth min_rows learn_rate col_sample_rate Rsquared_new Rsquared_old  Diff Flag
     10        11       10        0.1             0.5     0.031728     0.045321 -30.0    *
    100        11       10        0.1             0.5     0.004737     0.009924 -52.3    *
     50        11       10        0.1             0.5     0.022389     0.013713  63.3    *

</pre>


**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 2.11s (new) 1.9s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `numeric`

 * ***UNequal results***:
<pre>
    %differences (n-o):                  
      mean   -5.18
      min  -516.16
      max    41.58
    correlation:  0.999 
</pre>

**Test Case**: `reg_pred`

Object class(es): `numeric`

 * ***UNequal results***:
<pre>
    %differences (n-o):                  
      mean  -0.376
      min  -32.856
      max   24.241
    correlation:  0.962 
</pre>

**Test Case**: `reg_pred_form`

Object class(es): `numeric`

 * ***UNequal results***:
<pre>
    %differences (n-o):                  
      mean   0.963
      min  -22.776
      max   23.521
    correlation:  0.957 
</pre>

**Test Case**: `reg_predictors1`

Object class(es): `character`

 * ***UNequal results***:
<pre>
   Agreement: 5.6% (1/18)
