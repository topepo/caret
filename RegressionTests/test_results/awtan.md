Tree Augmented Naive Bayes Classifier with Attribute Weighting (`awtan`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/awtan.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/awtan.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `bnclassify` (0.3.2), `caret` (6.0-73)
 * tested on 2017-04-12 at 18:54. 
 * total test time: 147.1s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `bnclassify` (0.3.2), `caret` (6.0-75)
 * tested on 2017-04-11 at 21:27. 
 * total test time: 135.5s


Results:
---------

**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: None  
 * 6 tuning parameter combinations were evaluated


Execution times: (old) 5.8s (new) 5.22s

Test Results:

 * _Equal results for ROC_
 * _Equal results for Sens_
 * _Equal results for Spec_

**Test Case**: `class_imp`

Object class(es): `varImp.train`

 * ***UNequal results***: differences (o-n):
<pre>
     Class1             Class2        
 Min.   :-100.000   Min.   :-100.000  
 1st Qu.: -31.907   1st Qu.: -31.907  
 Median :  21.826   Median :  21.826  
 Mean   :   4.223   Mean   :   4.223  
 3rd Qu.:  48.816   3rd Qu.:  48.816  
 Max.   :  57.480   Max.   :  57.480  
</pre>

**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: None  
 * 6 tuning parameter combinations were evaluated


Execution times: (old) 2.22m (new) 2.07m

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
 * Pre-processing: None  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.72s (new) 0.39s

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

**Test Case**: `class_predictors1`

Object class(es): `character`

 * _Equal results_

**Test Case**: `class_prob`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 6 tuning parameter combinations were evaluated


Execution times: (old) 4.76s (new) 3.19s

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    0 missing values in old
    2 missing values in new
    %differences (n-o):                 
      mean  1.287
      min  -3.556
      max   8.702
    correlation:  0.4901 

  score smooth Accuracy_new Accuracy_old Diff Flag
 loglik      1       0.5398       0.5597 -3.6    *
 loglik      2       0.6197       0.5701  8.7    *
    bic      1       0.5995       0.5995  0.0     
    bic      2           NA       0.6096   NA     
    aic      1       0.5897       0.5897  0.0     
    aic      2           NA       0.5998   NA     

</pre>

 * ***UNequal results for Kappa***:

<pre>
    0 missing values in old
    2 missing values in new
    %differences (n-o):                 
      mean  10.93
      min  -37.08
      max   80.78
    correlation:  0.4405 

  score smooth Kappa_new Kappa_old  Diff Flag
 loglik      1   0.07106    0.1129 -37.1    *
 loglik      2   0.23091    0.1277  80.8    *
    bic      1   0.19334    0.1933   0.0     
    bic      2        NA    0.2120    NA     
    aic      1   0.17220    0.1722   0.0     
    aic      2        NA    0.1903    NA     

</pre>


**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

