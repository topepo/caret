`rfe_nb`
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rfe_nb.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/rfe_nb.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `plyr` (1.8.4)
 * tested on 2017-04-12 at 20:39. 
 * total test time: 22.3s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `plyr` (1.8.4)
 * tested on 2017-04-11 at 21:04. 
 * total test time: 24.2s


Results:
---------

**Test Case**: `cv_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 1.09s (new) 1.27s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                  
      mean -0.9254
      min  -7.5949
      max   3.8934
    correlation:  0.8876 

 Variables Accuracy_new Accuracy_old Diff Flag
         1       0.5172       0.5172  0.0     
         5       0.5368       0.5809 -7.6    *
        10       0.6213       0.5980  3.9    *
        15       0.6397       0.6397  0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                  
      mean  -8.609
      min  -55.490
      max   21.054
    correlation:  0.8789 

 Variables Kappa_new Kappa_old  Diff Flag
         1   0.03621   0.03621   0.0     
         5   0.07738   0.17385 -55.5    *
        10   0.25248   0.20857  21.1    *
        15   0.28369   0.28369   0.0     

</pre>


**Test Case**: `cv_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 1.06s (new) 1.29s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                 
      mean  2.035
      min  -7.595
      max   9.916
    correlation:  0.6804 

 Variables Accuracy_new Accuracy_old Diff Flag
         1       0.5172       0.5172  0.0     
         5       0.5368       0.5809 -7.6    *
        10       0.6225       0.5772  7.9    *
        15       0.6385       0.5809  9.9    *
        17       0.6189       0.6189  0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                 
      mean  13.40
      min  -55.49
      max   67.36
    correlation:  0.6642 

 Variables Kappa_new Kappa_old  Diff Flag
         1   0.03621   0.03621   0.0     
         5   0.07738   0.17385 -55.5    *
        10   0.25346   0.16338  55.1    *
        15   0.27682   0.16540  67.4    *
        17   0.23937   0.23937   0.0     

</pre>


**Test Case**: `cv_pred_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `cv_pred_form_class`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
   pred             Class1               Class2          
 Mode:logical   Min.   :-0.1207278   Min.   :-0.5475111  
 NA's:500       1st Qu.:-0.0220693   1st Qu.:-0.0122921  
                Median : 0.0000441   Median :-0.0000441  
                Mean   : 0.0074132   Mean   :-0.0074132  
                3rd Qu.: 0.0122921   3rd Qu.: 0.0220693  
                Max.   : 0.5475111   Max.   : 0.1207278  
</pre>

**Test Case**: `loo_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 5.06s (new) 5.72s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                
      mean  0.00
      min  -3.03
      max   3.03
    correlation:  0.9258 

 Variables Accuracy_new Accuracy_old Diff Flag
         1         0.74         0.74    0     
         5         0.64         0.66   -3    *
        10         0.68         0.66    3    *
        15         0.66         0.66    0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                
      mean   0.0
      min  -12.5
      max   12.5
    correlation:  0.9258 

 Variables Kappa_new Kappa_old  Diff Flag
         1      0.48      0.48   0.0     
         5      0.28      0.32 -12.5    *
        10      0.36      0.32  12.5    *
        15      0.32      0.32   0.0     

</pre>


**Test Case**: `loo_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 7.25s (new) 7.94s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                   
      mean   0.9678
      min  -12.8205
      max   20.6897
    correlation:  0.2267 

 Variables Accuracy_new Accuracy_old  Diff Flag
         1         0.74         0.74   0.0     
         5         0.64         0.66  -3.0    *
        10         0.70         0.58  20.7    *
        15         0.68         0.78 -12.8    *
        17         0.72         0.72   0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                 
      mean  20.36
      min  -35.71
      max  150.00
    correlation:  0.2267 

 Variables Kappa_new Kappa_old  Diff Flag
         1      0.48      0.48   0.0     
         5      0.28      0.32 -12.5    *
        10      0.40      0.16 150.0    *
        15      0.36      0.56 -35.7    *
        17      0.44      0.44   0.0     

</pre>


**Test Case**: `loo_pred_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `loo_pred_form_class`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
   pred             Class1             Class2        
 Mode:logical   Min.   :-0.77594   Min.   :-0.89906  
 NA's:500       1st Qu.:-0.13874   1st Qu.:-0.22876  
                Median : 0.06896   Median :-0.06896  
                Mean   : 0.03789   Mean   :-0.03789  
                3rd Qu.: 0.22876   3rd Qu.: 0.13874  
                Max.   : 0.89906   Max.   : 0.77594  
</pre>

