`rfe_nb`
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rfe_nb.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `plyr` (1.8.4)
 * tested on 2016-06-10 at 17:37


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `plyr` (1.8.4)
 * tested on 2016-06-12 at 13:15


Results:
---------

**Test Case**: `cv_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 1.31s (new) 1.25s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                
      mean 3.751
      min  0.000
      max  8.219
    correlation:  0.8978 

 Variables Accuracy_new Accuracy_old Diff Flag
         1       0.5172       0.5172  0.0     
         5       0.5809       0.5368  8.2    *
        10       0.5980       0.5600  6.8    *
        15       0.6397       0.6397  0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                 
      mean  46.76
      min    0.00
      max  124.67
    correlation:  0.8832 

 Variables Kappa_new Kappa_old  Diff Flag
         1   0.03621   0.03621   0.0     
         5   0.17385   0.07738 124.7    *
        10   0.20857   0.12846  62.4    *
        15   0.28369   0.28369   0.0     

</pre>


**Test Case**: `cv_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 1.16s (new) 1.18s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                 
      mean  1.603
      min  -3.265
      max   8.219
    correlation:  0.8289 

 Variables Accuracy_new Accuracy_old Diff Flag
         1       0.5172       0.5172  0.0     
         5       0.5809       0.5368  8.2    *
        10       0.5772       0.5600  3.1    *
        15       0.5809       0.6005 -3.3    *
        17       0.6189       0.6189  0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                 
      mean  26.38
      min  -19.95
      max  124.67
    correlation:  0.8 

 Variables Kappa_new Kappa_old  Diff Flag
         1   0.03621   0.03621   0.0     
         5   0.17385   0.07738 124.7    *
        10   0.16338   0.12846  27.2    *
        15   0.16540   0.20662 -20.0    *
        17   0.23937   0.23937   0.0     

</pre>


**Test Case**: `cv_pred_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `cv_pred_form_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `loo_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 5.8s (new) 5.66s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                   
      mean  0.04596
      min  -2.94118
      max   3.12500
    correlation:  0.9258 

 Variables Accuracy_new Accuracy_old Diff Flag
         1         0.74         0.74  0.0     
         5         0.66         0.64  3.1    *
        10         0.66         0.68 -2.9    *
        15         0.66         0.66  0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                   
      mean   0.7937
      min  -11.1111
      max   14.2857
    correlation:  0.9258 

 Variables Kappa_new Kappa_old  Diff Flag
         1      0.48      0.48   0.0     
         5      0.32      0.28  14.3    *
        10      0.32      0.36 -11.1    *
        15      0.32      0.32   0.0     

</pre>


**Test Case**: `loo_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 7.94s (new) 7.5s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                 
      mean   1.32
      min  -14.71
      max   18.18
    correlation:  0.2598 

 Variables Accuracy_new Accuracy_old  Diff Flag
         1         0.74         0.74   0.0     
         5         0.66         0.64   3.1    *
        10         0.58         0.68 -14.7    *
        15         0.78         0.66  18.2    *
        17         0.72         0.72   0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                  
      mean   6.746
      min  -55.556
      max   75.000
    correlation:  0.2598 

 Variables Kappa_new Kappa_old  Diff Flag
         1      0.48      0.48   0.0     
         5      0.32      0.28  14.3    *
        10      0.16      0.36 -55.6    *
        15      0.56      0.32  75.0    *
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
 Mode:logical   Min.   :-0.89906   Min.   :-0.77594  
 NA's:500       1st Qu.:-0.22876   1st Qu.:-0.13874  
                Median :-0.06896   Median : 0.06896  
                Mean   :-0.03789   Mean   : 0.03789  
                3rd Qu.: 0.13874   3rd Qu.: 0.22876  
                Max.   : 0.77594   Max.   : 0.89906  
</pre>

