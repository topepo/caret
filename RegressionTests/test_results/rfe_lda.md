`rfe_lda`
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rfe_lda.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-68), `plyr` (1.8.4)
 * tested on 2016-06-10 at 17:36


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-06-07 r70726)
 * `caret` (6.0-70), `plyr` (1.8.4)
 * tested on 2016-06-12 at 13:14


Results:
---------

**Test Case**: `cv_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 0.71s (new) 0.69s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                 
      mean  4.251
      min  -9.497
      max  14.858
    correlation:  0.9144 

 Variables Accuracy_new Accuracy_old Diff Flag
         1       0.3971       0.4387 -9.5    *
         5       0.3995       0.3578 11.6    *
        10       0.5968       0.5196 14.9    *
        15       0.6360       0.6360  0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                 
      mean 117.60
      min  -28.91
      max  432.06
    correlation:  0.9133 

 Variables Kappa_new Kappa_old  Diff Flag
         1   -0.2073  -0.12394  67.2    *
         5   -0.2037  -0.28653 -28.9    *
        10    0.1940   0.03646 432.1    *
        15    0.2716   0.27161   0.0     

</pre>


**Test Case**: `cv_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 0.47s (new) 0.43s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                 
      mean  2.005
      min  -9.808
      max  17.687
    correlation:  0.8639 

 Variables Accuracy_new Accuracy_old Diff Flag
         1       0.3971       0.4387 -9.5    *
         5       0.3995       0.3578 11.6    *
        10       0.6360       0.5404 17.7    *
        15       0.5748       0.6373 -9.8    *
        17       0.6360       0.6360  0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                 
      mean  47.58
      min  -45.32
      max  244.90
    correlation:  0.8649 

 Variables Kappa_new Kappa_old  Diff Flag
         1   -0.2073  -0.12394  67.2    *
         5   -0.2037  -0.28653 -28.9    *
        10    0.2695   0.07813 244.9    *
        15    0.1508   0.27579 -45.3    *
        17    0.2716   0.27161   0.0     

</pre>


**Test Case**: `cv_pred_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `cv_pred_form_class`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
   pred             Class1             Class2        
 Mode:logical   Min.   :-0.75951   Min.   :-0.70843  
 NA's:500       1st Qu.:-0.30012   1st Qu.:-0.23049  
                Median :-0.05319   Median : 0.05319  
                Mean   :-0.03263   Mean   : 0.03263  
                3rd Qu.: 0.23049   3rd Qu.: 0.30012  
                Max.   : 0.70843   Max.   : 0.75951  
</pre>

**Test Case**: `loo_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 2.9s (new) 2.65s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                  
      mean   7.855
      min  -21.212
      max   52.632
    correlation:  0.2053 

 Variables Accuracy_new Accuracy_old  Diff Flag
         1         0.60         0.60   0.0     
         5         0.58         0.38  52.6    *
        10         0.52         0.66 -21.2    *
        15         0.68         0.68   0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences < 0.1%
</pre>


**Test Case**: `loo_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 3.24s (new) 3.05s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                 
      mean   9.92
      min  -24.24
      max   52.63
    correlation:  0.2784 

 Variables Accuracy_new Accuracy_old  Diff Flag
         1         0.60         0.60   0.0     
         5         0.58         0.38  52.6    *
        10         0.50         0.66 -24.2    *
        15         0.80         0.66  21.2    *
        17         0.66         0.66   0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                  
      mean  -35.83
      min  -166.67
      max    87.50
    correlation:  0.2784 

 Variables Kappa_new Kappa_old   Diff Flag
         1      0.20      0.20    0.0     
         5      0.16     -0.24 -166.7    *
        10      0.00      0.32 -100.0    *
        15      0.60      0.32   87.5    *
        17      0.32      0.32    0.0     

</pre>


**Test Case**: `loo_pred_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `loo_pred_form_class`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
   pred             Class1             Class2        
 Mode:logical   Min.   :-0.85592   Min.   :-0.90928  
 NA's:500       1st Qu.:-0.08086   1st Qu.:-0.15480  
                Median : 0.01593   Median :-0.01593  
                Mean   : 0.03399   Mean   :-0.03399  
                3rd Qu.: 0.15480   3rd Qu.: 0.08086  
                Max.   : 0.90928   Max.   : 0.85592  
</pre>

