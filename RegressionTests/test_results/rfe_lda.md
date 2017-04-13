`rfe_lda`
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rfe_lda.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/rfe_lda.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `plyr` (1.8.4)
 * tested on 2017-04-12 at 18:21. 
 * total test time: 6.1s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75), `plyr` (1.8.4)
 * tested on 2017-04-11 at 20:27. 
 * total test time: 5.5s


Results:
---------

**Test Case**: `cv_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 0.4s (new) 0.34s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                  
      mean  -5.785
      min  -23.203
      max   10.494
    correlation:  0.8032 

 Variables Accuracy_new Accuracy_old  Diff Flag
         1       0.4387       0.3971  10.5    *
         5       0.3578       0.3995 -10.4    *
        10       0.4583       0.5968 -23.2    *
        15       0.6360       0.6360   0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                  
      mean  -35.61
      min  -142.89
      max    40.66
    correlation:  0.8046 

 Variables Kappa_new Kappa_old   Diff Flag
         1  -0.12394   -0.2073  -40.2    *
         5  -0.28653   -0.2037   40.7    *
        10  -0.08321    0.1940 -142.9    *
        15   0.27161    0.2716    0.0     

</pre>


**Test Case**: `cv_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 0.33s (new) 0.27s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                  
      mean  -3.497
      min  -21.387
      max   10.494
    correlation:  0.8227 

 Variables Accuracy_new Accuracy_old  Diff Flag
         1       0.4387       0.3971  10.5    *
         5       0.3578       0.3995 -10.4    *
        10       0.5000       0.6360 -21.4    *
        15       0.5968       0.5748   3.8    *
        17       0.6360       0.6360   0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                 
      mean -14.97
      min  -99.95
      max   40.66
    correlation:  0.8287 

 Variables  Kappa_new Kappa_old   Diff Flag
         1 -0.1239410   -0.2073  -40.2    *
         5 -0.2865284   -0.2037   40.7    *
        10  0.0001277    0.2695 -100.0    *
        15  0.1879433    0.1508   24.6    *
        17  0.2716137    0.2716    0.0     

</pre>


**Test Case**: `cv_pred_class`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `cv_pred_form_class`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
   pred             Class1              Class2         
 Mode:logical   Min.   :-0.739889   Min.   :-0.760326  
 NA's:500       1st Qu.:-0.257386   1st Qu.:-0.300029  
                Median : 0.004295   Median :-0.004295  
                Mean   : 0.015205   Mean   :-0.015205  
                3rd Qu.: 0.300029   3rd Qu.: 0.257386  
                Max.   : 0.760326   Max.   : 0.739889  
</pre>

**Test Case**: `loo_model_class`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15


Execution times: (old) 1.23s (new) 1.16s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                  
      mean  -2.851
      min  -34.483
      max   23.077
    correlation:  0.2672 

 Variables Accuracy_new Accuracy_old  Diff Flag
         1         0.60         0.60   0.0     
         5         0.38         0.58 -34.5    *
        10         0.64         0.52  23.1    *
        15         0.68         0.68   0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):                 
      mean   87.5
      min  -250.0
      max   600.0
    correlation:  0.2672 

 Variables Kappa_new Kappa_old Diff Flag
         1      0.20      0.20    0     
         5     -0.24      0.16 -250    *
        10      0.28      0.04  600    *
        15      0.36      0.36    0     

</pre>


**Test Case**: `loo_model_form_class`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 17


Execution times: (old) 1.53s (new) 1.4s

Test Results:

 * ***UNequal results for Accuracy***:
<pre>
    %differences (n-o):                  
      mean  -4.497
      min  -34.483
      max   32.000
    correlation:  0.2196 

 Variables Accuracy_new Accuracy_old  Diff Flag
         1         0.60         0.60   0.0     
         5         0.38         0.58 -34.5    *
        10         0.66         0.50  32.0    *
        15         0.64         0.80 -20.0    *
        17         0.66         0.66   0.0     

</pre>

 * ***UNequal results for Kappa***:
<pre>
    %differences (n-o):               
      mean  Inf
      min  -250
      max   Inf
    correlation:  0.2196 

 Variables Kappa_new Kappa_old   Diff Flag
         1      0.20      0.20    0.0     
         5     -0.24      0.16 -250.0    *
        10      0.32      0.00    Inf    *
        15      0.28      0.60  -53.3    *
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
 Mode:logical   Min.   :-0.90590   Min.   :-0.86397  
 NA's:500       1st Qu.:-0.15979   1st Qu.:-0.09039  
                Median :-0.01727   Median : 0.01727  
                Mean   :-0.03781   Mean   : 0.03781  
                3rd Qu.: 0.09039   3rd Qu.: 0.15979  
                Max.   : 0.86397   Max.   : 0.90590  
</pre>

