`rfe_lm`
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rfe_lm.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/rfe_lm.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73), `plyr` (1.8.4)
 * tested on 2017-04-12 at 20:38. 
 * total test time: 67s


New:

 * x86_64-apple-darwin15.6.0 (64-bit)
 * R Under development (unstable) (2017-04-10 r72501)
 * `caret` (6.0-75), `plyr` (1.8.4)
 * tested on 2017-04-11 at 18:59. 
 * total test time: 7.6s


Results:
---------

**Test Case**: `cv_model`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 20


Execution times: (old) 2.25s (new) 0.79s

Test Results:

 * ***UNequal results for RMSE***:
<pre>
    %differences (n-o):                  
      mean  -5.669
      min  -16.133
      max    2.351
    correlation:  0.9668 

 Variables RMSE_new RMSE_old  Diff Flag
         1    17.71    20.86 -15.1    *
         5    18.14    21.64 -16.1    *
        10    23.21    23.08   0.5    *
        15    23.38    22.84   2.4    *
        20    23.01    23.01   0.0     

</pre>

 * ***UNequal results for Rsquared***:
<pre>
    %differences (n-o):                 
      mean  80.77
      min  -13.76
      max  180.34
    correlation:  -0.2957 

 Variables Rsquared_new Rsquared_old  Diff Flag
         1      0.06177      0.03253  89.9    *
         5      0.06049      0.02446 147.4    *
        10      0.09634      0.03436 180.3    *
        15      0.05825      0.06755 -13.8    *
        20      0.06367      0.06367   0.0     

</pre>


**Test Case**: `cv_model_form`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Cross-Validated (3 fold)
 * Subset sizes: 1, 5, 10, 15, 22


Execution times: (old) 1.65s (new) 0.37s

Test Results:

 * ***UNequal results for RMSE***:
<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Rsquared***:
<pre>
    %differences (n-o):                 
      mean  739.3
      min     0.0
      max  3369.4
    correlation:  0.5771 

 Variables Rsquared_new Rsquared_old   Diff Flag
         1      0.06177     0.026296  134.9    *
         5      0.06049     0.001744 3369.4    *
        10      0.09634     0.048517   98.6    *
        15      0.05825     0.030059   93.8    *
        22      0.01927     0.019266    0.0     

</pre>


**Test Case**: `cv_pred`

Object class(es): `numeric`

 * ***UNequal results***:
<pre>
    %differences (n-o):                 
      mean   8.63
      min  -75.29
      max  279.82
    correlation:  0.0226 
</pre>

**Test Case**: `loo_model`

Object class(es): `rfe`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 20


Execution times: (old) 30.61s (new) 1.7s

Test Results:

 * ***UNequal results for RMSE***:
<pre>
    %differences (n-o):                   
      mean  -0.7519
      min  -12.1675
      max   13.1451
    correlation:  0.1875 

 Variables RMSE_new RMSE_old  Diff Flag
         1    18.15    19.19  -5.4    *
         5    19.64    22.36 -12.2    *
        10    21.05    20.90   0.7    *
        15    23.06    20.38  13.1    *
        20    21.04    21.04   0.0     

</pre>

 * ***UNequal results for Rsquared***:
<pre>
    %differences (n-o):                   
      mean  2739.17
      min    -96.95
      max  13980.02
    correlation:  0.1388 

 Variables Rsquared_new Rsquared_old    Diff Flag
         1     0.005021    1.100e-01   -95.4    *
         5     0.003523    2.502e-05 13980.0    *
        10     0.004078    4.956e-02   -91.8    *
        15     0.002905    9.518e-02   -96.9    *
        20     0.076430    7.643e-02     0.0     

</pre>


**Test Case**: `loo_model_form`

Object class(es): `rfe` and `rfe.formula`

Model Configuration:

 * Resampling: Leave-One-Out Cross-Validation
 * Subset sizes: 1, 5, 10, 15, 22


Execution times: (old) 29.66s (new) 1.51s

Test Results:

 * ***UNequal results for RMSE***:
<pre>
    %differences (n-o):                  
      mean  -2.712
      min  -12.687
      max    5.503
    correlation:  0.6789 

 Variables RMSE_new RMSE_old  Diff Flag
         1    18.15    19.19  -5.4    *
         5    19.64    22.49 -12.7    *
        10    21.05    21.25  -0.9    *
        15    23.06    21.86   5.5    *
        22    22.60    22.60   0.0     

</pre>

 * ***UNequal results for Rsquared***:
<pre>
    %differences (n-o):                  
      mean  163.78
      min   -95.74
      max  1098.69
    correlation:  -0.08178 

 Variables Rsquared_new Rsquared_old   Diff Flag
         1     0.005021    0.1179769  -95.7    *
         5     0.003523    0.0002939 1098.7    *
        10     0.004078    0.0377242  -89.2    *
        15     0.002905    0.0564439  -94.9    *
        22     0.042467    0.0424673    0.0     

</pre>


**Test Case**: `loo_pred`

Object class(es): `numeric`

 * _Equal results_

