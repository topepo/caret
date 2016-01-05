Random k-Nearest Neighbors (`rknn`)
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/rknn.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2015-11-18 r69655)
 * `caret` (6.0-62), `gmp` (0.5-12), `rknn` (1.2-1)
 * tested on 2015-12-31 at 17:39


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2015-11-18 r69655)
 * `caret` (6.0-64), `gmp` (0.5-12), `rknn` (1.2-1)
 * tested on 2016-01-04 at 14:24


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 3.62s (new) 3.88s

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences (n-o):                  
      mean -0.9677
      min  -5.4514
      max   2.8725
    correlation:  0.93 

 mtry k Accuracy_new Accuracy_old Diff Flag
    2 5       0.7402       0.7414 -0.2    *
    2 7       0.6801       0.7194 -5.5    *
    2 9       0.6777       0.6985 -3.0    *
    4 5       0.6826       0.7022 -2.8    *
    4 7       0.7022       0.6826  2.9    *
    4 9       0.6422       0.6434 -0.2    *
    7 5       0.6029       0.6029  0.0     
    7 7       0.6189       0.6189  0.0     
    7 9       0.6801       0.6801  0.0     

</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences (n-o):                  
      mean  -3.223
      min  -18.511
      max   10.083
    correlation:  0.9289 

 mtry k Kappa_new Kappa_old  Diff Flag
    2 5    0.4832    0.4879  -1.0    *
    2 7    0.3585    0.4399 -18.5    *
    2 9    0.3597    0.4014 -10.4    *
    4 5    0.3653    0.4028  -9.3    *
    4 7    0.4033    0.3664  10.1    *
    4 9    0.2861    0.2859   0.1    *
    7 5    0.2122    0.2122   0.0     
    7 7    0.2404    0.2404   0.0     
    7 9    0.3568    0.3568   0.0     

</pre>


**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 8.49s (new) 9.67s

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences < 0.1%
</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences < 0.1%
</pre>


**Test Case**: `class_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 1.83m (new) 1.96m

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences (n-o):                  
      mean  0.3017
      min  -5.4054
      max   5.2632
    correlation:  0.9314 

 k mtry Accuracy_new Accuracy_old Diff Flag
 5    2         0.80         0.76  5.3    *
 5    4         0.72         0.72  0.0     
 5    7         0.60         0.60  0.0     
 7    2         0.72         0.70  2.9    *
 7    4         0.68         0.68  0.0     
 7    7         0.66         0.66  0.0     
 9    2         0.70         0.74 -5.4    *
 9    4         0.66         0.66  0.0     
 9    7         0.64         0.64  0.0     

</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences (n-o):                   
      mean   0.9687
      min  -16.6667
      max   15.3846
    correlation:  0.9314 

 k mtry Kappa_new Kappa_old  Diff Flag
 5    2      0.60      0.52  15.4    *
 5    4      0.44      0.44   0.0     
 5    7      0.20      0.20   0.0     
 7    2      0.44      0.40  10.0    *
 7    4      0.36      0.36   0.0     
 7    7      0.32      0.32   0.0     
 9    2      0.40      0.48 -16.7    *
 9    4      0.32      0.32   0.0     
 9    7      0.28      0.28   0.0     

</pre>


**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (7), scaled (7)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.47s (new) 0.45s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_none_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 95% (475/500)
        newer
older    Class1 Class2
  Class1    270     14
  Class2     11    205
</pre>

**Test Case**: `class_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 94.2% (471/500)
        newer
older    Class1 Class2
  Class1    266     14
  Class2     15    205
</pre>

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 94% (470/500)
        newer
older    Class1 Class2
  Class1    273     13
  Class2     17    197
</pre>

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: centered (7), scaled (7)  
 * 3 tuning parameter combinations were evaluated


Execution times: (old) 3.96s (new) 4.36s

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences (n-o):                
      mean 1.406
      min  0.000
      max  4.217
    correlation:  0.9951 

 mtry  k Accuracy_new Accuracy_old Diff Flag
    2 15       0.7420       0.7120  4.2    *
    7 13       0.5833       0.5833  0.0     
    7 15       0.6422       0.6422  0.0     

</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences (n-o):                 
      mean  4.858
      min   0.000
      max  14.573
    correlation:  0.9953 

 mtry  k Kappa_new Kappa_old Diff Flag
    2 15    0.4921    0.4295 14.6    *
    7 13    0.1662    0.1662  0.0     
    7 15    0.2834    0.2834  0.0     

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
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 2.39s (new) 2.34s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    %differences (n-o):                 
      mean 0.2443
      min  0.0000
      max  0.7531
    correlation:  0.9966 

 mtry k RMSE_new RMSE_old Diff Flag
   11 5    15.99    15.87  0.8    *
   11 7    16.02    15.99  0.2    *
   11 9    16.12    16.10  0.1    *
    2 5    16.06    15.99  0.4    *
    2 7    16.08    16.03  0.3    *
    2 9    16.27    16.22  0.4    *
   20 5    16.40    16.40  0.0     
   20 7    16.39    16.39  0.0     
   20 9    17.14    17.14  0.0     

</pre>

 * ***UNequal results for Rsquared***:

<pre>
    %differences (n-o):                  
      mean -10.186
      min  -35.352
      max    5.032
    correlation:  0.908 

 mtry k Rsquared_new Rsquared_old  Diff Flag
   11 5      0.12200      0.18871 -35.4    *
   11 7      0.17572      0.17006   3.3    *
   11 9      0.15793      0.15037   5.0    *
    2 5      0.13403      0.14834  -9.6    *
    2 7      0.10522      0.13258 -20.6    *
    2 9      0.04854      0.07399 -34.4    *
   20 5      0.05463      0.05463   0.0     
   20 7      0.02470      0.02470   0.0     
   20 9      0.08069      0.08069   0.0     

</pre>


**Test Case**: `reg_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 8.93s (new) 8.81s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    %differences (n-o):                  
      mean -0.1432
      min  -0.6019
      max   0.1657
    correlation:  0.9921 

 mtry k RMSE_new RMSE_old Diff Flag
   11 5    15.91    15.89  0.1    *
   11 7    16.07    16.09 -0.1    *
   11 9    16.14    16.11  0.2    *
    2 5    16.04    16.08 -0.3    *
    2 7    16.06    16.16 -0.6    *
    2 9    16.15    16.24 -0.6    *
   20 5    16.40    16.40  0.0     
   20 7    16.39    16.39  0.0     
   20 9    17.14    17.14  0.0     

</pre>

 * ***UNequal results for Rsquared***:

<pre>
    %differences (n-o):                 
      mean  18.29
      min  -15.95
      max  106.40
    correlation:  0.8033 

 mtry k Rsquared_new Rsquared_old  Diff Flag
   11 5      0.16913      0.17238  -1.9    *
   11 7      0.15997      0.13210  21.1    *
   11 9      0.14750      0.17548 -15.9    *
    2 5      0.13700      0.09039  51.6    *
    2 7      0.15459      0.07490 106.4    *
    2 9      0.09311      0.09006   3.4    *
   20 5      0.05463      0.05463   0.0     
   20 7      0.02470      0.02470   0.0     
   20 9      0.08069      0.08069   0.0     

</pre>


**Test Case**: `reg_loo_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Leave-One-Out Cross-Validation
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 9 tuning parameter combinations were evaluated


Execution times: (old) 1.33m (new) 1.33m

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    %differences (n-o):                  
      mean  0.1765
      min  -0.1248
      max   0.6413
    correlation:  0.9996 

 k mtry RMSE_new RMSE_old Diff Flag
 5   11    16.58    16.53  0.3    *
 5    2    16.33    16.31  0.1    *
 5   20    19.81    19.81  0.0     
 7   11    16.72    16.62  0.6    *
 7    2    16.42    16.44 -0.1    *
 7   20    18.29    18.29  0.0     
 9   11    16.75    16.70  0.3    *
 9    2    16.53    16.47  0.3    *
 9   20    18.23    18.23  0.0     

</pre>

 * ***UNequal results for Rsquared***:

<pre>
    %differences (n-o):                 
      mean  175.9
      min   -99.8
      max  1766.2
    correlation:  0.9929 

 k mtry Rsquared_new Rsquared_old   Diff Flag
 5   11    9.877e-03    1.288e-02  -23.3    *
 5    2    2.008e-02    2.423e-02  -17.1    *
 5   20    4.262e-02    4.262e-02    0.0     
 7   11    9.286e-04    4.079e-03  -77.2    *
 7    2    8.435e-03    6.273e-03   34.5    *
 7   20    1.332e-02    1.332e-02    0.0     
 9   11    1.664e-04    8.918e-06 1766.2    *
 9    2    3.919e-06    2.000e-03  -99.8    *
 9   20    4.463e-02    4.463e-02    0.0     

</pre>


**Test Case**: `reg_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (20), scaled (20)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 0.47s (new) 0.49s

Test Results:

 * _Equal results for RMSE_
 * _Equal results for Rsquared_

**Test Case**: `reg_none_pred`

Object class(es): `numeric`

 * ***UNequal results***:
<pre>
    %differences (n-o):                 
      mean   5.09
      min   -4.97
      max  102.66
    correlation:  0.998 
</pre>

**Test Case**: `reg_pred`

Object class(es): `numeric`

 * ***UNequal results***:
<pre>
    %differences (n-o):                  
      mean  -0.103
      min  -11.206
      max   49.987
    correlation:  0.998 
</pre>

**Test Case**: `reg_pred_form`

Object class(es): `numeric`

 * ***UNequal results***:
<pre>
    %differences (n-o):                  
      mean  -0.186
      min  -13.805
      max   11.776
    correlation:  0.998 
</pre>

**Test Case**: `reg_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: centered (20), scaled (20)  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 3.84s (new) 3.86s

Test Results:

 * ***UNequal results for RMSE***:

<pre>
    %differences (n-o):                  
      mean  0.6367
      min  -0.2619
      max   2.6547
    correlation:  0.9851 

 mtry  k RMSE_new RMSE_old Diff Flag
    1  8    16.57    16.55  0.1    *
   18  3    16.90    16.90  0.0    *
    8 10    16.09    16.13 -0.3    *
    9  2    15.53    15.13  2.7    *

</pre>

 * ***UNequal results for Rsquared***:

<pre>
    1 missing values in old
    1 missing values in new
    %differences (n-o):                 
      mean -10.32
      min  -24.62
      max    3.89
    correlation:  0.9777 

 mtry  k Rsquared_new Rsquared_old  Diff Flag
    1  8          NaN          NaN   NaN     
   18  3      0.05771      0.05555   3.9    *
    8 10      0.17051      0.18994 -10.2    *
    9  2      0.16381      0.21731 -24.6    *

</pre>


