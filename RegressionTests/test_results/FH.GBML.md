Fuzzy Rules Using Genetic Cooperative-Competitive Learning and Pittsburgh (`FH.GBML`)
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/FH.GBML.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/FH.GBML.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-71), `frbs` (3.1-0)
 * tested on 2016-10-31 at 05:36


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-10-26 r71594)
 * `caret` (6.0-72), `frbs` (3.1-0)
 * tested on 2016-10-30 at 22:44


Results:
---------

**Test Case**: `class_cv_form`

Object class(es): `train` and `train.formula`

Model Configuration:

 * Formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 1.33m (new) 1.21m

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences (n-o):                
      mean 37.79
      min  14.44
      max  61.14
    correlation:  -1 

 max.num.rule popu.size max.gen Accuracy_new Accuracy_old Diff Flag
            3        10      10       0.5404       0.3354 61.1    *
            5        10      10       0.5337       0.4663 14.4    *

</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences < 0.1%
</pre>


**Test Case**: `class_cv_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 1.35m (new) 1.24m

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences (n-o):                
      mean 37.79
      min  14.44
      max  61.14
    correlation:  -1 

 max.num.rule popu.size max.gen Accuracy_new Accuracy_old Diff Flag
            3        10      10       0.5404       0.3354 61.1    *
            5        10      10       0.5337       0.4663 14.4    *

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
 * Pre-processing: centered (3), scaled (3)  
 * 2 tuning parameter combinations were evaluated


Execution times: (old) 9.29m (new) 9.84m

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences (n-o):                 
      mean  1.389
      min  -5.556
      max   8.333
    correlation:  1 

 max.num.rule popu.size max.gen Accuracy_new Accuracy_old Diff Flag
            3        10      10       0.4333          0.4  8.3    *
            5        10      10       0.5667          0.6 -5.6    *

</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences < 0.1%
</pre>


**Test Case**: `class_none_model`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: None
 * Grid search
 * Pre-processing: centered (3), scaled (3)  
 * 0 tuning parameter combinations were evaluated


Execution times: (old) 13.26s (new) 8.75s

Test Results:

 * _Equal results for Accuracy_
 * _Equal results for Kappa_

**Test Case**: `class_none_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 46.7% (14/30)
        newer
older    Class1 Class2
  Class1      4      2
  Class2     14     10
</pre>

**Test Case**: `class_pred`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 46.7% (14/30)
        newer
older    Class1 Class2
  Class1      4      2
  Class2     14     10
</pre>

**Test Case**: `class_pred_form`

Object class(es): `factor`

 * ***UNequal results***:
<pre>
   Agreement: 46.7% (14/30)
        newer
older    Class1 Class2
  Class1      4      2
  Class2     14     10
</pre>

**Test Case**: `class_rand`

Object class(es): `train`

Model Configuration:

 * Non-formula method
 * Resampling: Cross-Validated (3 fold)
 * Random search
 * Pre-processing: None  
 * 4 tuning parameter combinations were evaluated


Execution times: (old) 4.86m (new) 4.82m

Test Results:

 * ***UNequal results for Accuracy***:

<pre>
    %differences (n-o):                  
      mean  -4.244
      min  -13.560
      max    7.645
    correlation:  0.3412 

 max.gen popu.size max.num.rule Accuracy_new Accuracy_old  Diff Flag
       1        18            5       0.4529       0.5239 -13.6    *
       4        10            4       0.4694       0.4360   7.6    *
       5         2           15       0.4929       0.4936  -0.1    *
       8        20           20       0.4997       0.5609 -10.9    *

</pre>

 * ***UNequal results for Kappa***:

<pre>
    %differences < 0.1%
</pre>


**Test Case**: `levels`

Object class(es): `character`

 * _Equal results_

