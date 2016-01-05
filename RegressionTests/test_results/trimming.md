`trimming`
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/trimming.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2015-11-18 r69655)
 * `caret` (6.0-62)


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2015-11-18 r69655)
 * `caret` (6.0-64)


Results:
---------

**Test Case**: `1_pred`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `10_pred`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `11_pred`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `12_pred`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `2_pred`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `3_pred`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `4_pred`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `5_pred`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `6_pred`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `7_pred`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `8_pred`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `9_pred`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `ext_pred`

Object class(es): `data.frame`

 * ***UNequal results***: differences (o-n):
<pre>
       p1          p2          p3          p4          p5          p6   
 Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
 1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
 Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
 Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
 3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
 Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
       p7          p8          p9         p10         p11         p12   
 Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0   Min.   :0  
 1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0   1st Qu.:0  
 Median :0   Median :0   Median :0   Median :0   Median :0   Median :0  
 Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0   Mean   :0  
 3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:0  
 Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0   Max.   :0  
</pre>

