`trimming`
===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/trimming.R).
A [history of commits](https://github.com/topepo/caret/commits/master/models/files/trimming.R) for the model code is also available

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-73)
 * tested on 2017-04-12 at 18:57. 
 * total test time: 11.2s


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R version 3.3.3 (2017-03-06)
 * `caret` (6.0-75)
 * tested on 2017-04-11 at 20:28. 
 * total test time: 8.2s


Results:
---------

**Test Case**: `external_cv_booleans`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `external_cv_bottom_only`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `external_cv_no_trimming`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `external_cv_top_only`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `external_loo_booleans`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `external_loo_bottom_only`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `external_loo_no_trimming`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `external_loo_top_only`

Object class(es): `numeric`

 * _Equal results_

**Test Case**: `internal_cv_booleans`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `internal_cv_bottom_only`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `internal_cv_no_trimming`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `internal_cv_top_only`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `internal_loo_booleans`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `internal_loo_bottom_only`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `internal_loo_no_trimming`

Object class(es): `data.frame`

 * _Equal results_

**Test Case**: `internal_loo_top_only`

Object class(es): `data.frame`

 * _Equal results_

