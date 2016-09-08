`trimming`
 ===== 

There are regression tests to compare model results between different versions of `caret` and the individual packages. These test evaluate whether consistent results can be obtained. The code used to generate the objects that are compared can be found [here](https://github.com/topepo/caret/blob/master/RegressionTests/Code/trimming.R).

Testing Information:
---------

Old:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-71)


New:

 * x86_64-apple-darwin13.4.0 (64-bit)
 * R Under development (unstable) (2016-07-31 r71015)
 * `caret` (6.0-72)


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

