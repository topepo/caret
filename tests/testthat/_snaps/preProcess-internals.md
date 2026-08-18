# check_for_wildcards reports and strips PCA/ICA wildcards

    Code
      res <- caret:::check_for_wildcards(opts)
    Output
      PCA wildcards found for: pca, center ...but should not be in methods: pca, center

# pre_process_options rejects unknown methods and missing fields

    Code
      caret:::pre_process_options(list(bogus = "a"), vars)
    Condition
      Error in `caret:::pre_process_options()`:
      ! These pre-processing methods are unknown: 'bogus'

---

    Code
      caret:::pre_process_options(list(center = "nope"), vars)
    Condition
      Error in `caret:::pre_process_options()`:
      ! These fields are not in the data: 'nope'

# pre_process_options drops group transformations of one predictor

    PCA is a group transformation and only a single predictor is listed. This method is eliminated.

---

    ICA is a group transformation and only a single predictor is listed. This method is eliminated.

---

    Spatial sign is a group transformation and only a single predictor is listed. This method is eliminated.

# pre_process_options warns when PCA and ICA overlap

    fastICA automatically uncorrelates the data using PCA. method = 'pca' is not needed for fields: 'c'

# pre_process_options rejects conflicting options

    Code
      caret:::pre_process_options(list(knnImpute = "a", medianImpute = c("a", "b")),
      vars)
    Condition
      Error in `caret:::pre_process_options()`:
      ! Please pick a single imputation method for: 'a'

---

    Code
      caret:::pre_process_options(list(range = "a", center = "b"), vars)
    Condition
      Error in `caret:::pre_process_options()`:
      ! Centering, scaling and/or Box-Cox transformations are inconsistent with scaling to a range

# check_for_wildcards reports the PCA and ICA wildcards

    Code
      res <- caret:::check_for_wildcards(opts, verbose = TRUE)
    Output
      PCA wildcards found for: spatialSign
      ICA wildcards found for: center 
       ...but should not be in methods: center

# check_for_wildcards rejects wildcards in the wrong methods

    Code
      res <- caret:::check_for_wildcards(opts, verbose = TRUE)
    Output
      PCA wildcards found for: pca ...but should not be in methods: pca
      ICA wildcards found for: ica 
       ...but should not be in methods: ica

# pre_process_options drops methods with no columns to work on

    The following pre-processing methods were eliminated: 'center'

# get_types needs column names

    Code
      caret:::get_types(x)
    Condition
      Error in `caret:::get_types()`:
      ! `x` must have column names

