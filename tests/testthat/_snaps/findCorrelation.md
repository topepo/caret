# findCorrelation errors when names are requested but unavailable

    Code
      findCorrelation(m, names = TRUE)
    Condition
      Error in `findCorrelation()`:
      ! 'x' must have column names when `names = TRUE`

# findCorrelation prints details when verbose

    Code
      findCorrelation(corr_R2, cutoff = 0.65, verbose = TRUE)
    Output
      Compare row 3  and column  2 with corr  0.7 
        Means:  0.35 vs 0.246 so flagging column 3 
      Compare row 4  and column  1 with corr  0.67 
        Means:  0.39 vs 0.186 so flagging column 4 
      All correlations <= 0.65 
      [1] 3 4

---

    Code
      findCorrelation(corr_R1, cutoff = 0.6, exact = TRUE, verbose = TRUE)
    Output
      Compare row 1  and column  5 with corr  0.85 
        Means:  0.647 vs 0.545 so flagging column 1 
      Compare row 5  and column  3 with corr  0.91 
        Means:  0.53 vs 0.49 so flagging column 5 
      Compare row 3  and column  4 with corr  0.65 
        Means:  0.33 vs 0.353 so flagging column 4 
      All correlations <= 0.6 
      [1] 1 5 4

---

    Code
      findCorrelation(corr_R1, cutoff = 0.6, exact = FALSE, verbose = TRUE)
    Output
      
       Combination row 1 and column 2 is above the cut-off, value = 0.86 
       	 Flagging column 1 
       Combination row 2 and column 4 is above the cut-off, value = 0.74 
       	 Flagging column 4 
       Combination row 3 and column 4 is above the cut-off, value = 0.65 
       	 Flagging column 3 
       Combination row 1 and column 5 is above the cut-off, value = 0.85 
       	 Flagging column 1 
       Combination row 3 and column 5 is above the cut-off, value = 0.91 
       	 Flagging column 5 
      [1] 4 5 1 3

---

    Code
      findCorrelation(corr_R2, cutoff = 0.99, verbose = TRUE)
    Output
      All correlations <= 0.99 
      integer(0)

# findCorrelation_fast rejects missing values

    Code
      caret:::findCorrelation_fast(m, cutoff = 0.6)
    Condition
      Error in `caret:::findCorrelation_fast()`:
      ! The correlation matrix has some missing values.

# findCorrelation_exact rejects non-symmetric and singleton input

    Code
      caret:::findCorrelation_exact(ns)
    Condition
      Error in `caret:::findCorrelation_exact()`:
      ! correlation matrix is not symmetric

---

    Code
      caret:::findCorrelation_exact(matrix(1, 1, 1))
    Condition
      Error in `caret:::findCorrelation_exact()`:
      ! only one variable given

