# Give `NA` metrics for failed models in the probability-based summaries

## Context

The move to yardstick (#1539) introduced a regression that is in the 7.1-0
release candidate but has never been on CRAN, so no NEWS entry is needed.

When a model fails during resampling, `fill_failed_prob()` fills its class
probabilities with logical `NA`. Before #1539, the summary functions returned
`NA` for these resamples. Now:

- yardstick errors on logical estimates, so `twoClassSummary()`,
  `multiClassSummary()`, and `prSummary()` error for a failed resample.
- `yardstick::pr_auc_vec()` returns 0, not `NA`, when every estimate is
  missing, so `prAUC` (and `prSummary()`'s `AUC`) is reported as 0.
- `mnLogLoss()` drops incomplete rows, so for a failed model it calls
  `mn_log_loss_vec()` on zero rows.

A first version of this fix was written on `RC-7-1-0` but was never committed
and is gone. Redo it on a new branch from `main`.

## Changes

1. **`R/yardstick.R`**: add an internal wrapper:

   ```r
   # When a model fails, all of its class probabilities are missing (and logical
   # when they come from the user's data). yardstick errors on logical estimates
   # and returns 0 for the PR AUC of an empty curve, so these are NA instead.
   prob_metric <- function(fn, truth, estimate) {
     if (all(is.na(estimate))) {
       return(NA_real_)
     }
     suppressWarnings(fn(truth, estimate))
   }
   ```

   Check that `suppressWarnings()` doesn't hide warnings that the current code
   lets through; if it does, keep the current behavior instead.

2. **`R/misc.R`**: use `matrix(NA_real_, ...)` in `fill_failed_prob()` and in
   the matching filler in `optimism_xy()` (both are `matrix(NA, ...)` on
   `main`).

3. **`R/postResample.R`**:
   - `twoClassSummary()`: `prob_metric(yardstick::roc_auc_vec, data$obs, data[, lev[1]])`.
   - `multiClassSummary()`: use `prob_metric()` for the per-class `ROC` and
     `AUC` (PR AUC) calls.
   - `mnLogLoss()`: return `c(logLoss = NA_real_)` when
     `nrow(dataComplete) == 0`.

4. **`R/prec_rec.R`**: `prSummary()`'s `AUC` uses
   `prob_metric(yardstick::pr_auc_vec, ...)`.

## Tests (`tests/testthat/test-postResample.R`, plus `test-prec_rec.R` if that fits better)

- "summary functions give NA when the model failed": call each of
  `twoClassSummary()`, `multiClassSummary()`, `prSummary()`, and
  `mnLogLoss()` on data whose probability columns are all logical `NA`, and on
  all `NA_real_`. Check that the probability metrics are `NA` (not 0, no
  error) and that the class-prediction metrics are unaffected.
- "train() gives NA performance for failed models": a custom model based on
  `glm` with a dummy tuning parameter `fail`; `fit` stops when `fail == 1`.
  Tune over `fail = 0:1` with `classProbs = TRUE` and `twoClassSummary`, and
  check that the `fail = 1` row of `results` has `NA` metrics while the other
  row does not. Silence the expected failure warnings with a snapshot, not by
  leaving output.
- Confirm that both tests fail on `main` before the fix.

## Verification

1. `devtools::test()`: no failures, no extra printed output.
2. `air format R/ tests/`.
3. Grep the touched files for `<-\s*if\s*\(`, `return\(if`, and
   `if \(.*\) [^{]`, and fix any hits.
4. `R CMD check` (not `--as-cran`).
5. Stop for review; no commits by the assistant.
