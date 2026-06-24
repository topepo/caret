# `predict.plsda()` and `predict.splsda()` fail with `probMethod = "Bayes"`

## Description

`predict()` on `plsda` or `splsda` objects fitted with `probMethod = "Bayes"` fails with:

```
Error in predict.NaiveBayes(...) :
  Not all variable names used in object found in newdata
```

This affects both `plsda()` and `caret:::splsda()`.

## Root cause

There are two issues in `predict.plsda()` (R/plsda.R):

### 1. Column name lost during subsetting (primary error)

At prediction time, line 165 does:

```r
tmpPred[, -length(object$obsLevels), i]
```

For a 2-class problem, this drops the second column from a 3D array slice. When only one column remains, R drops the dimension and returns a **named vector** instead of a single-column matrix. When this vector is passed to `as.data.frame()` on line 165, the resulting column is named after the variable (e.g., a generic name), **not** the original class level name (e.g., `"Active"`).

The `NaiveBayes` model was trained on scores with column names matching the class levels, so `predict.NaiveBayes()` raises a "variable names not found" error.

A `drop = FALSE` on the subsetting would fix this.

### 2. `predict.mvr()` ignoring `newdata` (secondary issue)

At line 118, `predict.plsda()` calls:

```r
tmpPred <- predict(object, newdata, ncomp = ncomp)
```

However, `predict.mvr()` from the **pls** package returns predictions based on the *training* data dimensions (450 rows) rather than the test data (78 rows), with the warning:

```
'newdata' had 78 rows but variables found have 450 rows
```

This means even if the column-name issue were fixed, the predictions would be wrong.

## Reprex

```r
library(caret)

data(mdrr)
set.seed(1)
inTrain <- sample(seq(along.with = mdrrClass), 450)

nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]

training <- filteredDescr[inTrain, ]
test     <- filteredDescr[-inTrain, ]
trainMDRR <- mdrrClass[inTrain]
testMDRR  <- mdrrClass[-inTrain]

preProcValues <- preProcess(training)
trainDescr <- predict(preProcValues, training)
testDescr  <- predict(preProcValues, test)

# Softmax works fine
useSoftmax <- plsda(trainDescr, trainMDRR, ncomp = 5)
confusionMatrix(predict(useSoftmax, testDescr), testMDRR)
#> Accuracy : 0.8333 ...

# Bayes fails
useBayes <- plsda(trainDescr, trainMDRR, ncomp = 5, probMethod = "Bayes")
predict(useBayes, testDescr)
#> Error in predict.NaiveBayes(...) :
#>   Not all variable names used in object found in newdata

# Demonstrating the column-name issue:
cls <- class(useBayes)
class(useBayes) <- "mvr"
tmpPred <- predict(useBayes, testDescr, ncomp = 5)

# NaiveBayes model expects a column named "Active":
useBayes$probModel[[5]]$varnames
#> [1] "Active"

# But subsetting drops the name:
oneSlice <- tmpPred[, -2, 1]
names(as.data.frame(oneSlice))
#> [1] "oneSlice"   # <-- should be "Active"
```

## Session info

```
R version 4.6.0 (2026-04-24)
caret 7.0-1
pls 2.9-0
klaR 1.7-4
```
