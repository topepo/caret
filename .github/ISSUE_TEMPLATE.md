If you are making a feature request or starting a discussion, you can ignore everything below and go wild =D However, please request **new models** using the [project page](https://github.com/topepo/caret/projects/2).

If you are filing a bug, make sure these boxes are checked before submitting your issue— thank you!

- [ ] Start a new R session
- [ ] Install the latest version of caret: `update.packages(oldPkgs="caret", ask=FALSE)`
- [ ] [Write a minimal reproducible example](http://stackoverflow.com/a/5963610)
- [ ] **Do not** use parallel processing in the code (unless you are certain that the issue is about parallel processing).
- [ ] run `sessionInfo()`

### Minimal, reproducible example:

__Please read this page__: [reprex = {repr}oducible {ex}ample](https://github.com/jennybc/reprex#what-is-a-reprex); your issue can be easily formatted using the **[`reprex` package](https://cran.r-project.org/package=reprex)**. 

Text and example code modified from [the R FAQ on stackoverflow](http://stackoverflow.com/a/5963610)

#### Minimal dataset:
```{R}
library(caret)
set.seed(1)
dat <- twoClassSim(100)
X <- dat[,1:5]
y <- dat[["Class"]]

```
If you have some data that would be too difficult to construct using `caret::twoClassSim` or `caret::SLC14_1`, then you can always make a subset of your original data, using e.g. `head()`, `subset()` or the indices. Then use e.g. `dput()` to give us something that can be put in R immediately, e.g. `dput(head(iris,4))`

If you must use `dput(head())`, please first remove an columns from your dataset that are not necessary to reproduce the error.

If your data frame has a factor with many levels, the `dput` output can be unwieldy because it will still list all the possible factor levels even if they aren't present in the the subset of your data. To solve this issue, you can use the `droplevels()` function. Notice below how species is a factor with only one level: `dput(droplevels(head(iris, 4)))`

#### Minimal, runnable code:
```{R}
model_class <- train(
  X, y, 
  metric='ROC',
  trControl=trainControl(
    method="cv", 
    number=5,
    classProbs=TRUE, 
    summaryFunction=twoClassSummary,
    savePredictions="final")
)
print(model_class)
```

### Session Info:
```{R}
>sessionInfo()

```

You can delete the text in each section that explains how to do it correctly.
Be sure to test your 2 chunks of code in an empty R session before submitting your issue!