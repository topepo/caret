If you are making a feature request or starting a discussion, you can ignore everything below and go wild =D

If you are filing a bug, make sure these boxes are checked before submitting your issue— thank you!

- [ ] Start a new R session
- [ ] Install the latest version of caretEnsemble: `devtools::install_github("zachmayer/caretEnsemble")`
- [ ] Install the latest version of caret: `update.packages(oldPkgs="caret", ask=FALSE)`
- [ ] [Write a minimal reproducible example](http://stackoverflow.com/a/5963610)
- [ ] run `sessionInfo()`

### Minimal, reproducible example:
Text and example code modified from [the R FAQ on stackoverflow](http://stackoverflow.com/a/5963610)

#### Minimal dataset:
```{R}
set.seed(1)
dat <- caret::twoClassSim(100)
X <- dat[,1:5]
y <- dat[["Class"]]

```
If you have some data that would be too difficult to construct using `caret::twoClassSim` or `caret::SLC14_1`, then you can always make a subset of your original data, using e.g. `head()`, `subset()` or the indices. Then use e.g. `dput()` to give us something that can be put in R immediately, e.g. `dput(head(iris,4))`

If you must use `dput(head())`, please first remove an columns from your dataset that are not necessary to reproduce the error.

If your data frame has a factor with many levels, the `dput` output can be unwieldy because it will still list all the possible factor levels even if they aren't present in the the subset of your data. To solve this issue, you can use the `droplevels()` function. Notice below how species is a factor with only one level: `dput(droplevels(head(iris, 4)))`

#### Minimal, runnable code:
```{R}
library(caretEnsemble)
models <- caretList(
  X, y, 
  methodList=c('glm', 'rpart'),
  trControl=trainControl(
    method="cv", 
    number=5,
    classProbs=TRUE, 
    savePredictions="final")
)
ens <- caretStack(models)
print(ens)
```

### Session Info:
```{R}
>sessionInfo()

```

You can delete the text in each section that explains how to do it correctly.
Be sure to test your 2 chunks of code in an empty R session before submitting your issue!