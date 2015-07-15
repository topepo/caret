Thoughts on survival analysis in `caret`
===

Problem type
--

Right now, we have "regression" and "classification" and this delineation serves almost as a class.

For survival models, I can envision two more problem types. First, would be called something like "Censored Regression" where the goal is to predict the actual value, as we would in ordinary regression, but be able to accommodate censoring. That is where most of my applications reside. 

A second problem type might be called something like "Hazard Regression" and is more focused on predicting the probability of survival a specific time point. This would encompass a lot of different models that don't directly mold (or predict) the actual outcome. Here, we would significantly leverage the [`pec`](http://www.jstatsoft.org/v50/i11/) package. 

I'm not sure how the user would choose the problem type. Right now, it is set based on the class of the outcome (e.g. numeric versus factor). We use the class of the outcome to determine that the problem is not classification or regression but it doesn't help us choose between modeling the outcome or the risk. 

The structural main difference in the survival model code is related to what "[modules](http://topepo.github.io/caret/custom_models.html#Components)" they have. For example, some classification models produce class probabilities and regression models are completely ignorant of this. Similarly, we might be able to differentiate "Hazard Regression" and "Censored Regression" based on what code is available for different models. For example, some modules might be:

* `fit`: fits the model based on the training data
* `pred`: predict the outcome (e.g. time). This could be `NULL` for Hazard Regression.
* `prob`: predict the probability of survival at time point `t`. This would be `NULL` for Censored Regression. 


**Q1** How should the user choose the problem type for survival models?

Resampling
--

Operationally, how do we resample censored data? We can treat them like numbers and try to make the distributions of the data consistent between the splits. This might break down for highly censored data. Alternatively, we could based the modeling/holdout split in a way that keeps the percent censored equal. 

**Q2** How should balanced data splitting occur with censored data?

Measuring Performance
--

Regression and classification have default metrics (e.g. RMSE, accuracy, etc). The nature of the performance metric depends on the problem type. 

* When we directly predict the outcome, [`survConcordance.fit`](http://www.inside-r.org/packages/cran/survival/docs/survConcordance) seems reasonable. 
* With probability models, the Brier, c-index, and various ROC curve metrics are available. 

Possible Models
---




First, let's simulate some data:

```
library(prodlim)
set.seed(43500)
train_dat <- SimSurv(200)
set.seed(1742)
test_dat <- SimSurv(3)
test_pred <- test_dat[, 4:5]
```


### `survival:::`[`survreg`](http://www.inside-r.org/packages/cran/survival/docs/survreg)

(hazard and censored). 


```
library(survival)
sr_mod <- survreg(Surv(time, status) ~ (X1+X2)^2, data = train_dat)
```

The `predict` function can generate predictions for the outcome and/or percentiles of the survival function via 


```
predict(sr_mod, test_pred)
```

```
       1        2        3 
4.074870 2.001814 7.533555 
```

```
predict(sr_mod, test_pred, type='quantile', p=c(.1, .5, .9))
```

```
          [,1]     [,2]      [,3]
[1,] 1.1987646 3.338635  6.412893
[2,] 0.5889032 1.640133  3.150388
[3,] 2.2162573 6.172416 11.856057
```

Note that the latter is in terms of the time units and are not probabilities of survival by a specified time. We would need to basically invert this (e.g. get the percentile for a given time).

### `survival:::`[`coxph`](http://www.inside-r.org/packages/cran/survival/docs/coxph)


```
cph_mod <- coxph(Surv(time, status) ~ (X1+X2)^2, data = train_dat)
```

There is no way to directly predict the outcome but we can get predictions but the `pec` package will produce survivor function probabilities:


```
library(pec)
predictSurvProb(cph_mod, newdata = test_pred, times = c(1, 5, 10))
```

```
       [,1]        [,2]         [,3]
1 0.9403543 0.245820986 4.005669e-03
2 0.7963222 0.005536592 1.323861e-09
3 0.9803677 0.636108287 1.686882e-01
```

### `rpart:::`[`rpart`](http://www.inside-r.org/packages/cran/rpart/docs/rpart)


```
library(rpart)
rp_mod <- rpart(Surv(time, status) ~ X1+X2, data = train_dat)
```

The basic invocation of `predict` generates the predicted outcome:


```
predict(rp_mod, test_pred)
```

```
       1        2        3 
2.916143 5.297942 1.454437 
```

The `pec` package can get the survivor probabilities:


```
predictSurvProb(rp_mod, newdata = test_pred, 
                train.data = train_dat, times = c(1, 5, 10))
```

```
                                  [,1]      [,2]       [,3]
rpartFactor=2.91614334259577 0.8571429 0.1428571         NA
rpartFactor=5.29794160988782 0.8571429        NA         NA
rpartFactor=1.4544374800168  0.9655172 0.5336246 0.05558589
```

### `ipred:::`[`bagging`](http://www.inside-r.org/packages/cran/ipred/docs/bagging)


```
library(ipred)
bag_mod <- bagging(Surv(time, status) ~ X1+X2, data = train_dat)
```

When generating predictions, `survfit` objects is returned for each data point being predicted. To get the median survival time:


```
bag_preds <- predict(bag_mod, test_pred)
bag_preds
```

```
[[1]]
Call: survfit(formula = Surv(agglsample[[j]], aggcens[[j]]) ~ 1)

records   n.max n.start  events  median 0.95LCL 0.95UCL 
 831.00  831.00  831.00  772.00    3.43    3.41    3.61 

[[2]]
Call: survfit(formula = Surv(agglsample[[j]], aggcens[[j]]) ~ 1)

records   n.max n.start  events  median 0.95LCL 0.95UCL 
 378.00  378.00  378.00  363.00    1.22    1.10    1.41 

[[3]]
Call: survfit(formula = Surv(agglsample[[j]], aggcens[[j]]) ~ 1)

records   n.max n.start  events  median 0.95LCL 0.95UCL 
 557.00  557.00  557.00  412.00    5.64    5.32    5.64 
```

```
unlist(lapply(bag_preds, function(x) quantile(x, probs = .5)$quantile))
```

```
      50       50       50 
3.434330 1.217630 5.635328 
```

```
## now use pec to convert this to survivor probabilities
predictSurvProb(bag_preds[[1]], newdata = test_pred[1,], times = c(1, 5, 10))
```

```
          [,1]      [,2]      [,3]
[1,] 0.9109507 0.2391471 0.2391471
```

### `party:::`[`ctree`](http://www.inside-r.org/packages/cran/party/docs/ctree)



```
library(party)
ctree_mod <- ctree(Surv(time, status) ~ X1+X2, data = train_dat)
```

The basic invocation of `predict` generates the predicted outcome.


```
predict(ctree_mod, test_pred)
```

```
[1] 3.612733 1.407054 5.324036
```


### `party:::`[`cforest`](http://www.inside-r.org/packages/cran/party/docs/cforest)


```
library(party)
cforest_mod <- cforest(Surv(time, status) ~ X1+X2, data = train_dat, 
                  control = cforest_unbiased(ntree = 100, mtry = 1))
```

The basic invocation of `predict` generates the predicted outcome although there tends to be a lot of `Inf` results. 


```
predict(cforest_mod, newdata = test_pred)
```

```
       1        2        3 
3.612733 3.421180 4.881756 
```


### `randomForestSRC:::`[`rfsrc`](http://www.inside-r.org/packages/cran/randomForestSRC/docs/rfsrc)


```
library(randomForestSRC)
rfsrce_mod <- rfsrc(Surv(time, status) ~ X1+X2, data = train_dat, 
                    ntree = 100)
```

The `predict` function generates an object with classes `"rfsrc"`, `"predict"`, and `"surv". It is unclear what is being predicted:


```
predict(rfsrce_mod, test_pred)$predicted
```

```
[1]  83.09505 189.84237  24.42533
```

The `survival` slot appears to be survival probabilities for some unknown reference values:


```
round(predict(rfsrce_mod, test_pred)$survival, 2)
```

```
     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17]
[1,]    1    1    1 1.00 0.99 0.99 0.99 0.99 0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99
[2,]    1    1    1 0.78 0.78 0.78 0.78 0.58 0.58  0.58  0.51  0.33  0.28  0.28  0.28  0.08  0.07
[3,]    1    1    1 1.00 1.00 1.00 0.99 0.99 0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99
     [,18] [,19] [,20] [,21] [,22] [,23] [,24] [,25] [,26] [,27] [,28] [,29] [,30] [,31] [,32]
[1,]  0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99
[2,]  0.07  0.07  0.07  0.07  0.07  0.03  0.02  0.02  0.02  0.02  0.02  0.02  0.02  0.02  0.02
[3,]  0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.99  0.97  0.97  0.97  0.97  0.97  0.96  0.96
     [,33] [,34] [,35] [,36] [,37] [,38] [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46] [,47]
[1,]  0.99  0.99  0.99  0.93  0.93  0.93  0.93  0.93  0.93  0.93  0.93  0.93  0.92  0.92  0.92
[2,]  0.02  0.02  0.02  0.02  0.02  0.02  0.02  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00
[3,]  0.96  0.96  0.96  0.96  0.96  0.96  0.96  0.96  0.96  0.96  0.96  0.96  0.96  0.96  0.96
     [,48] [,49] [,50] [,51] [,52] [,53] [,54] [,55] [,56] [,57] [,58] [,59] [,60] [,61] [,62]
[1,]  0.92  0.92  0.91  0.82  0.82  0.82  0.82  0.82  0.82  0.82  0.82  0.78  0.78  0.78  0.78
[2,]  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00
[3,]  0.96  0.96  0.96  0.96  0.96  0.96  0.96  0.96  0.96  0.96  0.95  0.95  0.95  0.95  0.95
     [,63] [,64] [,65] [,66] [,67] [,68] [,69] [,70] [,71] [,72] [,73] [,74] [,75] [,76] [,77]
[1,]  0.78  0.55  0.55  0.45  0.45  0.45  0.45  0.45  0.45  0.45  0.25  0.25  0.25  0.25  0.25
[2,]  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00
[3,]  0.95  0.95  0.95  0.95  0.95  0.95  0.95  0.95  0.95  0.95  0.95  0.95  0.95  0.95  0.95
     [,78] [,79] [,80] [,81] [,82] [,83] [,84] [,85] [,86] [,87] [,88] [,89] [,90] [,91] [,92]
[1,]  0.15  0.15  0.15  0.15  0.15  0.15  0.15  0.15  0.15  0.15  0.15  0.15  0.15  0.15  0.15
[2,]  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00  0.00
[3,]  0.95  0.95  0.95  0.86  0.86  0.86  0.86  0.86  0.86  0.86  0.86  0.86  0.86  0.86  0.86
     [,93] [,94] [,95] [,96] [,97] [,98] [,99] [,100] [,101] [,102] [,103] [,104] [,105] [,106]
[1,]  0.15  0.15  0.15  0.15  0.15  0.15  0.15   0.15   0.15   0.15   0.15   0.09   0.00    0.0
[2,]  0.00  0.00  0.00  0.00  0.00  0.00  0.00   0.00   0.00   0.00   0.00   0.00   0.00    0.0
[3,]  0.86  0.86  0.86  0.86  0.86  0.86  0.86   0.68   0.68   0.68   0.68   0.68   0.68    0.3
     [,107] [,108] [,109] [,110] [,111] [,112] [,113] [,114] [,115] [,116] [,117] [,118] [,119]
[1,]    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0   0.00   0.00   0.00
[2,]    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0   0.00   0.00   0.00
[3,]    0.3    0.3    0.3    0.3    0.3    0.3    0.3    0.3    0.3    0.3   0.07   0.07   0.07
     [,120] [,121]
[1,]   0.00   0.00
[2,]   0.00   0.00
[3,]   0.06   0.06
```

### `mboost:::`[`blackboost`](http://www.inside-r.org/packages/cran/mboost/docs/blackboost)

The model can be fit using parametric assumptions and predictions can be made for the outcome:


```
library(mboost)
bb_mod <- blackboost(Surv(time, status) ~ X1+X2, data = train_dat, family = Lognormal())
```

For outcome predictions, we can predict using the basic syntax:


```
predict(bb_mod, newdata = test_pred, type = "response")
```

```
          [,1]
[1,] 3.7293262
[2,] 0.8929852
[3,] 6.4422546
```

Additional Classes/Modules/Changes
---

* For the `predict` function, an option would be added to specify the time value for generating probability estimates. This type of prediction might require some structural changes when there are multiple times being predicted.
* Right now, the code assumes that there is a single problem type. We might want to have model than one now. 




