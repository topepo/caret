\name{train_model_list}
\alias{train_model_list}
\alias{models}
\title{A List of Available Models in train}
\description{These models are included in the package via wrappers for \code{\link{train}}. Custom models can also be created. See the URL below.

\strong{Bagged CART} (\code{method = 'treebag'})

For classification and regression using packages \pkg{ipred} and \pkg{plyr} with no tuning parameters



\strong{Bagged Flexible Discriminant Analysis} (\code{method = 'bagFDA'})

For classification using packages \pkg{earth} and \pkg{mda} with tuning parameters:
\itemize{
\item Product Degree (\code{degree})
\item Number of Terms (\code{nprune})
}


\strong{Bagged Logic Regression} (\code{method = 'logicBag'})

For classification and regression using package \pkg{logicFS} with tuning parameters:
\itemize{
\item Maximum Number of Leaves (\code{nleaves})
\item Number of Trees (\code{ntrees})
}


\strong{Bagged MARS} (\code{method = 'bagEarth'})

For classification and regression using package \pkg{earth} with tuning parameters:
\itemize{
\item Number of Terms (\code{nprune})
\item Product Degree (\code{degree})
}


\strong{Bagged Model} (\code{method = 'bag'})

For classification and regression using package \pkg{caret} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{vars})
}


\strong{Bayesian Generalized Linear Model} (\code{method = 'bayesglm'})

For classification and regression using package \pkg{arm} with no tuning parameters



\strong{Bayesian Regularized Neural Networks} (\code{method = 'brnn'})

For regression using package \pkg{brnn} with tuning parameters:
\itemize{
\item Number of  Neurons (\code{neurons})
}


\strong{Boosted Classification Trees} (\code{method = 'ada'})

For classification using package \pkg{ada} with tuning parameters:
\itemize{
\item Number of Trees (\code{iter})
\item Max Tree Depth (\code{maxdepth})
\item Learning Rate (\code{nu})
}


\strong{Boosted Generalized Additive Model} (\code{method = 'gamboost'})

For classification and regression using package \pkg{mboost} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{mstop})
\item AIC Prune? (\code{prune})
}


\strong{Boosted Generalized Linear Model} (\code{method = 'glmboost'})

For classification and regression using package \pkg{mboost} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{mstop})
\item AIC Prune? (\code{prune})
}


\strong{Boosted Linear Model} (\code{method = 'bstLs'})

For classification and regression using packages \pkg{bst} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{mstop})
\item Shrinkage (\code{nu})
}


\strong{Boosted Logistic Regression} (\code{method = 'LogitBoost'})

For classification using package \pkg{caTools} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{nIter})
}


\strong{Boosted Smoothing Spline} (\code{method = 'bstSm'})

For classification and regression using packages \pkg{bst} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{mstop})
\item Shrinkage (\code{nu})
}


\strong{Boosted Tree} (\code{method = 'blackboost'})

For classification and regression using packages \pkg{party}, \pkg{mboost} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of Trees (\code{mstop})
\item Max Tree Depth (\code{maxdepth})
}


\strong{Boosted Tree} (\code{method = 'bstTree'})

For classification and regression using packages \pkg{bst} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{mstop})
\item Max Tree Depth (\code{maxdepth})
\item Shrinkage (\code{nu})
}


\strong{C4.5-like Trees} (\code{method = 'J48'})

For classification using package \pkg{RWeka} with tuning parameters:
\itemize{
\item Confidence Threshold (\code{C})
}


\strong{C5.0} (\code{method = 'C5.0'})

For classification using packages \pkg{C50} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{trials})
\item Model Type (\code{model})
\item Winnow (\code{winnow})
}


\strong{CART} (\code{method = 'rpart'})

For classification and regression using package \pkg{rpart} with tuning parameters:
\itemize{
\item Complexity Parameter (\code{cp})
}


\strong{CART} (\code{method = 'rpart2'})

For classification and regression using package \pkg{rpart} with tuning parameters:
\itemize{
\item Max Tree Depth (\code{maxdepth})
}


\strong{Conditional Inference Random Forest} (\code{method = 'cforest'})

For classification and regression using package \pkg{party} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry})
}


\strong{Conditional Inference Tree} (\code{method = 'ctree'})

For classification and regression using package \pkg{party} with tuning parameters:
\itemize{
\item 1 - P-Value Threshold (\code{mincriterion})
}


\strong{Conditional Inference Tree} (\code{method = 'ctree2'})

For classification and regression using package \pkg{party} with tuning parameters:
\itemize{
\item Max Tree Depth (\code{maxdepth})
}


\strong{Cost-Sensitive C5.0} (\code{method = 'C5.0Cost'})

For classification using packages \pkg{C50} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{trials})
\item Model Type (\code{model})
\item Winnow (\code{winnow})
\item Cost (\code{cost})
}


\strong{Cost-Sensitive CART} (\code{method = 'rpartCost'})

For classification using package \pkg{rpart} with tuning parameters:
\itemize{
\item Complexity Parameter (\code{cp})
\item Cost (\code{Cost})
}


\strong{Cubist} (\code{method = 'cubist'})

For regression using package \pkg{Cubist} with tuning parameters:
\itemize{
\item Number of Committees (\code{committees})
\item Number of Instances (\code{neighbors})
}


\strong{Elasticnet} (\code{method = 'enet'})

For regression using package \pkg{elasticnet} with tuning parameters:
\itemize{
\item Fraction of Full Solution (\code{fraction})
\item Weight Decay (\code{lambda})
}


\strong{Extreme Learning Machine} (\code{method = 'elm'})

For classification and regression using package \pkg{elmNN} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{nhid})
\item Activation Function (\code{actfun})
}


\strong{Factor-Based Linear Discriminant Analysis} (\code{method = 'RFlda'})

For classification using package \pkg{HiDimDA} with tuning parameters:
\itemize{
\item Number of  Factors (\code{q})
}


\strong{Flexible Discriminant Analysis} (\code{method = 'fda'})

For classification using packages \pkg{earth} and \pkg{mda} with tuning parameters:
\itemize{
\item Product Degree (\code{degree})
\item Number of Terms (\code{nprune})
}


\strong{Gaussian Process} (\code{method = 'gaussprLinear'})

For classification and regression using package \pkg{kernlab} with no tuning parameters



\strong{Gaussian Process with Polynomial Kernel} (\code{method = 'gaussprPoly'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Polynomial Degree (\code{degree})
\item Scale (\code{scale})
}


\strong{Gaussian Process with Radial Basis Function Kernel} (\code{method = 'gaussprRadial'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Sigma (\code{sigma})
}


\strong{Generalized Additive Model using LOESS} (\code{method = 'gamLoess'})

For classification and regression using package \pkg{gam} with tuning parameters:
\itemize{
\item Span (\code{span})
\item Degree (\code{degree})
}


\strong{Generalized Additive Model using Splines} (\code{method = 'gam'})

For classification and regression using package \pkg{mgcv} with tuning parameters:
\itemize{
\item Feature Selection (\code{select})
\item Method (\code{method})
}


\strong{Generalized Additive Model using Splines} (\code{method = 'gamSpline'})

For classification and regression using package \pkg{gam} with tuning parameters:
\itemize{
\item Degrees of Freedom (\code{df})
}


\strong{Generalized Linear Model} (\code{method = 'glm'})

For classification and regression with no tuning parameters



\strong{Generalized Linear Model with Stepwise Feature Selection} (\code{method = 'glmStepAIC'})

For classification and regression using package \pkg{MASS} with no tuning parameters



\strong{Generalized Partial Least Squares} (\code{method = 'gpls'})

For classification using package \pkg{gpls} with tuning parameters:
\itemize{
\item Number of Components (\code{K.prov})
}


\strong{glmnet} (\code{method = 'glmnet'})

For classification and regression using package \pkg{glmnet} with tuning parameters:
\itemize{
\item Mixing Percentage (\code{alpha})
\item Regularization Parameter (\code{lambda})
}


\strong{Greedy Prototype Selection} (\code{method = 'protoclass'})

For classification using packages \pkg{proxy} and \pkg{protoclass} with tuning parameters:
\itemize{
\item Ball Size (\code{eps})
\item Distance Order (\code{Minkowski})
}


\strong{Heteroscedastic Discriminant Analysis} (\code{method = 'hda'})

For classification using package \pkg{hda} with tuning parameters:
\itemize{
\item Gamma (\code{gamma})
\item Lambda (\code{lambda})
\item Dimension of the Discriminative Subspace (\code{newdim})
}


\strong{High Dimensional Discriminant Analysis} (\code{method = 'hdda'})

For classification using package \pkg{HDclassif} with tuning parameters:
\itemize{
\item Threshold (\code{threshold})
\item Model Type (\code{model})
}


\strong{Independent Component Regression} (\code{method = 'icr'})

For regression using package \pkg{fastICA} with tuning parameters:
\itemize{
\item Number of Components (\code{n.comp})
}


\strong{k-Nearest Neighbors} (\code{method = 'kknn'})

For classification and regression using package \pkg{kknn} with tuning parameters:
\itemize{
\item Max. Number of Neighbors (\code{kmax})
\item Distance (\code{distance})
\item Kernel (\code{kernel})
}


\strong{k-Nearest Neighbors} (\code{method = 'knn'})

For classification and regression with tuning parameters:
\itemize{
\item Number of Neighbors (\code{k})
}


\strong{Learning Vector Quantization} (\code{method = 'lvq'})

For classification using package \pkg{class} with tuning parameters:
\itemize{
\item Codebook Size (\code{size})
\item Number of Prototypes (\code{k})
}


\strong{Least Angle Regression} (\code{method = 'lars'})

For regression using package \pkg{lars} with tuning parameters:
\itemize{
\item Fraction (\code{fraction})
}


\strong{Least Angle Regression} (\code{method = 'lars2'})

For regression using package \pkg{lars} with tuning parameters:
\itemize{
\item Number of Steps (\code{step})
}


\strong{Least Squares Support Vector Machine} (\code{method = 'lssvmLinear'})

For classification using package \pkg{kernlab} with no tuning parameters



\strong{Least Squares Support Vector Machine with Polynomial Kernel} (\code{method = 'lssvmPoly'})

For classification using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Polynomial Degree (\code{degree})
\item Scale (\code{scale})
}


\strong{Least Squares Support Vector Machine with Radial Basis Function Kernel} (\code{method = 'lssvmRadial'})

For classification using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Sigma (\code{sigma})
}


\strong{Linear Discriminant Analysis} (\code{method = 'lda'})

For classification using package \pkg{MASS} with no tuning parameters



\strong{Linear Discriminant Analysis} (\code{method = 'lda2'})

For classification using package \pkg{MASS} with tuning parameters:
\itemize{
\item Number of Discriminant Functions (\code{dimen})
}


\strong{Linear Discriminant Analysis with Stepwise Feature Selection} (\code{method = 'stepLDA'})

For classification using packages \pkg{klaR} and \pkg{MASS} with tuning parameters:
\itemize{
\item Maximum Number of Variables (\code{maxvar})
\item Search Direction (\code{direction})
}


\strong{Linear Regression} (\code{method = 'lm'})

For regression with no tuning parameters



\strong{Linear Regression with Backwards Selection} (\code{method = 'leapBackward'})

For regression using package \pkg{leaps} with tuning parameters:
\itemize{
\item Maximum Number of Predictors (\code{nvmax})
}


\strong{Linear Regression with Forward Selection} (\code{method = 'leapForward'})

For regression using package \pkg{leaps} with tuning parameters:
\itemize{
\item Maximum Number of Predictors (\code{nvmax})
}


\strong{Linear Regression with Stepwise Selection} (\code{method = 'leapSeq'})

For regression using package \pkg{leaps} with tuning parameters:
\itemize{
\item Maximum Number of Predictors (\code{nvmax})
}


\strong{Linear Regression with Stepwise Selection} (\code{method = 'lmStepAIC'})

For regression using package \pkg{MASS} with no tuning parameters



\strong{Logic Regression} (\code{method = 'logreg'})

For classification and regression using package \pkg{LogicReg} with tuning parameters:
\itemize{
\item Maximum Number of Leaves (\code{treesize})
\item Number of Trees (\code{ntrees})
}


\strong{Logistic Model Trees} (\code{method = 'LMT'})

For classification using package \pkg{RWeka} with tuning parameters:
\itemize{
\item Number of  Iteratons (\code{iter})
}


\strong{Maximum Uncertainty Linear Discriminant Analysis} (\code{method = 'Mlda'})

For classification using package \pkg{HiDimDA} with no tuning parameters



\strong{Mixture Discriminant Analysis} (\code{method = 'mda'})

For classification using package \pkg{mda} with tuning parameters:
\itemize{
\item Number of Subclasses Per Class (\code{subclasses})
}


\strong{Model Averaged Neural Network} (\code{method = 'avNNet'})

For classification and regression using package \pkg{nnet} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size})
\item Weight Decay (\code{decay})
\item Bagging (\code{bag})
}


\strong{Model Rules} (\code{method = 'M5Rules'})

For regression using package \pkg{RWeka} with tuning parameters:
\itemize{
\item Pruned (\code{pruned})
\item Smoothed (\code{smoothed})
}


\strong{Model Tree} (\code{method = 'M5'})

For regression using package \pkg{RWeka} with tuning parameters:
\itemize{
\item Pruned (\code{pruned})
\item Smoothed (\code{smoothed})
\item Rules (\code{rules})
}


\strong{Multi-Layer Perceptron} (\code{method = 'mlp'})

For classification and regression using package \pkg{RSNNS} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size})
}


\strong{Multi-Layer Perceptron} (\code{method = 'mlpWeightDecay'})

For classification and regression using package \pkg{RSNNS} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size})
\item Weight Decay (\code{decay})
}


\strong{Multivariate Adaptive Regression Spline} (\code{method = 'earth'})

For classification and regression using package \pkg{earth} with tuning parameters:
\itemize{
\item Number of Terms (\code{nprune})
\item Product Degree (\code{degree})
}


\strong{Multivariate Adaptive Regression Splines} (\code{method = 'gcvEarth'})

For classification and regression using package \pkg{earth} with tuning parameters:
\itemize{
\item Product Degree (\code{degree})
}


\strong{Naive Bayes} (\code{method = 'nb'})

For classification using package \pkg{klaR} with tuning parameters:
\itemize{
\item Laplace Correction (\code{fL})
\item Distribution Type (\code{usekernel})
}


\strong{Nearest Shrunken Centroids} (\code{method = 'pam'})

For classification using package \pkg{pamr} with tuning parameters:
\itemize{
\item Shrinkage Threshold (\code{threshold})
}


\strong{Neural Network} (\code{method = 'neuralnet'})

For regression using package \pkg{neuralnet} with tuning parameters:
\itemize{
\item Number of Hidden Units in Layer 1 (\code{layer1})
\item Number of Hidden Units in Layer 2 (\code{layer2})
\item Number of Hidden Units in Layer 3 (\code{layer3})
}


\strong{Neural Network} (\code{method = 'nnet'})

For classification and regression using package \pkg{nnet} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size})
\item Weight Decay (\code{decay})
}


\strong{Neural Networks with Feature Extraction} (\code{method = 'pcaNNet'})

For classification and regression using package \pkg{nnet} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size})
\item Weight Decay (\code{decay})
}


\strong{Oblique Random Forest} (\code{method = 'ORFlog'})

For classification using package \pkg{obliqueRF} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry})
}


\strong{Oblique Random Forest} (\code{method = 'ORFpls'})

For classification using package \pkg{obliqueRF} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry})
}


\strong{Oblique Random Forest} (\code{method = 'ORFridge'})

For classification using package \pkg{obliqueRF} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry})
}


\strong{Oblique Random Forest} (\code{method = 'ORFsvm'})

For classification using package \pkg{obliqueRF} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry})
}


\strong{Oblique Trees} (\code{method = 'oblique.tree'})

For classification using package \pkg{oblique.tree} with tuning parameters:
\itemize{
\item Oblique Splits (\code{oblique.splits})
\item Variable Selection Method (\code{variable.selection})
}


\strong{Parallel Random Forest} (\code{method = 'parRF'})

For classification and regression using package \pkg{randomForest} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry})
}


\strong{partDSA} (\code{method = 'partDSA'})

For classification and regression using package \pkg{partDSA} with tuning parameters:
\itemize{
\item Number of Terminal Partitions (\code{cut.off.growth})
\item Minimum Percent Difference (\code{MPD})
}


\strong{Partial Least Squares} (\code{method = 'kernelpls'})

For classification and regression using package \pkg{pls} with tuning parameters:
\itemize{
\item Number of Components (\code{ncomp})
}


\strong{Partial Least Squares} (\code{method = 'pls'})

For classification and regression using package \pkg{pls} with tuning parameters:
\itemize{
\item Number of Components (\code{ncomp})
}


\strong{Partial Least Squares} (\code{method = 'simpls'})

For classification and regression using package \pkg{pls} with tuning parameters:
\itemize{
\item Number of Components (\code{ncomp})
}


\strong{Partial Least Squares} (\code{method = 'widekernelpls'})

For classification and regression using package \pkg{pls} with tuning parameters:
\itemize{
\item Number of Components (\code{ncomp})
}


\strong{Penalized Discriminant Analysis} (\code{method = 'pda'})

For classification using package \pkg{mda} with tuning parameters:
\itemize{
\item Shrinkage Penalty Coefficient (\code{lambda})
}


\strong{Penalized Discriminant Analysis} (\code{method = 'pda2'})

For classification using package \pkg{mda} with tuning parameters:
\itemize{
\item Degrees of Freedom (\code{df})
}


\strong{Penalized Linear Discriminant Analysis} (\code{method = 'PenalizedLDA'})

For classification using packages \pkg{penalizedLDA} and \pkg{plyr} with tuning parameters:
\itemize{
\item L1 Penalty (\code{lambda})
\item Number of Discriminant Functions (\code{K})
}


\strong{Penalized Linear Regression} (\code{method = 'penalized'})

For regression using package \pkg{penalized} with tuning parameters:
\itemize{
\item L1 Penalty (\code{lambda1})
\item L2 Penalty (\code{lambda2})
}


\strong{Penalized Logistic Regression} (\code{method = 'plr'})

For classification using package \pkg{stepPlr} with tuning parameters:
\itemize{
\item L2 Penalty (\code{lambda})
\item Complexity Parameter (\code{cp})
}


\strong{Penalized Multinomial Regression} (\code{method = 'multinom'})

For classification using package \pkg{nnet} with tuning parameters:
\itemize{
\item Weight Decay (\code{decay})
}


\strong{Polynomial Kernel Regularized Least Squares} (\code{method = 'krlsPoly'})

For regression using package \pkg{KRLS} with tuning parameters:
\itemize{
\item Regularization Parameter (\code{lambda})
\item Polynomial Degree (\code{degree})
}


\strong{Principal Component Analysis} (\code{method = 'pcr'})

For regression using package \pkg{pls} with tuning parameters:
\itemize{
\item Number of Components (\code{ncomp})
}


\strong{Projection Pursuit Regression} (\code{method = 'ppr'})

For regression with tuning parameters:
\itemize{
\item Number of  Terms (\code{nterms})
}


\strong{Quadratic Discriminant Analysis} (\code{method = 'qda'})

For classification using package \pkg{MASS} with no tuning parameters



\strong{Quadratic Discriminant Analysis with Stepwise Feature Selection} (\code{method = 'stepQDA'})

For classification using packages \pkg{klaR} and \pkg{MASS} with tuning parameters:
\itemize{
\item Maximum Number of Variables (\code{maxvar})
\item Search Direction (\code{direction})
}


\strong{Quantile Random Forest} (\code{method = 'qrf'})

For regression using package \pkg{quantregForest} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry})
}


\strong{Quantile Regression Neural Network} (\code{method = 'qrnn'})

For regression using package \pkg{qrnn} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{n.hidden})
\item  Weight Decay (\code{penalty})
\item Bagged Models? (\code{bag})
}


\strong{Radial Basis Function Kernel Regularized Least Squares} (\code{method = 'krlsRadial'})

For regression using packages \pkg{KRLS} and \pkg{kernlab} with tuning parameters:
\itemize{
\item Regularization Parameter (\code{lambda})
\item Sigma (\code{sigma})
}


\strong{Radial Basis Function Network} (\code{method = 'rbf'})

For classification using package \pkg{RSNNS} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size})
}


\strong{Radial Basis Function Network} (\code{method = 'rbfDDA'})

For classification and regression using package \pkg{RSNNS} with tuning parameters:
\itemize{
\item Activation Limit for Conflicting Classes (\code{negativeThreshold})
}


\strong{Random Ferns} (\code{method = 'rFerns'})

For classification using package \pkg{rFerns} with tuning parameters:
\itemize{
\item Fern Depth (\code{depth})
}


\strong{Random Forest} (\code{method = 'rf'})

For classification and regression using package \pkg{randomForest} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry})
}


\strong{Random Forest by Randomization} (\code{method = 'extraTrees'})

For classification and regression using package \pkg{extraTrees} with tuning parameters:
\itemize{
\item Number of  Randomly Selected Predictors (\code{mtry})
\item Number of  Random Cuts (\code{numRandomCuts})
}


\strong{Random Forest with Additional Feature Selection} (\code{method = 'Boruta'})

For classification and regression using packages \pkg{Boruta} and \pkg{randomForest} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry})
}


\strong{Random k-Nearest Neighbors} (\code{method = 'rknn'})

For classification and regression using package \pkg{rknn} with tuning parameters:
\itemize{
\item Number of Neighbors (\code{k})
\item Number of Randomly Selected Predictors (\code{mtry})
}


\strong{Random k-Nearest Neighbors with Feature Selection} (\code{method = 'rknnBel'})

For classification and regression using packages \pkg{rknn} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of Neighbors (\code{k})
\item Number of Randomly Selected Predictors (\code{mtry})
\item Number of Features Dropped (\code{d})
}


\strong{Regularized Discriminant Analysis} (\code{method = 'rda'})

For classification using package \pkg{klaR} with tuning parameters:
\itemize{
\item Gamma (\code{gamma})
\item Lambda (\code{lambda})
}


\strong{Regularized Random Forest} (\code{method = 'RRF'})

For classification and regression using packages \pkg{randomForest} and \pkg{RRF} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry})
\item Regularization Value (\code{coefReg})
\item Importance Coefficient (\code{coefImp})
}


\strong{Regularized Random Forest} (\code{method = 'RRFglobal'})

For classification and regression using package \pkg{RRF} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry})
\item Regularization Value (\code{coefReg})
}


\strong{Relaxed Lasso} (\code{method = 'relaxo'})

For regression using packages \pkg{relaxo} and \pkg{plyr} with tuning parameters:
\itemize{
\item Penalty Parameter (\code{lambda})
\item Relaxation Parameter (\code{phi})
}


\strong{Relevance Vector Machines with Linear Kernel} (\code{method = 'rvmLinear'})

For regression using package \pkg{kernlab} with no tuning parameters



\strong{Relevance Vector Machines with Polynomial Kernel} (\code{method = 'rvmPoly'})

For regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Scale (\code{scale})
\item Polynomial Degree (\code{degree})
}


\strong{Relevance Vector Machines with Radial Basis Function Kernel} (\code{method = 'rvmRadial'})

For regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Sigma (\code{sigma})
}


\strong{Ridge Regression} (\code{method = 'ridge'})

For regression using package \pkg{elasticnet} with tuning parameters:
\itemize{
\item Weight Decay (\code{lambda})
}


\strong{Ridge Regression with Variable Selection} (\code{method = 'foba'})

For regression using package \pkg{foba} with tuning parameters:
\itemize{
\item Number of Variables Retained (\code{k})
\item L2 Penalty (\code{lambda})
}


\strong{Robust Linear Discriminant Analysis} (\code{method = 'Linda'})

For classification using package \pkg{rrcov} with no tuning parameters



\strong{Robust Linear Model} (\code{method = 'rlm'})

For regression using package \pkg{MASS} with no tuning parameters



\strong{Robust Quadratic Discriminant Analysis} (\code{method = 'QdaCov'})

For classification using package \pkg{rrcov} with no tuning parameters



\strong{Robust Regularized Linear Discriminant Analysis} (\code{method = 'rrlda'})

For classification using package \pkg{rrlda} with tuning parameters:
\itemize{
\item Penalty Parameter (\code{lambda})
\item Robustness Parameter (\code{hp})
\item Penalty Type (\code{penalty})
}


\strong{Robust SIMCA} (\code{method = 'RSimca'})

For classification using package \pkg{rrcovHD} with no tuning parameters



\strong{ROC-Based Classifier} (\code{method = 'rocc'})

For classification using package \pkg{rocc} with tuning parameters:
\itemize{
\item Number of Variables Retained (\code{xgenes})
}


\strong{Rule-Based Classifier} (\code{method = 'JRip'})

For classification using package \pkg{RWeka} with tuning parameters:
\itemize{
\item Number of  Optimizations (\code{NumOpt})
}


\strong{Rule-Based Classifier} (\code{method = 'PART'})

For classification using package \pkg{RWeka} with tuning parameters:
\itemize{
\item Confidence Threshold (\code{threshold})
\item Confidence Threshold (\code{pruned})
}


\strong{Self-Organizing Map} (\code{method = 'bdk'})

For classification and regression using package \pkg{kohonen} with tuning parameters:
\itemize{
\item Row (\code{xdim})
\item Columns (\code{ydim})
\item X Weight (\code{xweight})
\item Topology (\code{topo})
}


\strong{Self-Organizing Maps} (\code{method = 'xyf'})

For classification and regression using package \pkg{kohonen} with tuning parameters:
\itemize{
\item Row (\code{xdim})
\item Columns (\code{ydim})
\item X Weight (\code{xweight})
\item Topology (\code{topo})
}


\strong{Shrinkage Discriminant Analysis} (\code{method = 'sda'})

For classification using package \pkg{sda} with tuning parameters:
\itemize{
\item Diagonalize (\code{diagonal})
\item shrinkage (\code{lambda})
}


\strong{SIMCA} (\code{method = 'CSimca'})

For classification using package \pkg{rrcovHD} with no tuning parameters



\strong{Single C5.0 Ruleset} (\code{method = 'C5.0Rules'})

For classification using package \pkg{C50} with no tuning parameters



\strong{Single C5.0 Tree} (\code{method = 'C5.0Tree'})

For classification using package \pkg{C50} with no tuning parameters



\strong{Single Rule Classification} (\code{method = 'OneR'})

For classification using package \pkg{RWeka} with no tuning parameters



\strong{Sparse Linear Discriminant Analysis} (\code{method = 'sparseLDA'})

For classification using package \pkg{sparseLDA} with tuning parameters:
\itemize{
\item Number of  Predictors (\code{NumVars})
\item Lambda (\code{lambda})
}


\strong{Sparse Mixture Discriminant Analysis} (\code{method = 'smda'})

For classification using package \pkg{sparseLDA} with tuning parameters:
\itemize{
\item Number of  Predictors (\code{NumVars})
\item Lambda (\code{lambda})
\item Number of  Subclasses (\code{R})
}


\strong{Sparse Partial Least Squares} (\code{method = 'spls'})

For classification and regression using package \pkg{spls} with tuning parameters:
\itemize{
\item Number of Components (\code{K})
\item Threshold (\code{eta})
\item Kappa (\code{kappa})
}


\strong{Stabilized Linear Discriminant Analysis} (\code{method = 'slda'})

For classification using package \pkg{ipred} with no tuning parameters



\strong{Stacked AutoEncoder Deep Neural Network} (\code{method = 'dnn'})

For classification and regression using package \pkg{deepnet} with tuning parameters:
\itemize{
\item Hidden Layer 1 (\code{layer1})
\item Hidden Layer 2 (\code{layer2})
\item Hidden Layer 3 (\code{layer3})
\item Hidden Dropouts (\code{hidden_dropout})
\item Visible Dropout (\code{visible_dropout})
}


\strong{Stepwise Diagonal Linear Discriminant Analysis} (\code{method = 'sddaLDA'})

For classification using package \pkg{SDDA} with no tuning parameters



\strong{Stepwise Diagonal Quadratic Discriminant Analysis} (\code{method = 'sddaQDA'})

For classification using package \pkg{SDDA} with no tuning parameters



\strong{Stochastic Gradient Boosting} (\code{method = 'gbm'})

For classification and regression using packages \pkg{gbm} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{n.trees})
\item Max Tree Depth (\code{interaction.depth})
\item Shrinkage (\code{shrinkage})
}


\strong{Supervised Principal Component Analysis} (\code{method = 'superpc'})

For regression using package \pkg{superpc} with tuning parameters:
\itemize{
\item Threshold (\code{threshold})
\item Number of Components (\code{n.components})
}


\strong{Support Vector Machines with Class Weights} (\code{method = 'svmRadialWeights'})

For classification using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Sigma (\code{sigma})
\item Cost (\code{C})
\item Weight (\code{Weight})
}


\strong{Support Vector Machines with Linear Kernel} (\code{method = 'svmLinear'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Cost (\code{C})
}


\strong{Support Vector Machines with Polynomial Kernel} (\code{method = 'svmPoly'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Polynomial Degree (\code{degree})
\item Cost (\code{scale})
\item Scale (\code{C})
}


\strong{Support Vector Machines with Radial Basis Function Kernel} (\code{method = 'svmRadial'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Sigma (\code{sigma})
\item Cost (\code{C})
}


\strong{Support Vector Machines with Radial Basis Function Kernel} (\code{method = 'svmRadialCost'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Cost (\code{C})
}


\strong{The lasso} (\code{method = 'lasso'})

For regression using package \pkg{elasticnet} with tuning parameters:
\itemize{
\item Fraction of Full Solution (\code{fraction})
}


\strong{Tree Models from Genetic Algorithms} (\code{method = 'evtree'})

For classification and regression using package \pkg{evtree} with tuning parameters:
\itemize{
\item Complexity Parameter (\code{alpha})
}


\strong{Tree-Based Ensembles} (\code{method = 'nodeHarvest'})

For classification and regression using package \pkg{nodeHarvest} with tuning parameters:
\itemize{
\item Maximum Interaction Depth (\code{maxinter})
\item Prediction Mode (\code{mode})
}


\strong{Variational Bayesian Multinomial Probit Regression} (\code{method = 'vbmpRadial'})

For classification using package \pkg{vbmp} with tuning parameters:
\itemize{
\item Theta Estimated (\code{estimateTheta})
}


}
\references{``Using your own model in \code{\link{train}}'' (\url{http://caret.r-forge.r-project.org/custom_models.html})}
\keyword{models}
