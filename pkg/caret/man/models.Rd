\name{train_model_list}
\alias{train_model_list}
\alias{models}
\title{A List of Available Models in train}
\description{These models are included in the package via wrappers for \code{\link{train}}. Custom models can also be created. See the URL below.

\strong{AdaBoost Classification Trees} (\code{method = 'adaboost'})

For classification using package \pkg{fastAdaboost} with tuning parameters:
\itemize{
\item Number of Trees (\code{nIter}, numeric)
\item Method (\code{method}, character)
}


\strong{AdaBoost.M1} (\code{method = 'AdaBoost.M1'})

For classification using packages \pkg{adabag} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of Trees (\code{mfinal}, numeric)
\item Max Tree Depth (\code{maxdepth}, numeric)
\item Coefficient Type (\code{coeflearn}, character)
}


\strong{Adaptive Mixture Discriminant Analysis} (\code{method = 'amdai'})

For classification using package \pkg{adaptDA} with tuning parameters:
\itemize{
\item Model Type (\code{model}, character)
}


\strong{Adaptive-Network-Based Fuzzy Inference System} (\code{method = 'ANFIS'})

For regression using package \pkg{frbs} with tuning parameters:
\itemize{
\item Number of Fuzzy Terms (\code{num.labels}, numeric)
\item Max. Iterations (\code{max.iter}, numeric)
}


\strong{Adjacent Categories Probability Model for Ordinal Data} (\code{method = 'vglmAdjCat'})

For classification using package \pkg{VGAM} with tuning parameters:
\itemize{
\item Parallel Curves (\code{parallel}, logical)
\item Link Function (\code{link}, character)
}


\strong{Bagged AdaBoost} (\code{method = 'AdaBag'})

For classification using packages \pkg{adabag} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of Trees (\code{mfinal}, numeric)
\item Max Tree Depth (\code{maxdepth}, numeric)
}


\strong{Bagged CART} (\code{method = 'treebag'})

For classification and regression using packages \pkg{ipred}, \pkg{plyr} and \pkg{e1071} with no tuning parameters



\strong{Bagged FDA using gCV Pruning} (\code{method = 'bagFDAGCV'})

For classification using package \pkg{earth} with tuning parameters:
\itemize{
\item Product Degree (\code{degree}, numeric)
}


\strong{Bagged Flexible Discriminant Analysis} (\code{method = 'bagFDA'})

For classification using packages \pkg{earth} and \pkg{mda} with tuning parameters:
\itemize{
\item Product Degree (\code{degree}, numeric)
\item Number of Terms (\code{nprune}, numeric)
}


\strong{Bagged Logic Regression} (\code{method = 'logicBag'})

For classification and regression using package \pkg{logicFS} with tuning parameters:
\itemize{
\item Maximum Number of Leaves (\code{nleaves}, numeric)
\item Number of Trees (\code{ntrees}, numeric)
}


\strong{Bagged MARS} (\code{method = 'bagEarth'})

For classification and regression using package \pkg{earth} with tuning parameters:
\itemize{
\item Number of Terms (\code{nprune}, numeric)
\item Product Degree (\code{degree}, numeric)
}


\strong{Bagged MARS using gCV Pruning} (\code{method = 'bagEarthGCV'})

For classification and regression using package \pkg{earth} with tuning parameters:
\itemize{
\item Product Degree (\code{degree}, numeric)
}


\strong{Bagged Model} (\code{method = 'bag'})

For classification and regression using package \pkg{caret} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{vars}, numeric)
}


\strong{Bayesian Additive Regression Trees} (\code{method = 'bartMachine'})

For classification and regression using package \pkg{bartMachine} with tuning parameters:
\itemize{
\item Number of Trees (\code{num_trees}, numeric)
\item Prior Boundary (\code{k}, numeric)
\item Base Terminal Node Hyperparameter (\code{alpha}, numeric)
\item Power Terminal Node Hyperparameter (\code{beta}, numeric)
\item Degrees of Freedom (\code{nu}, numeric)
}


\strong{Bayesian Generalized Linear Model} (\code{method = 'bayesglm'})

For classification and regression using package \pkg{arm} with no tuning parameters



\strong{Bayesian Regularized Neural Networks} (\code{method = 'brnn'})

For regression using package \pkg{brnn} with tuning parameters:
\itemize{
\item Number of  Neurons (\code{neurons}, numeric)
}


\strong{Bayesian Ridge Regression} (\code{method = 'bridge'})

For regression using package \pkg{monomvn} with no tuning parameters



\strong{Bayesian Ridge Regression (Model Averaged)} (\code{method = 'blassoAveraged'})

For regression using package \pkg{monomvn} with no tuning parameters



\strong{Binary Discriminant Analysis} (\code{method = 'binda'})

For classification using package \pkg{binda} with tuning parameters:
\itemize{
\item Shrinkage Intensity (\code{lambda.freqs}, numeric)
}


\strong{Boosted Classification Trees} (\code{method = 'ada'})

For classification using packages \pkg{ada} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of Trees (\code{iter}, numeric)
\item Max Tree Depth (\code{maxdepth}, numeric)
\item Learning Rate (\code{nu}, numeric)
}


\strong{Boosted Generalized Additive Model} (\code{method = 'gamboost'})

For classification and regression using packages \pkg{mboost} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{mstop}, numeric)
\item AIC Prune? (\code{prune}, character)
}


\strong{Boosted Generalized Linear Model} (\code{method = 'glmboost'})

For classification and regression using packages \pkg{plyr} and \pkg{mboost} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{mstop}, numeric)
\item AIC Prune? (\code{prune}, character)
}


\strong{Boosted Linear Model} (\code{method = 'BstLm'})

For classification and regression using packages \pkg{bst} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{mstop}, numeric)
\item Shrinkage (\code{nu}, numeric)
}


\strong{Boosted Logistic Regression} (\code{method = 'LogitBoost'})

For classification using package \pkg{caTools} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{nIter}, numeric)
}


\strong{Boosted Smoothing Spline} (\code{method = 'bstSm'})

For classification and regression using packages \pkg{bst} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{mstop}, numeric)
\item Shrinkage (\code{nu}, numeric)
}


\strong{Boosted Tree} (\code{method = 'blackboost'})

For classification and regression using packages \pkg{party}, \pkg{mboost} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of Trees (\code{mstop}, numeric)
\item Max Tree Depth (\code{maxdepth}, numeric)
}


\strong{Boosted Tree} (\code{method = 'bstTree'})

For classification and regression using packages \pkg{bst} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{mstop}, numeric)
\item Max Tree Depth (\code{maxdepth}, numeric)
\item Shrinkage (\code{nu}, numeric)
}


\strong{C4.5-like Trees} (\code{method = 'J48'})

For classification using package \pkg{RWeka} with tuning parameters:
\itemize{
\item Confidence Threshold (\code{C}, numeric)
}


\strong{C5.0} (\code{method = 'C5.0'})

For classification using packages \pkg{C50} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{trials}, numeric)
\item Model Type (\code{model}, character)
\item Winnow (\code{winnow}, logical)
}


\strong{CART} (\code{method = 'rpart'})

For classification and regression using package \pkg{rpart} with tuning parameters:
\itemize{
\item Complexity Parameter (\code{cp}, numeric)
}


\strong{CART} (\code{method = 'rpart1SE'})

For classification and regression using package \pkg{rpart} with no tuning parameters



\strong{CART} (\code{method = 'rpart2'})

For classification and regression using package \pkg{rpart} with tuning parameters:
\itemize{
\item Max Tree Depth (\code{maxdepth}, numeric)
}


\strong{CART or Ordinal Responses} (\code{method = 'rpartScore'})

For classification using packages \pkg{rpartScore} and \pkg{plyr} with tuning parameters:
\itemize{
\item Complexity Parameter (\code{cp}, numeric)
\item Split Function (\code{split}, character)
\item Pruning Measure (\code{prune}, character)
}


\strong{CHi-squared Automated Interaction Detection} (\code{method = 'chaid'})

For classification using package \pkg{CHAID} with tuning parameters:
\itemize{
\item Merging Threshold (\code{alpha2}, numeric)
\item Splitting former Merged Threshold (\code{alpha3}, numeric)
\item 
                                                    Splitting former Merged Threshold (\code{alpha4}, numeric)
}


\strong{Conditional Inference Random Forest} (\code{method = 'cforest'})

For classification and regression using package \pkg{party} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
}


\strong{Conditional Inference Tree} (\code{method = 'ctree'})

For classification and regression using package \pkg{party} with tuning parameters:
\itemize{
\item 1 - P-Value Threshold (\code{mincriterion}, numeric)
}


\strong{Conditional Inference Tree} (\code{method = 'ctree2'})

For classification and regression using package \pkg{party} with tuning parameters:
\itemize{
\item Max Tree Depth (\code{maxdepth}, numeric)
\item 1 - P-Value Threshold (\code{mincriterion}, numeric)
}


\strong{Continuation Ratio Model for Ordinal Data} (\code{method = 'vglmContRatio'})

For classification using package \pkg{VGAM} with tuning parameters:
\itemize{
\item Parallel Curves (\code{parallel}, logical)
\item Link Function (\code{link}, character)
}


\strong{Cost-Sensitive C5.0} (\code{method = 'C5.0Cost'})

For classification using packages \pkg{C50} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{trials}, numeric)
\item Model Type (\code{model}, character)
\item Winnow (\code{winnow}, logical)
\item Cost (\code{cost}, numeric)
}


\strong{Cost-Sensitive CART} (\code{method = 'rpartCost'})

For classification using package \pkg{rpart} with tuning parameters:
\itemize{
\item Complexity Parameter (\code{cp}, numeric)
\item Cost (\code{Cost}, numeric)
}


\strong{Cubist} (\code{method = 'cubist'})

For regression using package \pkg{Cubist} with tuning parameters:
\itemize{
\item Number of Committees (\code{committees}, numeric)
\item Number of Instances (\code{neighbors}, numeric)
}


\strong{Cumulative Probability Model for Ordinal Data} (\code{method = 'vglmCumulative'})

For classification using package \pkg{VGAM} with tuning parameters:
\itemize{
\item Parallel Curves (\code{parallel}, logical)
\item Link Function (\code{link}, character)
}


\strong{DeepBoost} (\code{method = 'deepboost'})

For classification using package \pkg{deepboost} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{num_iter}, numeric)
\item Tree Depth (\code{tree_depth}, numeric)
\item L1 Regularization (\code{beta}, numeric)
\item Tree Depth Regularization (\code{lambda}, numeric)
\item Loss (\code{loss_type}, character)
}


\strong{Diagonal Discriminant Analysis} (\code{method = 'dda'})

For classification using package \pkg{sparsediscrim} with tuning parameters:
\itemize{
\item Model (\code{model}, character)
\item Shrinkage Type (\code{shrinkage}, character)
}


\strong{Distance Weighted Discrimination with Polynomial Kernel} (\code{method = 'dwdPoly'})

For classification using package \pkg{kerndwd} with tuning parameters:
\itemize{
\item Regularization Parameter (\code{lambda}, numeric)
\item q (\code{qval}, numeric)
\item Polynomial Degree (\code{degree}, numeric)
\item Scale (\code{scale}, numeric)
}


\strong{Distance Weighted Discrimination with Radial Basis Function Kernel} (\code{method = 'dwdRadial'})

For classification using packages \pkg{kernlab} and \pkg{kerndwd} with tuning parameters:
\itemize{
\item Regularization Parameter (\code{lambda}, numeric)
\item q (\code{qval}, numeric)
\item Sigma (\code{sigma}, numeric)
}


\strong{Dynamic Evolving Neural-Fuzzy Inference System } (\code{method = 'DENFIS'})

For regression using package \pkg{frbs} with tuning parameters:
\itemize{
\item Threshold (\code{Dthr}, numeric)
\item Max. Iterations (\code{max.iter}, numeric)
}


\strong{Elasticnet} (\code{method = 'enet'})

For regression using package \pkg{elasticnet} with tuning parameters:
\itemize{
\item Fraction of Full Solution (\code{fraction}, numeric)
\item Weight Decay (\code{lambda}, numeric)
}


\strong{Ensemble Partial Least Squares Regression} (\code{method = 'enpls'})

For regression using package \pkg{enpls} with tuning parameters:
\itemize{
\item Max. Number of Components (\code{maxcomp}, numeric)
}


\strong{Ensemble Partial Least Squares Regression with Feature Selection} (\code{method = 'enpls.fs'})

For regression using package \pkg{enpls} with tuning parameters:
\itemize{
\item Max. Number of Components (\code{maxcomp}, numeric)
\item Importance Cutoff (\code{threshold}, numeric)
}


\strong{Ensembles of Generalized Lienar Models} (\code{method = 'randomGLM'})

For classification and regression using package \pkg{randomGLM} with tuning parameters:
\itemize{
\item Interaction Order (\code{maxInteractionOrder}, numeric)
}


\strong{eXtreme Gradient Boosting} (\code{method = 'xgbLinear'})

For classification and regression using package \pkg{xgboost} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{nrounds}, numeric)
\item L2 Regularization (\code{lambda}, numeric)
\item L1 Regularization (\code{alpha}, numeric)
\item Learning Rate (\code{eta}, numeric)
}


\strong{eXtreme Gradient Boosting} (\code{method = 'xgbTree'})

For classification and regression using packages \pkg{xgboost} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{nrounds}, numeric)
\item Max Tree Depth (\code{max_depth}, numeric)
\item Shrinkage (\code{eta}, numeric)
\item Minimum Loss Reduction (\code{gamma}, numeric)
\item Subsample Ratio of Columns (\code{colsample_bytree}, numeric)
\item Minimum Sum of Instance Weight (\code{min_child_weight}, numeric)
}


\strong{Extreme Learning Machine} (\code{method = 'elm'})

For classification and regression using package \pkg{elmNN} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{nhid}, numeric)
\item Activation Function (\code{actfun}, character)
}


\strong{Factor-Based Linear Discriminant Analysis} (\code{method = 'RFlda'})

For classification using package \pkg{HiDimDA} with tuning parameters:
\itemize{
\item Number of  Factors (\code{q}, numeric)
}


\strong{Flexible Discriminant Analysis} (\code{method = 'fda'})

For classification using packages \pkg{earth} and \pkg{mda} with tuning parameters:
\itemize{
\item Product Degree (\code{degree}, numeric)
\item Number of Terms (\code{nprune}, numeric)
}


\strong{Fuzzy Inference Rules by Descent Method} (\code{method = 'FIR.DM'})

For regression using package \pkg{frbs} with tuning parameters:
\itemize{
\item Number of Fuzzy Terms (\code{num.labels}, numeric)
\item Max. Iterations (\code{max.iter}, numeric)
}


\strong{Fuzzy Rules Using Chi's Method} (\code{method = 'FRBCS.CHI'})

For classification using package \pkg{frbs} with tuning parameters:
\itemize{
\item Number of Fuzzy Terms (\code{num.labels}, numeric)
\item Membership Function (\code{type.mf}, character)
}


\strong{Fuzzy Rules Using Genetic Cooperative-Competitive Learning} (\code{method = 'GFS.GCCL'})

For classification using package \pkg{frbs} with tuning parameters:
\itemize{
\item Number of Fuzzy Terms (\code{num.labels}, numeric)
\item Population Size (\code{popu.size}, numeric)
\item Max. Generations (\code{max.gen}, numeric)
}


\strong{Fuzzy Rules Using Genetic Cooperative-Competitive Learning and Pittsburgh} (\code{method = 'FH.GBML'})

For classification using package \pkg{frbs} with tuning parameters:
\itemize{
\item Max. Number of Rules (\code{max.num.rule}, numeric)
\item Population Size (\code{popu.size}, numeric)
\item Max. Generations (\code{max.gen}, numeric)
}


\strong{Fuzzy Rules Using the Structural Learning Algorithm on Vague Environment} (\code{method = 'SLAVE'})

For classification using package \pkg{frbs} with tuning parameters:
\itemize{
\item Number of Fuzzy Terms (\code{num.labels}, numeric)
\item Max. Iterations (\code{max.iter}, numeric)
\item Max. Generations (\code{max.gen}, numeric)
}


\strong{Fuzzy Rules via MOGUL} (\code{method = 'GFS.FR.MOGUL'})

For regression using package \pkg{frbs} with tuning parameters:
\itemize{
\item Max. Generations (\code{max.gen}, numeric)
\item Max. Iterations (\code{max.iter}, numeric)
\item Max. Tuning Iterations (\code{max.tune}, numeric)
}


\strong{Fuzzy Rules via Thrift} (\code{method = 'GFS.THRIFT'})

For regression using package \pkg{frbs} with tuning parameters:
\itemize{
\item Population Size (\code{popu.size}, numeric)
\item Number of  Fuzzy Labels (\code{num.labels}, numeric)
\item Max. Generations (\code{max.gen}, numeric)
}


\strong{Fuzzy Rules with Weight Factor} (\code{method = 'FRBCS.W'})

For classification using package \pkg{frbs} with tuning parameters:
\itemize{
\item Number of Fuzzy Terms (\code{num.labels}, numeric)
\item Membership Function (\code{type.mf}, character)
}


\strong{Gaussian Process} (\code{method = 'gaussprLinear'})

For classification and regression using package \pkg{kernlab} with no tuning parameters



\strong{Gaussian Process with Polynomial Kernel} (\code{method = 'gaussprPoly'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Polynomial Degree (\code{degree}, numeric)
\item Scale (\code{scale}, numeric)
}


\strong{Gaussian Process with Radial Basis Function Kernel} (\code{method = 'gaussprRadial'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Sigma (\code{sigma}, numeric)
}


\strong{Generalized Additive Model using LOESS} (\code{method = 'gamLoess'})

For classification and regression using package \pkg{gam} with tuning parameters:
\itemize{
\item Span (\code{span}, numeric)
\item Degree (\code{degree}, numeric)
}


\strong{Generalized Additive Model using Splines} (\code{method = 'bam'})

For classification and regression using package \pkg{mgcv} with tuning parameters:
\itemize{
\item Feature Selection (\code{select}, logical)
\item Method (\code{method}, character)
}


\strong{Generalized Additive Model using Splines} (\code{method = 'gam'})

For classification and regression using package \pkg{mgcv} with tuning parameters:
\itemize{
\item Feature Selection (\code{select}, logical)
\item Method (\code{method}, character)
}


\strong{Generalized Additive Model using Splines} (\code{method = 'gamSpline'})

For classification and regression using package \pkg{gam} with tuning parameters:
\itemize{
\item Degrees of Freedom (\code{df}, numeric)
}


\strong{Generalized Linear Model} (\code{method = 'glm'})

For classification and regression with no tuning parameters



\strong{Generalized Linear Model with Stepwise Feature Selection} (\code{method = 'glmStepAIC'})

For classification and regression using package \pkg{MASS} with no tuning parameters



\strong{Generalized Partial Least Squares} (\code{method = 'gpls'})

For classification using package \pkg{gpls} with tuning parameters:
\itemize{
\item Number of Components (\code{K.prov}, numeric)
}


\strong{Genetic Lateral Tuning and Rule Selection of Linguistic Fuzzy Systems} (\code{method = 'GFS.LT.RS'})

For regression using package \pkg{frbs} with tuning parameters:
\itemize{
\item Population Size (\code{popu.size}, numeric)
\item Number of  Fuzzy Labels (\code{num.labels}, numeric)
\item Max. Generations (\code{max.gen}, numeric)
}


\strong{glmnet} (\code{method = 'glmnet'})

For classification and regression using package \pkg{glmnet} with tuning parameters:
\itemize{
\item Mixing Percentage (\code{alpha}, numeric)
\item Regularization Parameter (\code{lambda}, numeric)
}


\strong{Greedy Prototype Selection} (\code{method = 'protoclass'})

For classification using packages \pkg{proxy} and \pkg{protoclass} with tuning parameters:
\itemize{
\item Ball Size (\code{eps}, numeric)
\item Distance Order (\code{Minkowski}, numeric)
}


\strong{Heteroscedastic Discriminant Analysis} (\code{method = 'hda'})

For classification using package \pkg{hda} with tuning parameters:
\itemize{
\item Gamma (\code{gamma}, numeric)
\item Lambda (\code{lambda}, numeric)
\item Dimension of the Discriminative Subspace (\code{newdim}, numeric)
}


\strong{High Dimensional Discriminant Analysis} (\code{method = 'hdda'})

For classification using package \pkg{HDclassif} with tuning parameters:
\itemize{
\item Threshold (\code{threshold}, character)
\item Model Type (\code{model}, numeric)
}


\strong{High-Dimensional Regularized Discriminant Analysis} (\code{method = 'hdrda'})

For classification using package \pkg{sparsediscrim} with tuning parameters:
\itemize{
\item Gamma (\code{gamma}, numeric)
\item Lambda (\code{lambda}, numeric)
\item Shrinkage Type (\code{shrinkage_type}, character)
}


\strong{Hybrid Neural Fuzzy Inference System} (\code{method = 'HYFIS'})

For regression using package \pkg{frbs} with tuning parameters:
\itemize{
\item Number of Fuzzy Terms (\code{num.labels}, numeric)
\item Max. Iterations (\code{max.iter}, numeric)
}


\strong{Independent Component Regression} (\code{method = 'icr'})

For regression using package \pkg{fastICA} with tuning parameters:
\itemize{
\item Number of Components (\code{n.comp}, numeric)
}


\strong{k-Nearest Neighbors} (\code{method = 'kknn'})

For classification and regression using package \pkg{kknn} with tuning parameters:
\itemize{
\item Max. Number of Neighbors (\code{kmax}, numeric)
\item Distance (\code{distance}, numeric)
\item Kernel (\code{kernel}, character)
}


\strong{k-Nearest Neighbors} (\code{method = 'knn'})

For classification and regression with tuning parameters:
\itemize{
\item Number of Neighbors (\code{k}, numeric)
}


\strong{Knn regression via sklearn.neighbors.KNeighborsRegressor} (\code{method = 'pythonKnnReg'})

For regression using package \pkg{rPython} with tuning parameters:
\itemize{
\item Number of Neighbors (\code{n_neighbors}, numeric)
\item Weight Function (\code{weights}, character)
\item Algorithm (\code{algorithm}, character)
\item Leaf Size (\code{leaf_size}, numeric)
\item Distance Metric (\code{metric}, character)
\item p (\code{p}, numeric)
}


\strong{L2 Regularized Linear Support Vector Machines with Class Weights} (\code{method = 'svmLinearWeights2'})

For classification using package \pkg{LiblineaR} with tuning parameters:
\itemize{
\item Cost (\code{cost}, numeric)
\item Loss Function (\code{Loss}, character)
\item Class Weight (\code{weight}, numeric)
}


\strong{L2 Regularized Support Vector Machine (dual) with Linear Kernel} (\code{method = 'svmLinear3'})

For classification and regression using package \pkg{LiblineaR} with tuning parameters:
\itemize{
\item Cost (\code{cost}, numeric)
\item Loss Function (\code{Loss}, character)
}


\strong{Learning Vector Quantization} (\code{method = 'lvq'})

For classification using package \pkg{class} with tuning parameters:
\itemize{
\item Codebook Size (\code{size}, numeric)
\item Number of Prototypes (\code{k}, numeric)
}


\strong{Least Angle Regression} (\code{method = 'lars'})

For regression using package \pkg{lars} with tuning parameters:
\itemize{
\item Fraction (\code{fraction}, numeric)
}


\strong{Least Angle Regression} (\code{method = 'lars2'})

For regression using package \pkg{lars} with tuning parameters:
\itemize{
\item Number of Steps (\code{step}, numeric)
}


\strong{Least Squares Support Vector Machine} (\code{method = 'lssvmLinear'})

For classification using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Regularization Parameter (\code{tau}, numeric)
}


\strong{Least Squares Support Vector Machine with Polynomial Kernel} (\code{method = 'lssvmPoly'})

For classification using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Polynomial Degree (\code{degree}, numeric)
\item Scale (\code{scale}, numeric)
\item Regularization Parameter (\code{tau}, numeric)
}


\strong{Least Squares Support Vector Machine with Radial Basis Function Kernel} (\code{method = 'lssvmRadial'})

For classification using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Sigma (\code{sigma}, numeric)
\item Regularization Parameter (\code{tau}, numeric)
}


\strong{Linear Discriminant Analysis} (\code{method = 'lda'})

For classification using package \pkg{MASS} with no tuning parameters



\strong{Linear Discriminant Analysis} (\code{method = 'lda2'})

For classification using package \pkg{MASS} with tuning parameters:
\itemize{
\item Number of Discriminant Functions (\code{dimen}, numeric)
}


\strong{Linear Discriminant Analysis with Stepwise Feature Selection} (\code{method = 'stepLDA'})

For classification using packages \pkg{klaR} and \pkg{MASS} with tuning parameters:
\itemize{
\item Maximum Number of Variables (\code{maxvar}, numeric)
\item Search Direction (\code{direction}, character)
}


\strong{Linear Distance Weighted Discrimination} (\code{method = 'dwdLinear'})

For classification using package \pkg{kerndwd} with tuning parameters:
\itemize{
\item Regularization Parameter (\code{lambda}, numeric)
\item q (\code{qval}, numeric)
}


\strong{Linear Regression} (\code{method = 'lm'})

For regression with tuning parameters:
\itemize{
\item intercept (\code{intercept}, logical)
}


\strong{Linear Regression with Backwards Selection} (\code{method = 'leapBackward'})

For regression using package \pkg{leaps} with tuning parameters:
\itemize{
\item Maximum Number of Predictors (\code{nvmax}, numeric)
}


\strong{Linear Regression with Forward Selection} (\code{method = 'leapForward'})

For regression using package \pkg{leaps} with tuning parameters:
\itemize{
\item Maximum Number of Predictors (\code{nvmax}, numeric)
}


\strong{Linear Regression with Stepwise Selection} (\code{method = 'leapSeq'})

For regression using package \pkg{leaps} with tuning parameters:
\itemize{
\item Maximum Number of Predictors (\code{nvmax}, numeric)
}


\strong{Linear Regression with Stepwise Selection} (\code{method = 'lmStepAIC'})

For regression using package \pkg{MASS} with no tuning parameters



\strong{Linear Support Vector Machines with Class Weights} (\code{method = 'svmLinearWeights'})

For classification using package \pkg{e1071} with tuning parameters:
\itemize{
\item Cost (\code{cost}, numeric)
\item Class Weight (\code{weight}, numeric)
}


\strong{Localized Linear Discriminant Analysis} (\code{method = 'loclda'})

For classification using package \pkg{klaR} with tuning parameters:
\itemize{
\item Number of Nearest Neighbors (\code{k}, numeric)
}


\strong{Logic Regression} (\code{method = 'logreg'})

For classification and regression using package \pkg{LogicReg} with tuning parameters:
\itemize{
\item Maximum Number of Leaves (\code{treesize}, numeric)
\item Number of Trees (\code{ntrees}, numeric)
}


\strong{Logistic Model Trees} (\code{method = 'LMT'})

For classification using package \pkg{RWeka} with tuning parameters:
\itemize{
\item Number of  Iteratons (\code{iter}, numeric)
}


\strong{Maximum Uncertainty Linear Discriminant Analysis} (\code{method = 'Mlda'})

For classification using package \pkg{HiDimDA} with no tuning parameters



\strong{Mixture Discriminant Analysis} (\code{method = 'mda'})

For classification using package \pkg{mda} with tuning parameters:
\itemize{
\item Number of Subclasses Per Class (\code{subclasses}, numeric)
}


\strong{Model Averaged Naive Bayes Classifier} (\code{method = 'manb'})

For classification using package \pkg{bnclassify} with tuning parameters:
\itemize{
\item Smoothing Parameter (\code{smooth}, numeric)
\item Prior Probability (\code{prior}, numeric)
}


\strong{Model Averaged Neural Network} (\code{method = 'avNNet'})

For classification and regression using package \pkg{nnet} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size}, numeric)
\item Weight Decay (\code{decay}, numeric)
\item Bagging (\code{bag}, logical)
}


\strong{Model Rules} (\code{method = 'M5Rules'})

For regression using package \pkg{RWeka} with tuning parameters:
\itemize{
\item Pruned (\code{pruned}, character)
\item Smoothed (\code{smoothed}, character)
}


\strong{Model Tree} (\code{method = 'M5'})

For regression using package \pkg{RWeka} with tuning parameters:
\itemize{
\item Pruned (\code{pruned}, character)
\item Smoothed (\code{smoothed}, character)
\item Rules (\code{rules}, character)
}


\strong{Multi-Layer Perceptron} (\code{method = 'mlp'})

For classification and regression using package \pkg{RSNNS} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size}, numeric)
}


\strong{Multi-Layer Perceptron} (\code{method = 'mlpWeightDecay'})

For classification and regression using package \pkg{RSNNS} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size}, numeric)
\item Weight Decay (\code{decay}, numeric)
}


\strong{Multi-Layer Perceptron, multiple layers} (\code{method = 'mlpWeightDecayML'})

For classification and regression using package \pkg{RSNNS} with tuning parameters:
\itemize{
\item Number of Hidden Units layer1 (\code{layer1}, numeric)
\item Number of Hidden Units layer2 (\code{layer2}, numeric)
\item Number of Hidden Units layer3 (\code{layer3}, numeric)
\item Weight Decay (\code{decay}, numeric)
}


\strong{Multi-Layer Perceptron, with multiple layers} (\code{method = 'mlpML'})

For classification and regression using package \pkg{RSNNS} with tuning parameters:
\itemize{
\item Number of Hidden Units layer1 (\code{layer1}, numeric)
\item Number of Hidden Units layer2 (\code{layer2}, numeric)
\item Number of Hidden Units layer3 (\code{layer3}, numeric)
}


\strong{Multilayer Perceptron Network by Stochastic Gradient Descent} (\code{method = 'mlpSGD'})

For regression using package \pkg{FCNN4R} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size}, numeric)
\item L2 Regularization (\code{l2reg}, numeric)
\item RMSE Gradient Scaling (\code{lambda}, numeric)
\item Learning Rate (\code{learn_rate}, numeric)
\item Momentum (\code{momentum}, numeric)
\item Decay (\code{gamma}, numeric)
\item Batch Size (\code{minibatchsz}, numeric)
\item Number of Models (\code{repeats}, numeric)
}


\strong{Multivariate Adaptive Regression Spline} (\code{method = 'earth'})

For classification and regression using package \pkg{earth} with tuning parameters:
\itemize{
\item Number of Terms (\code{nprune}, numeric)
\item Product Degree (\code{degree}, numeric)
}


\strong{Multivariate Adaptive Regression Splines} (\code{method = 'gcvEarth'})

For classification and regression using package \pkg{earth} with tuning parameters:
\itemize{
\item Product Degree (\code{degree}, numeric)
}


\strong{Naive Bayes} (\code{method = 'nb'})

For classification using package \pkg{klaR} with tuning parameters:
\itemize{
\item Laplace Correction (\code{fL}, numeric)
\item Distribution Type (\code{usekernel}, logical)
\item Bandwidth Adjustment (\code{adjust}, numeric)
}


\strong{Naive Bayes Classifier} (\code{method = 'nbDiscrete'})

For classification using package \pkg{bnclassify} with tuning parameters:
\itemize{
\item Smoothing Parameter (\code{smooth}, numeric)
}


\strong{Naive Bayes Classifier with Attribute Weighting} (\code{method = 'awnb'})

For classification using package \pkg{bnclassify} with tuning parameters:
\itemize{
\item Smoothing Parameter (\code{smooth}, numeric)
}


\strong{Nearest Shrunken Centroids} (\code{method = 'pam'})

For classification using package \pkg{pamr} with tuning parameters:
\itemize{
\item Shrinkage Threshold (\code{threshold}, numeric)
}


\strong{Neural Network} (\code{method = 'neuralnet'})

For regression using package \pkg{neuralnet} with tuning parameters:
\itemize{
\item Number of Hidden Units in Layer 1 (\code{layer1}, numeric)
\item Number of Hidden Units in Layer 2 (\code{layer2}, numeric)
\item Number of Hidden Units in Layer 3 (\code{layer3}, numeric)
}


\strong{Neural Network} (\code{method = 'nnet'})

For classification and regression using package \pkg{nnet} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size}, numeric)
\item Weight Decay (\code{decay}, numeric)
}


\strong{Neural Networks with Feature Extraction} (\code{method = 'pcaNNet'})

For classification and regression using package \pkg{nnet} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size}, numeric)
\item Weight Decay (\code{decay}, numeric)
}


\strong{Non-Convex Penalized Quantile Regression} (\code{method = 'rqnc'})

For regression using package \pkg{rqPen} with tuning parameters:
\itemize{
\item L1 Penalty (\code{lambda}, numeric)
\item Penalty Type (\code{penalty}, character)
}


\strong{Non-Negative Least Squares} (\code{method = 'nnls'})

For regression using package \pkg{nnls} with no tuning parameters



\strong{Oblique Random Forest} (\code{method = 'ORFlog'})

For classification using package \pkg{obliqueRF} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
}


\strong{Oblique Random Forest} (\code{method = 'ORFpls'})

For classification using package \pkg{obliqueRF} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
}


\strong{Oblique Random Forest} (\code{method = 'ORFridge'})

For classification using package \pkg{obliqueRF} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
}


\strong{Oblique Random Forest} (\code{method = 'ORFsvm'})

For classification using package \pkg{obliqueRF} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
}


\strong{Oblique Trees} (\code{method = 'oblique.tree'})

For classification using package \pkg{oblique.tree} with tuning parameters:
\itemize{
\item Oblique Splits (\code{oblique.splits}, character)
\item Variable Selection Method (\code{variable.selection}, character)
}


\strong{Optimal Weighted Nearest Neighbor Classifier} (\code{method = 'ownn'})

For classification using package \pkg{snn} with tuning parameters:
\itemize{
\item Number of Neighbors (\code{K}, numeric)
}


\strong{Ordered Logistic or Probit Regression} (\code{method = 'polr'})

For classification using package \pkg{MASS} with tuning parameters:
\itemize{
\item parameter (\code{method}, character)
}


\strong{Parallel Random Forest} (\code{method = 'parRF'})

For classification and regression using packages \pkg{e1071}, \pkg{randomForest} and \pkg{foreach} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
}


\strong{partDSA} (\code{method = 'partDSA'})

For classification and regression using package \pkg{partDSA} with tuning parameters:
\itemize{
\item Number of Terminal Partitions (\code{cut.off.growth}, numeric)
\item Minimum Percent Difference (\code{MPD}, numeric)
}


\strong{Partial Least Squares} (\code{method = 'kernelpls'})

For classification and regression using package \pkg{pls} with tuning parameters:
\itemize{
\item Number of Components (\code{ncomp}, numeric)
}


\strong{Partial Least Squares} (\code{method = 'pls'})

For classification and regression using package \pkg{pls} with tuning parameters:
\itemize{
\item Number of Components (\code{ncomp}, numeric)
}


\strong{Partial Least Squares} (\code{method = 'simpls'})

For classification and regression using package \pkg{pls} with tuning parameters:
\itemize{
\item Number of Components (\code{ncomp}, numeric)
}


\strong{Partial Least Squares} (\code{method = 'widekernelpls'})

For classification and regression using package \pkg{pls} with tuning parameters:
\itemize{
\item Number of Components (\code{ncomp}, numeric)
}


\strong{Partial Least Squares Generalized Linear Models } (\code{method = 'plsRglm'})

For classification and regression using package \pkg{plsRglm} with tuning parameters:
\itemize{
\item Number of PLS Components (\code{nt}, numeric)
\item p-Value threshold (\code{alpha.pvals.expli}, numeric)
}


\strong{Penalized Discriminant Analysis} (\code{method = 'pda'})

For classification using package \pkg{mda} with tuning parameters:
\itemize{
\item Shrinkage Penalty Coefficient (\code{lambda}, numeric)
}


\strong{Penalized Discriminant Analysis} (\code{method = 'pda2'})

For classification using package \pkg{mda} with tuning parameters:
\itemize{
\item Degrees of Freedom (\code{df}, numeric)
}


\strong{Penalized Linear Discriminant Analysis} (\code{method = 'PenalizedLDA'})

For classification using packages \pkg{penalizedLDA} and \pkg{plyr} with tuning parameters:
\itemize{
\item L1 Penalty (\code{lambda}, numeric)
\item Number of Discriminant Functions (\code{K}, numeric)
}


\strong{Penalized Linear Regression} (\code{method = 'penalized'})

For regression using package \pkg{penalized} with tuning parameters:
\itemize{
\item L1 Penalty (\code{lambda1}, numeric)
\item L2 Penalty (\code{lambda2}, numeric)
}


\strong{Penalized Logistic Regression} (\code{method = 'plr'})

For classification using package \pkg{stepPlr} with tuning parameters:
\itemize{
\item L2 Penalty (\code{lambda}, numeric)
\item Complexity Parameter (\code{cp}, character)
}


\strong{Penalized Multinomial Regression} (\code{method = 'multinom'})

For classification using package \pkg{nnet} with tuning parameters:
\itemize{
\item Weight Decay (\code{decay}, numeric)
}


\strong{Penalized Ordinal Regression} (\code{method = 'ordinalNet'})

For classification and regression using packages \pkg{ordinalNet} and \pkg{plyr} with tuning parameters:
\itemize{
\item Mixing Percentage (\code{alpha}, numeric)
\item Selection Criterion (\code{criteria}, character)
\item Link Function (\code{link}, character)
}


\strong{Polynomial Kernel Regularized Least Squares} (\code{method = 'krlsPoly'})

For regression using package \pkg{KRLS} with tuning parameters:
\itemize{
\item Regularization Parameter (\code{lambda}, numeric)
\item Polynomial Degree (\code{degree}, numeric)
}


\strong{Principal Component Analysis} (\code{method = 'pcr'})

For regression using package \pkg{pls} with tuning parameters:
\itemize{
\item Number of Components (\code{ncomp}, numeric)
}


\strong{Projection Pursuit Regression} (\code{method = 'ppr'})

For regression with tuning parameters:
\itemize{
\item Number of  Terms (\code{nterms}, numeric)
}


\strong{Quadratic Discriminant Analysis} (\code{method = 'qda'})

For classification using package \pkg{MASS} with no tuning parameters



\strong{Quadratic Discriminant Analysis with Stepwise Feature Selection} (\code{method = 'stepQDA'})

For classification using packages \pkg{klaR} and \pkg{MASS} with tuning parameters:
\itemize{
\item Maximum Number of Variables (\code{maxvar}, numeric)
\item Search Direction (\code{direction}, character)
}


\strong{Quantile Random Forest} (\code{method = 'qrf'})

For regression using package \pkg{quantregForest} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
}


\strong{Quantile Regression Neural Network} (\code{method = 'qrnn'})

For regression using package \pkg{qrnn} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{n.hidden}, numeric)
\item  Weight Decay (\code{penalty}, numeric)
\item Bagged Models? (\code{bag}, logical)
}


\strong{Quantile Regression with LASSO penalty} (\code{method = 'rqlasso'})

For regression using package \pkg{rqPen} with tuning parameters:
\itemize{
\item L1 Penalty (\code{lambda}, numeric)
}


\strong{Radial Basis Function Kernel Regularized Least Squares} (\code{method = 'krlsRadial'})

For regression using packages \pkg{KRLS} and \pkg{kernlab} with tuning parameters:
\itemize{
\item Regularization Parameter (\code{lambda}, numeric)
\item Sigma (\code{sigma}, numeric)
}


\strong{Radial Basis Function Network} (\code{method = 'rbf'})

For classification and regression using package \pkg{RSNNS} with tuning parameters:
\itemize{
\item Number of Hidden Units (\code{size}, numeric)
}


\strong{Radial Basis Function Network} (\code{method = 'rbfDDA'})

For classification and regression using package \pkg{RSNNS} with tuning parameters:
\itemize{
\item Activation Limit for Conflicting Classes (\code{negativeThreshold}, numeric)
}


\strong{Random Ferns} (\code{method = 'rFerns'})

For classification using package \pkg{rFerns} with tuning parameters:
\itemize{
\item Fern Depth (\code{depth}, numeric)
}


\strong{Random Forest} (\code{method = 'ranger'})

For classification and regression using packages \pkg{e1071} and \pkg{ranger} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
}


\strong{Random Forest} (\code{method = 'Rborist'})

For classification and regression using package \pkg{Rborist} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{predFixed}, numeric)
}


\strong{Random Forest} (\code{method = 'rf'})

For classification and regression using package \pkg{randomForest} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
}


\strong{Random Forest by Randomization} (\code{method = 'extraTrees'})

For classification and regression using package \pkg{extraTrees} with tuning parameters:
\itemize{
\item Number of  Randomly Selected Predictors (\code{mtry}, numeric)
\item Number of  Random Cuts (\code{numRandomCuts}, numeric)
}


\strong{Random Forest Rule-Based Model} (\code{method = 'rfRules'})

For classification and regression using packages \pkg{randomForest}, \pkg{inTrees} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
\item Maximum Rule Depth (\code{maxdepth}, numeric)
}


\strong{Random Forest with Additional Feature Selection} (\code{method = 'Boruta'})

For classification and regression using packages \pkg{Boruta} and \pkg{randomForest} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
}


\strong{Regularized Discriminant Analysis} (\code{method = 'rda'})

For classification using package \pkg{klaR} with tuning parameters:
\itemize{
\item Gamma (\code{gamma}, numeric)
\item Lambda (\code{lambda}, numeric)
}


\strong{Regularized Linear Discriminant Analysis} (\code{method = 'rlda'})

For classification using package \pkg{sparsediscrim} with tuning parameters:
\itemize{
\item Regularization Method (\code{estimator}, character)
}


\strong{Regularized Random Forest} (\code{method = 'RRF'})

For classification and regression using packages \pkg{randomForest} and \pkg{RRF} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
\item Regularization Value (\code{coefReg}, numeric)
\item Importance Coefficient (\code{coefImp}, numeric)
}


\strong{Regularized Random Forest} (\code{method = 'RRFglobal'})

For classification and regression using package \pkg{RRF} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
\item Regularization Value (\code{coefReg}, numeric)
}


\strong{Relaxed Lasso} (\code{method = 'relaxo'})

For regression using packages \pkg{relaxo} and \pkg{plyr} with tuning parameters:
\itemize{
\item Penalty Parameter (\code{lambda}, numeric)
\item Relaxation Parameter (\code{phi}, numeric)
}


\strong{Relevance Vector Machines with Linear Kernel} (\code{method = 'rvmLinear'})

For regression using package \pkg{kernlab} with no tuning parameters



\strong{Relevance Vector Machines with Polynomial Kernel} (\code{method = 'rvmPoly'})

For regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Scale (\code{scale}, numeric)
\item Polynomial Degree (\code{degree}, numeric)
}


\strong{Relevance Vector Machines with Radial Basis Function Kernel} (\code{method = 'rvmRadial'})

For regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Sigma (\code{sigma}, numeric)
}


\strong{Ridge Regression} (\code{method = 'ridge'})

For regression using package \pkg{elasticnet} with tuning parameters:
\itemize{
\item Weight Decay (\code{lambda}, numeric)
}


\strong{Ridge Regression with Variable Selection} (\code{method = 'foba'})

For regression using package \pkg{foba} with tuning parameters:
\itemize{
\item Number of Variables Retained (\code{k}, numeric)
\item L2 Penalty (\code{lambda}, numeric)
}


\strong{Robust Linear Discriminant Analysis} (\code{method = 'Linda'})

For classification using package \pkg{rrcov} with no tuning parameters



\strong{Robust Linear Model} (\code{method = 'rlm'})

For regression using package \pkg{MASS} with tuning parameters:
\itemize{
\item intercept (\code{intercept}, logical)
\item psi (\code{psi}, character)
}


\strong{Robust Mixture Discriminant Analysis} (\code{method = 'rmda'})

For classification using package \pkg{robustDA} with tuning parameters:
\itemize{
\item Number of Subclasses Per Class (\code{K}, numeric)
\item Model (\code{model}, character)
}


\strong{Robust Quadratic Discriminant Analysis} (\code{method = 'QdaCov'})

For classification using package \pkg{rrcov} with no tuning parameters



\strong{Robust Regularized Linear Discriminant Analysis} (\code{method = 'rrlda'})

For classification using package \pkg{rrlda} with tuning parameters:
\itemize{
\item Penalty Parameter (\code{lambda}, numeric)
\item Robustness Parameter (\code{hp}, numeric)
\item Penalty Type (\code{penalty}, character)
}


\strong{Robust SIMCA} (\code{method = 'RSimca'})

For classification using package \pkg{rrcovHD} with no tuning parameters



\strong{ROC-Based Classifier} (\code{method = 'rocc'})

For classification using package \pkg{rocc} with tuning parameters:
\itemize{
\item Number of Variables Retained (\code{xgenes}, numeric)
}


\strong{Rotation Forest} (\code{method = 'rotationForest'})

For classification using package \pkg{rotationForest} with tuning parameters:
\itemize{
\item Number of Variable Subsets (\code{K}, numeric)
\item Ensemble Size (\code{L}, numeric)
}


\strong{Rotation Forest} (\code{method = 'rotationForestCp'})

For classification using packages \pkg{rpart}, \pkg{plyr} and \pkg{rotationForest} with tuning parameters:
\itemize{
\item Number of Variable Subsets (\code{K}, numeric)
\item Ensemble Size (\code{L}, numeric)
\item Complexity Parameter (\code{cp}, numeric)
}


\strong{Rule-Based Classifier} (\code{method = 'JRip'})

For classification using package \pkg{RWeka} with tuning parameters:
\itemize{
\item Number of  Optimizations (\code{NumOpt}, numeric)
}


\strong{Rule-Based Classifier} (\code{method = 'PART'})

For classification using package \pkg{RWeka} with tuning parameters:
\itemize{
\item Confidence Threshold (\code{threshold}, numeric)
\item Confidence Threshold (\code{pruned}, character)
}


\strong{Self-Organizing Map} (\code{method = 'bdk'})

For classification and regression using package \pkg{kohonen} with tuning parameters:
\itemize{
\item Row (\code{xdim}, numeric)
\item Columns (\code{ydim}, numeric)
\item X Weight (\code{xweight}, numeric)
\item Topology (\code{topo}, character)
}


\strong{Self-Organizing Maps} (\code{method = 'xyf'})

For classification and regression using package \pkg{kohonen} with tuning parameters:
\itemize{
\item Row (\code{xdim}, numeric)
\item Columns (\code{ydim}, numeric)
\item X Weight (\code{xweight}, numeric)
\item Topology (\code{topo}, character)
}


\strong{Semi-Naive Structure Learner Wrapper} (\code{method = 'nbSearch'})

For classification using package \pkg{bnclassify} with tuning parameters:
\itemize{
\item Number of Folds (\code{k}, numeric)
\item Minimum Absolute Improvement (\code{epsilon}, numeric)
\item Smoothing Parameter (\code{smooth}, numeric)
\item Final Smoothing Parameter (\code{final_smooth}, numeric)
\item Search Direction (\code{direction}, character)
}


\strong{Shrinkage Discriminant Analysis} (\code{method = 'sda'})

For classification using package \pkg{sda} with tuning parameters:
\itemize{
\item Diagonalize (\code{diagonal}, logical)
\item shrinkage (\code{lambda}, numeric)
}


\strong{SIMCA} (\code{method = 'CSimca'})

For classification using package \pkg{rrcovHD} with no tuning parameters



\strong{Simplified TSK Fuzzy Rules} (\code{method = 'FS.HGD'})

For regression using package \pkg{frbs} with tuning parameters:
\itemize{
\item Number of Fuzzy Terms (\code{num.labels}, numeric)
\item Max. Iterations (\code{max.iter}, numeric)
}


\strong{Single C5.0 Ruleset} (\code{method = 'C5.0Rules'})

For classification using package \pkg{C50} with no tuning parameters



\strong{Single C5.0 Tree} (\code{method = 'C5.0Tree'})

For classification using package \pkg{C50} with no tuning parameters



\strong{Single Rule Classification} (\code{method = 'OneR'})

For classification using package \pkg{RWeka} with no tuning parameters



\strong{Sparse Distance Weighted Discrimination} (\code{method = 'sdwd'})

For classification using package \pkg{sdwd} with tuning parameters:
\itemize{
\item L1 Penalty (\code{lambda}, numeric)
\item L2 Penalty (\code{lambda2}, numeric)
}


\strong{Sparse Linear Discriminant Analysis} (\code{method = 'sparseLDA'})

For classification using package \pkg{sparseLDA} with tuning parameters:
\itemize{
\item Number of  Predictors (\code{NumVars}, numeric)
\item Lambda (\code{lambda}, numeric)
}


\strong{Sparse Mixture Discriminant Analysis} (\code{method = 'smda'})

For classification using package \pkg{sparseLDA} with tuning parameters:
\itemize{
\item Number of  Predictors (\code{NumVars}, numeric)
\item Lambda (\code{lambda}, numeric)
\item Number of  Subclasses (\code{R}, numeric)
}


\strong{Sparse Partial Least Squares} (\code{method = 'spls'})

For classification and regression using package \pkg{spls} with tuning parameters:
\itemize{
\item Number of Components (\code{K}, numeric)
\item Threshold (\code{eta}, numeric)
\item Kappa (\code{kappa}, numeric)
}


\strong{Spike and Slab Regression} (\code{method = 'spikeslab'})

For regression using packages \pkg{spikeslab} and \pkg{plyr} with tuning parameters:
\itemize{
\item Variables Retained (\code{vars}, numeric)
}


\strong{Stabilized Linear Discriminant Analysis} (\code{method = 'slda'})

For classification using package \pkg{ipred} with no tuning parameters



\strong{Stabilized Nearest Neighbor Classifier} (\code{method = 'snn'})

For classification using package \pkg{snn} with tuning parameters:
\itemize{
\item Stabilization Parameter (\code{lambda}, numeric)
}


\strong{Stacked AutoEncoder Deep Neural Network} (\code{method = 'dnn'})

For classification and regression using package \pkg{deepnet} with tuning parameters:
\itemize{
\item Hidden Layer 1 (\code{layer1}, numeric)
\item Hidden Layer 2 (\code{layer2}, numeric)
\item Hidden Layer 3 (\code{layer3}, numeric)
\item Hidden Dropouts (\code{hidden_dropout}, numeric)
\item Visible Dropout (\code{visible_dropout}, numeric)
}


\strong{Stepwise Diagonal Linear Discriminant Analysis} (\code{method = 'sddaLDA'})

For classification using package \pkg{SDDA} with no tuning parameters



\strong{Stepwise Diagonal Quadratic Discriminant Analysis} (\code{method = 'sddaQDA'})

For classification using package \pkg{SDDA} with no tuning parameters



\strong{Stochastic Gradient Boosting} (\code{method = 'gbm'})

For classification and regression using packages \pkg{gbm} and \pkg{plyr} with tuning parameters:
\itemize{
\item Number of  Boosting Iterations (\code{n.trees}, numeric)
\item Max Tree Depth (\code{interaction.depth}, numeric)
\item Shrinkage (\code{shrinkage}, numeric)
\item Min. Terminal Node Size (\code{n.minobsinnode}, numeric)
}


\strong{Subtractive Clustering and Fuzzy c-Means Rules} (\code{method = 'SBC'})

For regression using package \pkg{frbs} with tuning parameters:
\itemize{
\item Radius (\code{r.a}, numeric)
\item Upper Threshold (\code{eps.high}, numeric)
\item Lower Threshold (\code{eps.low}, numeric)
}


\strong{Supervised Principal Component Analysis} (\code{method = 'superpc'})

For regression using package \pkg{superpc} with tuning parameters:
\itemize{
\item Threshold (\code{threshold}, numeric)
\item Number of Components (\code{n.components}, numeric)
}


\strong{Support Vector Machines with Boundrange String Kernel} (\code{method = 'svmBoundrangeString'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item length (\code{length}, numeric)
\item Cost (\code{C}, numeric)
}


\strong{Support Vector Machines with Class Weights} (\code{method = 'svmRadialWeights'})

For classification using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Sigma (\code{sigma}, numeric)
\item Cost (\code{C}, numeric)
\item Weight (\code{Weight}, numeric)
}


\strong{Support Vector Machines with Exponential String Kernel} (\code{method = 'svmExpoString'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item lambda (\code{lambda}, numeric)
\item Cost (\code{C}, numeric)
}


\strong{Support Vector Machines with Linear Kernel} (\code{method = 'svmLinear'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Cost (\code{C}, numeric)
}


\strong{Support Vector Machines with Linear Kernel} (\code{method = 'svmLinear2'})

For classification and regression using package \pkg{e1071} with tuning parameters:
\itemize{
\item Cost (\code{cost}, numeric)
}


\strong{Support Vector Machines with Polynomial Kernel} (\code{method = 'svmPoly'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Polynomial Degree (\code{degree}, numeric)
\item Scale (\code{scale}, numeric)
\item Cost (\code{C}, numeric)
}


\strong{Support Vector Machines with Radial Basis Function Kernel} (\code{method = 'svmRadial'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Sigma (\code{sigma}, numeric)
\item Cost (\code{C}, numeric)
}


\strong{Support Vector Machines with Radial Basis Function Kernel} (\code{method = 'svmRadialCost'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Cost (\code{C}, numeric)
}


\strong{Support Vector Machines with Radial Basis Function Kernel} (\code{method = 'svmRadialSigma'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item Sigma (\code{sigma}, numeric)
\item Cost (\code{C}, numeric)
}


\strong{Support Vector Machines with Spectrum String Kernel} (\code{method = 'svmSpectrumString'})

For classification and regression using package \pkg{kernlab} with tuning parameters:
\itemize{
\item length (\code{length}, numeric)
\item Cost (\code{C}, numeric)
}


\strong{The Bayesian lasso} (\code{method = 'blasso'})

For regression using package \pkg{monomvn} with tuning parameters:
\itemize{
\item Sparsity Threshold (\code{sparsity}, numeric)
}


\strong{The lasso} (\code{method = 'lasso'})

For regression using package \pkg{elasticnet} with tuning parameters:
\itemize{
\item Fraction of Full Solution (\code{fraction}, numeric)
}


\strong{Tree Augmented Naive Bayes Classifier} (\code{method = 'tan'})

For classification using package \pkg{bnclassify} with tuning parameters:
\itemize{
\item Score Function (\code{score}, character)
\item Smoothing Parameter (\code{smooth}, numeric)
}


\strong{Tree Augmented Naive Bayes Classifier Structure Learner Wrapper} (\code{method = 'tanSearch'})

For classification using package \pkg{bnclassify} with tuning parameters:
\itemize{
\item Number of Folds (\code{k}, numeric)
\item Minimum Absolute Improvement (\code{epsilon}, numeric)
\item Smoothing Parameter (\code{smooth}, numeric)
\item Final Smoothing Parameter (\code{final_smooth}, numeric)
\item Super-Parent (\code{sp}, logical)
}


\strong{Tree Augmented Naive Bayes Classifier with Attribute Weighting} (\code{method = 'awtan'})

For classification using package \pkg{bnclassify} with tuning parameters:
\itemize{
\item Score Function (\code{score}, character)
\item Smoothing Parameter (\code{smooth}, numeric)
}


\strong{Tree Models from Genetic Algorithms} (\code{method = 'evtree'})

For classification and regression using package \pkg{evtree} with tuning parameters:
\itemize{
\item Complexity Parameter (\code{alpha}, numeric)
}


\strong{Tree-Based Ensembles} (\code{method = 'nodeHarvest'})

For classification and regression using package \pkg{nodeHarvest} with tuning parameters:
\itemize{
\item Maximum Interaction Depth (\code{maxinter}, numeric)
\item Prediction Mode (\code{mode}, character)
}


\strong{Variational Bayesian Multinomial Probit Regression} (\code{method = 'vbmpRadial'})

For classification using package \pkg{vbmp} with tuning parameters:
\itemize{
\item Theta Estimated (\code{estimateTheta}, character)
}


\strong{Wang and Mendel Fuzzy Rules} (\code{method = 'WM'})

For regression using package \pkg{frbs} with tuning parameters:
\itemize{
\item Number of Fuzzy Terms (\code{num.labels}, numeric)
\item Membership Function (\code{type.mf}, character)
}


\strong{Weighted Subspace Random Forest} (\code{method = 'wsrf'})

For classification using package \pkg{wsrf} with tuning parameters:
\itemize{
\item Number of Randomly Selected Predictors (\code{mtry}, numeric)
}


}
\references{``Using your own model in \code{\link{train}}'' (\url{http://caret.r-forge.r-project.org/custom_models.html})}
\keyword{models}
