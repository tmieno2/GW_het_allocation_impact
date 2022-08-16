# Learn DML-OF
source: https://econml.azurewebsites.net/spec/estimation forest.html#what-are-the-relevant-estimator-classes

# General Classification
1. The Orthogonal Random Forest Estimator (`DROrthoForest`, `DMLOrthoForest`)
2. The Forest Double Machine Learning Estimator (`CausalForestDML`)
	+  double machine learning version of Causal Forests
3. The Forest Doubly Robust Estimator (`ForestDRLearner`)

+ They differ in a substantial manner in how they estimate the first stage regression/classification (nuisance) models.
	* OrthoForest methods fit local nuisance parameters around the target feature X and so as to optimize a local mean squared error, putting more weight on samples that look similar in the X space. 
	


# DML-OF
+ `DMLOrthoForest()` : main
	
	* n_trees
	* min_leaf_size
	* max_depth
	* subsample_ratio
	
	* lambda_reg
	* model_T: The estimator for residualizing the continuous treatment at each leaf
	* model_Y: The estimator for residualizing the outcome at each leaf
	* model_T_final: The estimator for residualizing the treatment at prediction time.
	* model_Y_final: The estimator for residualizing the outcome at prediction time.
	
	* discrete_treatment : Whether the treatment should be treated as categorical. If True, then the treatment T is one-hot-encoded and the model_T is treated as a classifier that must have a predict_proba method.
	

+ `fit()` : building models
	* Y : dependent variable
	* T : Treatment policy
	* X : Feature vector
	* W : High-dimensional controls 


+ `effect()`: Calculate the heterogeneous treatment effect:  : The effect is calculated between the two treatment points conditional on a vector of features on a set of m test samples



# DR-OF
+ main: `DROrthoForest`

+ building models: `fit()` 

+ `const_marginal_effect_inference`: Confidence intervals for the quantities θ(X)
 produced by the model. 