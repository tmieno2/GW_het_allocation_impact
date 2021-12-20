# Set up (maybe this scrip is unnecessary? or invalid?)

import econml

# Main imports
#### ==== import ORF variants ==== ####
from econml.orf import DMLOrthoForest, DROrthoForest

#### ==== CausalForestDML ==== ####
# https://econml.azurewebsites.net/_autosummary/econml.dml.CausalForestDML.html
from econml.dml import CausalForestDML

#### ==== Version of sklearn MultiTaskLassoCV that accepts weights ==== ####
from econml.sklearn_extensions.linear_model import WeightedLassoCVWrapper, WeightedLasso, WeightedLassoCV

#### ==== Random Forest ==== ####
from sklearn.ensemble import RandomForestRegressor,RandomForestClassifier

# Helper imports
import numpy as np
from itertools import product
from sklearn.linear_model import Lasso, LassoCV, LogisticRegression, LogisticRegressionCV
import matplotlib.pyplot as plt



#### ==== DMLOrthoForest ==== ####
# The base is from 
# + https://github.com/microsoft/EconML/blob/master/notebooks/Causal%20Forest%20and%20Orthogonal%20Random%20Forest%20Examples.ipynb
# + file:///Users/shunkeikakimoto/Dropbox/ResearchProject/HeterogeneousAllocation/GitControlled/EconML/Practice/R-EconML_sample/single.html
def run_DMLORF (Y, T, X, W, X_test) 

	est = DMLOrthoForest(
    	n_trees=2000, 
    	min_leaf_size=10,
    	max_depth=50, 
    	subsample_ratio= 0.5,
    	# model_T=Lasso(alpha=lambda_reg),
    	# model_Y=Lasso(alpha=lambda_reg),
    	# model_T_final=WeightedLasso(alpha=lambda_reg),
    	# model_Y_final=WeightedLasso(alpha=lambda_reg),
    	model_T=RandomForestRegressor(),
    	model_Y=RandomForestRegressor(),
    	discrete_treatment = True,
    	global_residualization=False,
    	random_state=123)

	# ====  Build an orthogonal random forest from a training set (Y, T, X, W) ==== #
		# + To use the built-in confidence intervals constructed via Bootstrap of Little Bags, we can specify inference="blb" 
	res_fit = est.fit(Y=Y, T=T, X=X, W=W, inference="blb")

	# ==== Calculate Treatment Effects (prediction phase)==== #
		# How to do with  
	treatment_effects = est.effect(X_test)

