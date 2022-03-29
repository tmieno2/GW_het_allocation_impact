# Double Machine Learning Examples
## DGP: Example Usage with Single Binary Treatment Synthetic Data and 

import econml

## Ignore warnings
import warnings
warnings.filterwarnings("ignore")

# Main imports
from econml.dml import DML, LinearDML, SparseLinearDML, CausalForestDML

# Helper imports
import numpy as np
from itertools import product
from sklearn.linear_model import (Lasso, LassoCV, LogisticRegression,
                                  LogisticRegressionCV,LinearRegression,
                                  MultiTaskElasticNet,MultiTaskElasticNetCV)
from sklearn.ensemble import RandomForestRegressor,RandomForestClassifier
from sklearn.preprocessing import PolynomialFeatures
from sklearn.model_selection import train_test_split



# Treatment effect function
def exp_te(x):
    return np.exp(2 * x[0])# DGP constants

np.random.seed(123)
n = 10
n_w = 6
support_size = 2
n_x = 3
# Outcome support
support_Y = np.random.choice(range(n_w), size=support_size, replace=False)
coefs_Y = np.random.uniform(0, 1, size=support_size)
epsilon_sample = lambda n:np.random.uniform(-1, 1, size=n)

# Treatment support
support_T = support_Y
coefs_T = np.random.uniform(0, 1, size=support_size)
eta_sample = lambda n: np.random.uniform(-1, 1, size=n) 

# Generate controls, covariates, treatments and outcomes
W = np.random.normal(0, 1, size=(n, n_w))
X = np.random.uniform(0, 1, size=(n, n_x))
# Heterogeneous treatment effects
TE = np.array([exp_te(x_i) for x_i in X])
# Define treatment
log_odds = np.dot(W[:, support_T], coefs_T) + eta_sample(n)
T_sigmoid = 1/(1 + np.exp(-log_odds))
T = np.array([np.random.binomial(1, p) for p in T_sigmoid])
# Define the outcome
Y = TE * T + np.dot(W[:, support_Y], coefs_Y) + epsilon_sample(n)

# get testing data
X_test = np.random.uniform(0, 1, size=(n, n_x))
X_test[:, 0] = np.linspace(0, 1, n)



est3 = CausalForestDML(model_y=RandomForestRegressor(),
                       model_t=RandomForestClassifier(min_samples_leaf=10),
                       discrete_treatment=True,
                       n_estimators=1000,
                       min_impurity_decrease=0.001,
                       verbose=0,
                       cv=6)
est3.tune(Y, T, X=X, W=W)
est3.fit(Y, T, X=X, W=W)
te_pred3 = est3.effect(X_test)
lb3, ub3 = est3.effect_interval(X_test, alpha=0.01)


# -----------  Subsection comment block  -----------

# Helper imports
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
%matplotlib inline

# /*===========================================*/
#'=  Training Data =
# /*===========================================*/

section
import numpy as np
import scipy.special
np.random.seed(123)
n = 10
p = 5
X = np.random.normal(size=(n, p))

true_propensity = lambda x: .4 + .2 * (x[:, 0] > 0)
true_effect = lambda x: (x[:, 0] * (x[:, 0] > 0))
true_conf = lambda x: x[:, 1] + np.clip(x[:, 2], - np.inf, 0)


T = np.random.binomial(1, true_propensity(X))
Y =  true_effect(X) * T + true_conf(X) + np.random.normal(size=(n,))




# /*===========================================*/
#'=  Testing Data =
# /*===========================================*/
X_test = np.zeros((100, p))
X_test[:, 0] = np.linspace(-2, 2, 100)