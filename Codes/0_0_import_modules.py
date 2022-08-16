import econml

## Ignore warnings
import warnings
warnings.filterwarnings("ignore")

# Main imports
from econml.orf import DMLOrthoForest, DROrthoForest
from econml.dml import CausalForestDML

from econml.sklearn_extensions.linear_model import (
    WeightedLassoCVWrapper,
    WeightedLasso,
    WeightedLassoCV,
)

from sklearn.linear_model import (
    Lasso, LassoCV, LogisticRegression, LogisticRegressionCV,
    LinearRegression, MultiTaskElasticNet, MultiTaskElasticNetCV
)


from sklearn.ensemble import RandomForestRegressor,RandomForestClassifier, GradientBoostingRegressor, GradientBoostingClassifier
from sklearn.multioutput import MultiOutputRegressor

# Helper imports
import numpy as np
import pandas as pd

from itertools import product