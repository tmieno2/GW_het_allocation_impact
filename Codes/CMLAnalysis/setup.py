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


# Helper imports
import numpy as np
from itertools import product
from sklearn.linear_model import Lasso, LassoCV, LogisticRegression, LogisticRegressionCV
import matplotlib.pyplot as plt
