# =============================================
# = DROrthoForest (Doubly Robust ORF)   =
# =============================================
# on R, run
# source_python("./GitControlled/Codes/0_0_functions.py")
# test <- run_dr_orf(y=Y, t=T, x=X_train, x_eval=X_eval)

# def run_dr_orf(y,t,x, x_eval):

#   # Define some parameters
#   n_trees = 1000
#   min_leaf_size = 50
#   max_depth = 20
#   subsample_ratio = 0.5

#   dr_orf_est = DROrthoForest(
#     n_trees = n_trees, 
#     min_leaf_size = min_leaf_size,
#     max_depth = max_depth, 
#     subsample_ratio = subsample_ratio
#     )

#   #--- Build an ORF from a training set ---#
#   dr_orf_est.fit(y, t, X=x)

#   #--- Calculate the heterogeneous treatment effect ---#
#   dr_orf_te_pred = dr_orf_est.effect(X = x_eval)

#   #--- Calculate default (95%) confidence intervals for the default treatment points T0=0 and T1=1 ---#
#   dr_orf_te_lower, dr_orf_te_upper = dr_orf_est.effect_interval(x_eval)

#   #--- Results ---#
#   res_dr_orf_df = pd.DataFrame({
#     "te_pred" : dr_orf_te_pred,
#     "te_lower" : dr_orf_te_lower,
#     "te_upper" : dr_orf_te_upper
#   })

#   return res_dr_orf_df

# ======  End of Section comment block  =======

def run_DR_OF(
  Y,
  T,
  X,
  W,
  X_test,
  n_trees=1000,
  min_leaf_size=10,
  max_depth=30,
  subsample_ratio=0.7,
  lambda_reg=0.33,
  se=True,
):

  est = DROrthoForest(
    n_trees=n_trees,
    min_leaf_size=min_leaf_size,
    max_depth=max_depth,
    subsample_ratio=subsample_ratio,
    # propensity_model=LogisticRegression(
    #     C=1 / (X.shape[0] * lambda_reg), penalty="l1", solver="saga"
    # ),
    # model_Y=Lasso(alpha=lambda_reg),
    model_Y=GradientBoostingRegressor(
      n_estimators=100, min_samples_leaf=10, max_depth=30
      ),
        # propensity_model_final=LogisticRegression(
        #     C=1 / (X.shape[0] * lambda_reg), penalty="l1", solver="saga"
        # ),
        # model_Y_final=WeightedLasso(alpha=lambda_reg, max_iter=4000),
        # model_Y_final=GradientBoostingRegressor(
        #     n_estimators=100, min_samples_leaf=10, max_depth=30
        # )
    )

  est.fit(Y, T, X=X, W=W)
  
  treatment_effects = est.const_marginal_effect(X_test)
  
  # te_lower, te_upper = est.const_marginal_effect_interval(X_test)

  return(treatment_effects)


# =============================================
# = DMLOrthoForest (Double Machine Learning ORF) =
# =============================================
def run_DML_OF(
  Y,
  T,
  X,
  W,
  X_test,
  n_trees=1000,
  min_leaf_size=10,
  max_depth=50,
  subsample_ratio=0.7,
  lambda_reg=0.33
  ):

  # * When T is continuous, it is assumed the linearity of the impact of T on Y
  est = DMLOrthoForest(
    n_trees=n_trees,
    min_leaf_size=min_leaf_size,
    max_depth=max_depth,
    subsample_ratio=subsample_ratio,
    model_T=Lasso(alpha=lambda_reg),
    model_Y=Lasso(alpha=lambda_reg),
    model_T_final=WeightedLasso(alpha=lambda_reg),
    model_Y_final=WeightedLasso(alpha=lambda_reg),
    # global_residualization=False,
    # discrete_treatment = True,
    random_state=123
    )

  # est = DMLOrthoForest(
  #   n_trees = n_trees, 
  #   min_leaf_size = min_leaf_size, 
  #   max_depth = max_depth, 
  #   subsample_ratio = subsample_ratio
  #   )

  #--- Build an ORF from a training set ---#
  # est.fit(r.Y, r.T, X=r.X, W=r.W, inference="blb")
  est.fit(Y, T, X=X, W=W)

  #--- Calculate the heterogeneous treatment effect ---#
  # treatment_effects = est.effect(X_test)
  treatment_effects = est.const_marginal_effect(X_test)

  #--- Calculate default (95%) confidence intervals for the default treatment points T0=0 and T1=1 ---#
  # te_lower, te_upper = est.effect_interval(X_test)
  
  # return treatment_effects, te_lower, te_upper
  return treatment_effects


# ======  End of Section comment block  =======















