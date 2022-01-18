# =============================================
# = DROrthoForest (Doubly Robust ORF)   =
# =============================================
# on R, run
# source_python("./GitControlled/Codes/0_0_functions.py")
# test <- run_dr_orf(y=Y, t=T, x=X_train, x_eval=X_eval)

def run_dr_orf(y,t,x, x_eval):

  # Define some parameters
  n_trees = 1000
  min_leaf_size = 50
  max_depth = 20
  subsample_ratio = 0.5

  dr_orf_est = DROrthoForest(
    n_trees = n_trees, 
    min_leaf_size = min_leaf_size,
    max_depth = max_depth, 
    subsample_ratio = subsample_ratio
    )

  #--- Build an ORF from a training set ---#
  dr_orf_est.fit(y, t, X=x)

  #--- Calculate the heterogeneous treatment effect ---#
  dr_orf_te_pred = dr_orf_est.effect(X = x_eval)

  #--- Calculate default (95%) confidence intervals for the default treatment points T0=0 and T1=1 ---#
  dr_orf_te_lower, dr_orf_te_upper = dr_orf_est.effect_interval(x_eval)

  #--- Results ---#
  res_dr_orf_df = pd.DataFrame({
    "te_pred" : dr_orf_te_pred,
    "te_lower" : dr_orf_te_lower,
    "te_upper" : dr_orf_te_upper
  })

  return res_dr_orf_df

# ======  End of Section comment block  =======



# =============================================
# = DMLOrthoForest (Double Machine Learning ORF) =
# =============================================

def run_dml_orf(y,t,x, x_eval):

  # Define some parameters
  n_trees = 1000
  min_leaf_size = 50
  max_depth = 20
  subsample_ratio = 0.5

  dml_orf_est = DMLOrthoForest(
        n_trees = n_trees, 
        min_leaf_size = min_leaf_size, 
        max_depth = max_depth, 
        subsample_ratio = subsample_ratio
       )

  #--- Build an ORF from a training set ---#
  dml_orf_est.fit(y, t, X=x)
  
  #--- Calculate the heterogeneous treatment effect ---#
  dml_orf_te_pred = dml_orf_est.effect(X = x_eval)
  
  #--- Calculate default (95%) confidence intervals for the default treatment points T0=0 and T1=1 ---#
  dml_orf_te_lower, dml_orf_te_upper = dml_orf_est.effect_interval(x_eval)
  
  #--- Results ---#
  res_dml_orf_df = pd.DataFrame({
    "te_pred" : dml_orf_te_pred,
    "te_lower" : dml_orf_te_lower,
    "te_upper" : dml_orf_te_upper
    })

  return res_dml_orf_df

# ======  End of Section comment block  =======















