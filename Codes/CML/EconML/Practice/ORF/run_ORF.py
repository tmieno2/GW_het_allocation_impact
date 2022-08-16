#### ====  Run ORF ==== ####

# DMLOrthoForest: OrthoForest for continuous or discrete treatments using the DML residual on residual moment function.
est = DMLOrthoForest(
    n_trees=1000, min_leaf_size=5,
    max_depth=50, subsample_ratio=subsample_ratio,
    model_T=Lasso(alpha=lambda_reg),
    model_Y=Lasso(alpha=lambda_reg),
    model_T_final=WeightedLasso(alpha=lambda_reg),
    model_Y_final=WeightedLasso(alpha=lambda_reg),
    global_residualization=False,
    random_state=123)

#### ==== Build an orthogonal random forest from a training set (Y, T, X, W). ==== ####
# fit(Y, T, *, X[, W, inference])
# To use the built-in confidence intervals constructed via Bootstrap of Little Bags, we can specify inference="blb" 
est.fit(Y, T, X=X, W=W, inference="blb")


#### ==== Calculate heterogeneous treatment effects ==== ####
# effect(X=None, *, T0=0, T1=1)
# Calculate the heterogeneous treatment effect 𝜏(𝑋,𝑇0,𝑇1)
# The effect is calculated between the two treatment points conditional on a vector of features on a set of m test samples {𝑇0𝑖,𝑇1𝑖,𝑋𝑖}

treatment_effects = est.effect(X_test)



#### ==== Calculate default (95%) confidence intervals for the test data ==== ####
# Confidence intervals for the quantities 𝜏(𝑋,𝑇0,𝑇1) produced by the model. 
# Available only when inference is not None, when calling the fit method.

te_lower, te_upper = est.effect_interval(X_test)


#### ==== inference (prediction) ==== ####
# Inference results for the quantities 𝜏(𝑋,𝑇0,𝑇1) produced by the model.
# Available only when inference is not None, when calling the fit method
# from econml.orf import DMLOrthoForest

res = est.effect_inference(X_test)


# ==== sumamry table ==== #
report = res.summary_frame().head()


# ==== 
res.population_summary()