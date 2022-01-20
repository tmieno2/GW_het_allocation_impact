def run_DML_OF(
    Y,
    T,
    X,
    W,
    X_test,
    n_trees=1000,
    min_leaf_size=5,
    max_depth=50,
    subsample_ratio=0.3,
    lambda_reg=0.33,
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
        model_Y_final=WeightedLasso(alpha=lambda_reg)
    )

    est.fit(Y, T, X=X, W=W, inference="blb")
    treatment_effects = est.effect(X_test)
    te_lower, te_upper = est.effect_interval(X_test)

    return treatment_effects, te_lower, te_upper
