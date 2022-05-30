def run_dml_orf_c(
    Y,
    T,
    X,
    W,
    X_test
):

    est = DMLOrthoForest(
        n_trees=1000,
        min_leaf_size=10,
        max_depth=50,
        subsample_ratio = 0.7,
        # === for discrete treatment === #
        # model_T=LogisticRegression(C=1, solver='lbfgs'),
        # model_Y=Lasso(alpha=0.01),
        # model_T_final=LogisticRegressionCV(cv=3,  solver='lbfgs'),
        # model_Y_final=WeightedLassoCV(cv=3),
        # === for continuous treatment === #
        model_T=Lasso(alpha=0.01),
        model_Y=Lasso(alpha=0.01),
        model_T_final=WeightedLassoCV(alpha=0.01),
        model_Y_final=WeightedLassoCV(alpha=0.01),
        # If discrete_treatment=True, model_T is treated as a classifier that must have a predict_proba method.
        discrete_treatment=False,
        verbose=0
    )

    est.fit(Y, T, X=X, W=W)
    treatment_effects = est.effect(X_test)
    te_lower, te_upper = est.effect_interval(X_test)
 
    return treatment_effects, te_lower, te_upper
