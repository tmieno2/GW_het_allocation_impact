def run_dr_orf(
    Y,
    T,
    X,
    W,
    X_test
):

    est = DROrthoForest(
        n_trees=1000,
        min_leaf_size=10,
        max_depth = 50,
        subsample_ratio = 0.7,
        propensity_model = GradientBoostingClassifier(
            n_estimators=100, min_samples_leaf=10, max_depth=30
        ),
        model_Y = GradientBoostingRegressor(
            n_estimators=100, min_samples_leaf=10, max_depth=30
        )
        # propensity_model=LogisticRegression(C=1, solver='lbfgs'),
        # model_Y=Lasso(alpha=0.01),
        # propensity_model_final=LogisticRegressionCV(cv=3, solver='lbfgs'),
        # model_Y_final=WeightedLassoCV(cv=3),
        verbose=0
    )

    est.fit(Y, T, X=X, W=W)
    treatment_effects = est.effect(X_test)
    te_lower, te_upper = est.effect_interval(X_test)
 
    return treatment_effects, te_lower, te_upper