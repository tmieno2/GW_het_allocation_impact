def run_DML_ORF(
    Y,
    T,
    X,
    W,
    X_test
):

    est = DMLOrthoForest(
        n_trees=1000,
        min_leaf_size=10, 
        max_depth=30,
        # model_Y=RandomForestRegressor(),
        # model_T=RandomForestClassifier(min_samples_leaf=10),
        # model_Y=GradientBoostingRegressor(),
        # model_T=GradientBoostingClassifier(min_samples_leaf=10),
        model_T=LogisticRegression(C=1),
        model_Y=Lasso(alpha=0.01),
        model_T_final=LogisticRegressionCV(cv=3),
        model_Y_final=WeightedLassoCV(cv=3),
        # model_T=LogisticRegression(C=1),
        # model_Y=Lasso(alpha=0.33),
        # model_T_final=LogisticRegressionCV(cv=3),
        # model_Y_final=LassoCV(cv=3),
        #=== these modes are for continuous treatment ===#
        # model_T=Lasso(alpha=0.33),
        # model_Y=Lasso(alpha=0.33),
        # model_T=WeightedLasso(alpha=0.33),
        # model_Y=WeightedLasso(alpha=0.33),
        # model_T_final=WeightedLasso(alpha=0.33),
        # model_Y_final=WeightedLasso(alpha=0.33),
        discrete_treatment=True,
        verbose=0
    )

    est.fit(Y, T, X=X, W=W)
    treatment_effects = est.effect(X_test)
    te_lower, te_upper = est.effect_interval(X_test)
 
    return treatment_effects, te_lower, te_upper