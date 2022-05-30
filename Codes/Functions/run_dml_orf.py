def run_dml_orf(
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
        model_T=LogisticRegression(C=1, solver='saga', multi_class='auto'),
        model_Y=Lasso(alpha=0.01),
        model_T_final=LogisticRegressionCV(cv=3,  solver='saga', multi_class='auto'),
        model_Y_final=WeightedLassoCV(cv=3),
        # model_T=RandomForestClassifier(min_samples_leaf=10),
        # model_Y=GradientBoostingRegressor(),
        # model_T=GradientBoostingClassifier(min_samples_leaf=10),
        # If discrete_treatment=True, model_T is treated as a classifier that must have a predict_proba method.
        discrete_treatment=True,
        verbose=0
    )

    est.fit(Y, T, X=X, W=W)
    treatment_effects = est.effect(X_test)
    te_lower, te_upper = est.effect_interval(X_test)
 
    return treatment_effects, te_lower, te_upper
