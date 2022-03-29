def run_CF_DML(
    Y,
    T,
    X,
    W,
    X_test
):

    est = CausalForestDML(
        model_y=RandomForestRegressor(),
        model_t=RandomForestClassifier(min_samples_leaf=10),
        # model_y=GradientBoostingRegressor(),
        # model_t=GradientBoostingClassifier(min_samples_leaf=10),
        discrete_treatment=True,
        n_estimators=1000,
        min_impurity_decrease=0.001,
        verbose=0,
        cv=6,
        criterion = "het"
    )

    est.tune(Y, T, X=X, W=W)
    est.fit(Y, T, X=X, W=W)
    te_pred = est.effect(X_test)
    te_lower, te_upper = est.effect_interval(X_test, alpha=0.01)

    return te_pred, te_lower, te_upper