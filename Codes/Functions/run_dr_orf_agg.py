def run_dr_orf_agg(
    Y,
    T,
    X,
    W,
    X_test
):

    est = DROrthoForest(
        n_trees=1000,
        min_leaf_size=20,
        subsample_ratio = 0.5,
        verbose=0,
        # === Try 1 === #
        # propensity_model=LogisticRegression(C=1, solver='lbfgs'),
        # model_Y=Lasso(alpha=0.01),
        # propensity_model_final=LogisticRegressionCV(cv=3, solver='lbfgs'),
        # model_Y_final=WeightedLassoCV(cv=3),
        # ==== Try 2 (This is better than try 1) === #
        propensity_model = LogisticRegression(C=1, penalty='l1', solver='saga', multi_class='auto'),
        # model_Y=LassoCV(cv=3),
        model_Y=Lasso(alpha=0.01),
        propensity_model_final=LogisticRegressionCV(cv=5, penalty='l1', solver='saga', multi_class='auto'),
        model_Y_final=WeightedLassoCV(cv=5),
        # ==== Try 3 (this does not work) === #
        # propensity_model = LogisticRegression(C=1, penalty='l1', solver='saga', multi_class='auto', max_iter = 1000),
        # model_Y = GradientBoostingRegressor(
        #     n_estimators=1000, min_samples_leaf=10, max_depth=30
        # ),
        # propensity_model_final = LogisticRegressionCV(cv=3, penalty='l1', solver='saga', multi_class='auto', max_iter = 1000)
        # ==== Try 4 (This takes long time)=== #
        # propensity_model = GradientBoostingClassifier(
        #     n_estimators=1000, min_samples_leaf=10, max_depth=30
        # ),
        # model_Y=GradientBoostingRegressor(
        #     n_estimators=1000, min_samples_leaf=10, max_depth=30
        # )
    )

    est.fit(Y, T, X=X, W=W)
    treatment_effects = est.effect(X_test)
    te_lower, te_upper = est.effect_interval(X_test)
 
    return treatment_effects, te_lower, te_upper