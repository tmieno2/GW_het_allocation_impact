# 1. Single continuous treatment case

## Data generating process
$$y = abs(x) * W + \alpha_i + \phi_t + \mu$$



# 2. Continuous treatment: non-linear treatment effect

## Data generating process
$$y = \theta_1(x) * W + \theta_2(x) * W^2 + \alpha_i + \phi_t + \mu$$

+ $\theta_1(x) = x$
+ $\theta_2(x) = -0.1*x$

## 2-1. Boosted Regression Forest (residualized $y$)
+ Use `y_resid` as the dependent variable: *biased*
+ `y_resid := feols(y ~ 1 | id + t, data = .)$residuals`	

```{r}
BRF_res <-
  grf::boosted_regression_forest(
    X = data[, .(x, w)] %>% as.matrix(),
    Y = data[, y_resid],
    num.trees = 1000,
    min.node.size = 10
    # tune.parameters = TRUE
  )
```

## 2-2. Boosted Regression Forest (include FEs)
+ Use `y` and include whole bunch of dummies: *biased*

```{r}
X <-
  data[, .(x, w, fe)] %>%
  as.matrix()

BRF_res <-
  grf::boosted_regression_forest(
    X = X,
    Y = data[, y],
    num.trees = 1000,
    min.node.size = 10
    # tune.parameters = TRUE
  )
```


## 2-3. Boosted Regression Forest (include FEs)
+ Use `y` and include whole bunch of dummies: biased

```{r }

X <-
  data[, .(x, w, fe)] %>%
  as.matrix()

BRF_res <-
  grf::boosted_regression_forest(
    X = X,
    Y = data[, y],
    num.trees = 1000,
    min.node.size = 10
    # tune.parameters = TRUE
  )
 ```


## 2-4. DML-OF flexible treatment effect (`y_resid`)
+ This does not work well.

```{r}
# /*+++++++++++++++++++++++++++++++++++
#' ## Construct T matrix
# /*+++++++++++++++++++++++++++++++++++
gam_setup <- gam(y ~ s(w, k = 4, m = 2), data = data)

#* construct T matrix
T_mat <-
  predict(gam_setup, data = data, type = "lpmatrix") %>%
  #* get rid of the intercept
  .[, -1]

# /*+++++++++++++++++++++++++++++++++++
#' ## Define input data
# /*+++++++++++++++++++++++++++++++++++
Y <- data[, y_resid] %>% as.matrix() #* dependent var
# Y <- data[, y] %>% as.matrix() #* dependent var
X <- data[, x] %>% as.matrix() #* het impact driver
W <- X #* controls
X_test <-
  seq(min(X), max(X), length = 6) %>%
  as.matrix()
```


## 2-5. DML-OF flexible treatment effect (include FEs)

```{r}
# /*+++++++++++++++++++++++++++++++++++
#' ## Construct T matrix
# /*+++++++++++++++++++++++++++++++++++
#* gam set up
gam_setup <- gam(y ~ s(w, k = 4, m = 2), data = data)

#* construct T matrix
T_mat <-
  predict(gam_setup, data = data, type = "lpmatrix") %>%
  #* get rid of the intercept
  .[, -1]

# /*+++++++++++++++++++++++++++++++++++
#' ## Define input data
# /*+++++++++++++++++++++++++++++++++++
Y <- data[, y] %>% as.matrix() #* dependent var
# Y <- data[, y] %>% as.matrix() #* dependent var
X <- data[, x] %>% as.matrix() #* het impact driver
W <- data[, .(x, fe)] %>% as.matrix() #* controls
X_test <-
  seq(min(X), max(X), length = 6) %>%
  as.matrix()
```


