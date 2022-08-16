# Orthogonal Random Forest (ORF)

## 1. DMLOrthoForest
+ `from econml.orf import DMLOrthoForest`
+ OrthoForest for continuous or discrete treatments using the *DML* residual on residual moment function
+ suitable for continuous or discrete treatments

### Main functions:

+ `DMLOrthoForest` : Parameter setting

+ `fit` : Build an orthogonal random forest from a training set (Y, T, X, W).

+ `effect` : *Calculate the heterogeneous treatment effect* 𝜏(𝑋,𝑇0,𝑇1)
	* τ – Heterogeneous treatment effects on each outcome for each sample Note that when Y is a vector rather than a 2-dimensional array, the corresponding singleton dimension will be collapsed (so this method will return a vector)
	

+ `effect_interval`: Confidence intervals for the quantities 𝜏(𝑋,𝑇0,𝑇1) produced by the model. Available only when inference is not None, when calling the fit method.

+ `effect_inference`: *Inference results* for the quantities 𝜏(𝑋,𝑇0,𝑇1) produced by the model. Available only when inference is not None, when calling the fit method.
	* InferenceResults – The inference results instance contains prediction and prediction standard error and can on demand calculate confidence interval, z statistic and p value. It can also output a dataframe summary of these inference results.

+ What's the difference between `effect` and `effect_inference` ? 



## 2. DROrthoForest
+ `from econml.orf import DROrthoForest`
+ OrthoForest for discrete treatments using the *doubly robust* moment function.
+ suitable for discrete treatments

