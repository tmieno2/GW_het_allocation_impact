# Overview
+ There are multiple variants of CF that targets identifying heterogeneous impacts of a treatment.
	* This python package lets you do that. (https://github.com/socket778/XBCF)
+ Don’t focus on the gory details of math. What you need to get is `how it is different from others`. They do a good job of explaining those

# Task
+ I would like you to apply them to this paper and see what you will see.
	* In doing so, try to use the reticulate package to run python programs from within R
	* Plenty of plenty of examples (https://github.com/Microsoft/EconML/tree/master/notebooks)
	* you would eventually get to this website (https://rstudio.github.io/reticulate/)


# Goal
+ Let's use (for now)
	* Causal Forest
	* Orthogonal Random Forest (ORF)
	* Double Machine Learning (aka RLearner)
	* Accelerated Bayesian Causal Forests (XBCF)
	

# 

# EconML
+ is a Python package to estimate individualized causal responses from observational or experimental data.  


## Materials
+ `EconML`: A Python Package for ML-Based Heterogeneous Treatment Effects Estimation
	* https://github.com/Shunkei3/EconML
	
+ `EconML` Examples: 
	* https://github.com/Microsoft/EconML/tree/master/notebooks
	
+ R `reticulate` package: 
	* https://rstudio.github.io/reticulate/

+ `EconML` with R and the `reticulate` package
	* https://github.com/itamarcaspi/EconML-with-R

+ `Accelerated Bayesian Causal Forests (XBCF)`
	* https://github.com/socket778/XBCF



# Estimation Methods under Unconfoundedness

## Double Machine Learning (DML)
+ is a method for estimating (heterogeneous) treatment effects when *all potential confounders/controls* (factors that simultaneously had a direct effect on the treatment decision in the collected data and the observed outcome) *are observed*, but are either *too many (high-dimensional)* for classical statistical approaches to be applicable or *their effect on the treatment and outcome cannot be satisfactorily modeled by parametric functions* (non-parametric)

	* predicting the outcome **from the controls**
	* predicting the treatment from the controls


## Doubly Robust Learning (DL)
+ similar to Double Machine Learning, is a method for estimating (heterogeneous) treatment effects **when the treatment is categorical** and *all potential confounders/controls* (factors that simultaneously had a direct effect on the treatment decision in the collected data and the observed outcome) *are observed*, but are either *too many (high-dimensional)* for classical statistical approaches to be applicable or *their effect on the treatment and outcome cannot be satisfactorily modeled by parametric functions* (non-parametric)

	* (1) predicting the outcome **from the treatment and controls**
	* (2) predicting the treatment from the controls

+ unlike Double Machine Learning *the first model predicts the outcome from **both** the treatment and the controls* as opposed to just the controls.






# Various Causal ML methods

## **(1) Orthogonal Random Forest (ORF)**
+ https://www.microsoft.com/en-us/research/project/econml/publications/
+ Our approach combines the notion of *Neyman orthogonality* of the moment equations with a *two-stage random forest based algorithm*, which generalizes prior work on Generalized Random Forests (Athey et al.,2017) and the double machine learning (double ML) approach
proposed in (Chernozhukov et al., 2017).

+ Causal Forests and Generalized Random Forests are a flexible method for estimating treatment effect heterogeneity with Random Forests. Orthogonal Random Forest (ORF) combines orthogonalization, a technique that effectively removes the confounding effect in two-stage estimation, with generalized random forests. Due to the orthogonalization aspect of this method, the ORF performs especially well in the presence of high-dimensional confounders.
	* see https://github.com/microsoft/EconML/blob/master/notebooks/Causal%20Forest%20and%20Orthogonal%20Random%20Forest%20Examples.ipynb


### feature
+ *allows for the non-parametric estimation of HTE via forest based approaches, while allowing for a hig-dimentional set of control variables W*
+ In the HTE setting, the ORF algorithm follows the *residual-on-residual regression approach* analyzed by (Chernozhukovet al., 2016) to formulate a locally Neyman orthogonal moment and then applies our orthogonal forest algorithm to this orthogonal moment.

+ double machine learning
	* *first* orthogonalizes out the effect of high-dimensional confounding factors using sophisticated machine learning algorithms, including Lasso, deep neuralnets and random forests, and *then* estimates the effect of the lower dimensional treatment variables, by running a low-dimensional linear regression between the residualized treatments and residualized outcomes.
	* assume the target function for HTE takes a parametric form and a
	* llow for a potentially high-dimensional parametric nuisance function

+ GRF
	* take a non-parametric stance at estimating HTE but do not allow for high-dimensional nuisance functions

### ORF variants
+ *DMLOrthoForest*: suitable for continuous or discrete treatments
	* OrthoForest for continuous or discrete treatments using the DML residual on residual moment function.
+ *DROrthoForest*: suitable for discrete treatments
	* OrthoForest for discrete treatments using the doubly robust moment function.
+ *CausalForest*: suitable for both discrete and continuous treatments


## **(2) Double Machine Learnming (DML)**
+ Double Machine Learning (DML) is an algorithm that applies arbitrary machine learning methods to fit the treatment and response, then uses a linear model to predict the response residuals from the treatment residuals.
+ https://github.com/Shunkei3/EconML/blob/master/notebooks/Double%20Machine%20Learning%20Examples.ipynb

### DML variants
+ *LinearDML*
+ *SparseLinearDML*
+ *DML*
+ *CausalForestDML*




## **Accelerated Bayesian Causal Forests (XBCF)**
+ R package, "XBCF" package: https://github.com/socket778/XBCF
+ Manuscript: https://projecteuclid.org/journals/bayesian-analysis/volume-15/issue-3/Bayesian-Regression-Tree-Models-for-Causal-Inference--Regularization-Confounding/10.1214/19-BA1195.full

### feature
+ performs at least as well as existing approaches for estimating heterogenous treatment effects across a range of plausible data generating processes. More importantly, it performs dramatically better in many cases, especially those with strong confounding, targeted selection, and relatively weak treatment effects, conditions we believe to be common in applied settings.

+ focus on the situations with small effect sizes, heterogeneous effects, and strong confounding. 

























