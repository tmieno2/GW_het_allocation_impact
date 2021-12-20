library(here)
library(lfe)
library(multiwayvcov)
library(data.table)
library(tidyverse)
library(grf)
library(plm)


#--- reticulate ---#
library(reticulate)

#- which python? -#
py_config()

# ==== python setup ==== #
source_python(here("GitControlled/GW_het_allocation_impact/Codes/CMLAnalysis/setup.py"))



#--- sourcing R.function ---#
source(here('GitControlled/GW_het_allocation_impact/Codes/Functions/functions.R'))


#--- import the data ---# (yearly data)
ir_data <- readRDS(here('Shared/Data/ir_reg.rds')) %>%
	.[source %in% c('Meter','METER','metered'),] %>%
	.[,trs:=paste(twnid,rngid,section,sep='_')] %>%
	.[,tr:=paste(twnid,rngid,sep='_')] %>%
	.[,phase1:=ifelse(year<2008,1,0)] %>%
	.[,phase2:=ifelse(year>=2008,1,0)]


#--- data ---#
data_w_W1 <- ir_data %>%
	.[Low_Tri_5mi==1, ] %>%
	.[,`:=`(
		#--- LR east vs TB (2007-2008)---#
		treat1e=ifelse(phase1==1 & nrdname=='Lower Republican' & in_east==1, 1, 0),
		#--- LR west vs TB (2007 - 2008) ---#
		treat1w=ifelse(phase1==1 & nrdname=='Lower Republican' & in_west==1, 1, 0),
		#--- LR vs TB (2008 - 2015)---#
		treat2=ifelse(((nrdname=='Lower Republican' & phase2==1) | (t5r22==1& year>=2009)), 1, 0)
		)] %>%
	.[,`:=`(
		mean_precip=mean(precip_in),
		mean_gdd=mean(gdd_in),
		mean_tmin=mean(tmin_in),
		mean_tmax=mean(tmax_in)
	),by=wellid]



cov_ls <- c('precip_in', 'tmin_in', 'tmax_in', 'gdd_in',
	'silt_pct', 'clay_pct', 'slope', 'kv', 'awc')

all_vars <- c(cov_ls,'usage','treat2', 'tr', 'year')


###--- W1 case 3: 11 inches (LR) vs no limit (TB) (2008 - 2015) ---###
data_reg_case3 <- data_w_W1 %>%
	.[year >= 2008, ] %>%
	.[,..all_vars] %>% 
	.[usage<=40,] %>% # Why?? -> make upper limit
	.[,tr_year:=factor(paste0(tr,year))] %>% 
	na.omit()


###--- training dataset ---###
Y <- data_reg_case3[,usage]
X <- data_reg_case3[,cov_ls,with=FALSE]
W <- data_reg_case3[,treat2]

Y_array <- Y %>% as.array() %>% unname()
X_array <- X %>% as.matrix() %>% unname()
W_array <- W %>% as.array() %>% unname()


###--- preparing testing dataset ---###





#-- cluster: unique combinations of twnid,rngid, year --#
cl <- data_reg_case3[,tr_year]

# ==========================================================================
# Causal Forest of GRF
# ==========================================================================

Y_forest <- regression_forest(X, Y, clusters=cl)
Y_hat <- predict(Y_forest)$predictions
W_forest <- regression_forest(X, W, clusters=cl)
W_hat <- predict(W_forest)$predictions


#=== run CF ===#
cf1 <- causal_forest(
	X, Y, W,
	Y.hat = Y_hat, 
	W.hat = W_hat,
	clusters = cl,
	num.trees=4000,
	tune.parameters = 'all' 
	)

#--- assessing treatment heterogeneity ---#
gen_impact_viz(
	cf_res= cf1,
	data_base=data_reg_case3,
	treat_var='treat2',
	var_ls= cov_ls
)

# ==========================================================================
# DMLOrthoForest: suitable for continuous or discrete treatments
# ==========================================================================


















