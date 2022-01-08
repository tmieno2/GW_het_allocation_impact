######################################
# Run regressions: Irrigation
######################################

#===================================
# 0. Preliminary Operations
#===================================
#--- library ---#
# source('~/Box/R_libraries_load/library.R')
library(here)
library(lfe)
library(multiwayvcov)
library(data.table)
library(tidyverse)
library(grf)
library(plm)

source(here('GitControlled/Codes/Functions/functions.R'))


#--- import the data ---# (yearly data)
ir_data <- readRDS(here('Shared/Data/ir_reg.rds')) %>%
	.[source %in% c('Meter','METER','metered'),] %>%
	.[,trs:=paste(twnid,rngid,section,sep='_')] %>%
	.[,tr:=paste(twnid,rngid,sep='_')] %>%
	.[,phase1:=ifelse(year<2008,1,0)] %>%
	.[,phase2:=ifelse(year>=2008,1,0)]


#============================================
# Causal Forest Estimation (LR vs Tri-Basin)
#============================================
# t5r22: township 5 range 22 located within the Tri-Basin has allocation limits

# LR: "Lower Republican"
# Tri-Basin: "Tri-Basin" 

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

# saveRDS(data_w_W1, here("Shared/Data/WaterAnalysis/data_w_LR_TB.rds"))

unique(data_w_W1$year) %>% sort()


#--- data exploration ---#
# ir_data$year%>%unique() 2007-2015
# subset_cols <- c('precip_in', 'tmin_in', 'tmax_in', 'gdd_in',
# 	'silt_pct', 'clay_pct', 'slope', 'kv', 'awc', "mean_precip", "mean_gdd", "mean_tmin", "mean_tmax")

# temp_wellid <- data_w_W1[wellid==709, subset_cols, with=FALSE]
# nrow(temp_wellid)
# unique(data_w_W1$nrdname)




#/*----------------------------------*/
#' ## individual 
#/*----------------------------------*/

####==== Cases ====####
# (1) 11 inches (LR east) vs no limit (TB) :2007
# (2) 12 inches (LR west) vs no limit (TB) :2007
# (3) 11 inches (LR) vs no limit (TB) :2008 - 2015


###--- Covariates ---###
#-- Weather --#
#' total precipitation : precip_in
#' the mean of minimum and maximum temperature for the growing season (Apr-Sep)
#' 
#-- Soil --#
#' the percentage of sand :
#' the percentage of clay
#' the percentage of silt
#' hydraulic conductivity
#' water holding capacity
#' slope



#++++ Water use ++++#
#' in season precipitation: precip_in
#' growing degree days: gdd_in
#' silt percentage: silt_pct
#' clay percentage: clay_pct
#' hydraulic conductivity: kv
#' water holding capacity: awc


# cov_ls <- c('precip_in','gdd_in','slope','kv','awc')

# Growing degree days (gdd)

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

# summary(data_reg_case3$usage)



# ==========================================================================
# Causal Forest Analysis
# ==========================================================================

X <- data_reg_case3[,cov_ls,with=FALSE]
Y <- data_reg_case3[,usage]
W <- data_reg_case3[,treat2]
#-- cluster: unique combinations of twnid,rngid, year --#
cl <- data_reg_case3[,tr_year]

#=== orthogonalization ===#

##- Outcome model -##
Y_forest <- regression_forest(X, Y, clusters=cl)
Y_hat <- predict(Y_forest)$predictions
variable_importance(Y_forest)

##- Propensity model -##
W_forest <- regression_forest(X, W, clusters=cl)
W_hat <- predict(W_forest)$predictions
variable_importance(W_forest)

##- 
cf.raw <- causal_forest(X, Y, W, 
	Y.hat = Y_hat, W.hat = W_hat,
	clusters = cl)

var_imp <- variable_importance(cf.raw)
selected.idx <- which (var_imp > mean(var_imp)) #"precip_in" "gdd_in"

#=== run CF ===#
cf1 <- causal_forest (
	X[, names(X)[selected.idx], with=FALSE], Y, W,
	Y.hat = Y_hat, 
	W.hat = W_hat,
	clusters = cl,
	num.trees=4000,
	tune.parameters = 'all' 
	)

cf2 <- causal_forest (
	X, Y, W,
	Y.hat = Y_hat, 
	W.hat = W_hat,
	clusters = cl,
	num.trees=4000,
	tune.parameters = 'all' 
	)



#--- Propensity Score (check randomization of the experiment)---#
# dt <- data.table(
# 	W = W,
# 	Propensity_score = W_hat)

# ggplot(dt) +
# 	geom_histogram(aes(x=Propensity_score))+
# 	facet_grid( ~W) 


#--- Out-of-bag CATE prediction ---#
oob_cf1_pred <- predict(cf1, estimate.variance=TRUE)
oob_cf2_pred <- predict(cf2, estimate.variance=TRUE)

hist(oob_cf1_pred$predictions)
hist(oob_cf2_pred$predictions)



#--- The conditional average treatment effect on the full sample (CATE) ---#
average_treatment_effect(cf1, target.sample="all")

average_treatment_effect(cf2, target.sample="all")




#--- assessing treatment heterogeneity ---#
gen_impact_viz(
	cf_res= cf1,
	data_base=data_reg_case3,
	treat_var='treat2',
	var_ls= names(X)[selected.idx]
)

gen_impact_viz(
	cf_res= cf2,
	data_base=data_reg_case3,
	treat_var='treat2',
	var_ls= cov_ls
)


#--- test for heterogeneity: "best linear predictor" method ---#
test_calibration(cf1)

test_calibration(cf2)



#--- TOC curve ---#

#--- Estimating the Best Linear Projections of CATE ---#
best_linear_projection(cf1, X[, names(X)[selected.idx], with=FALSE])
best_linear_projection(cf2, X)
