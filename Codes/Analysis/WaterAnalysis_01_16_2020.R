######################################
# Run regressions: Irrigation
######################################

#===================================
# 0. Preliminary Operations
#===================================
#--- setwd ---#
setwd('~/Box/ResearchProjects/HeterogeneousAllocation')

#--- library ---#
source('~/Box/R_libraries_load/library.R')
library(lfe)
library(multiwayvcov)
library(grf)
library(plm)
source('./Codes/Functions/functions.R')

#--- import the data ---#
ir_data <- readRDS('./Data/ir_reg.rds') %>%
	filter(source %in% c('Meter','METER','metered')) %>%
	mutate(
		trs=paste(twnid,rngid,section,sep='_'),
		tr=paste(twnid,rngid,sep='_')
		) %>%
	data.table() %>%
	.[,phase1:=ifelse(year<2008,1,0)] %>%
	.[,phase2:=ifelse(year>=2008,1,0)]

#===================================
# Causal Forest Estimation
#===================================
# t5r22: township 5 range 22 located within the Tri-Basin has allocation limits

#--- data ---#
data_w <- ir_data %>%
	filter(Low_Tri_5mi==1) %>%
	mutate(
		treat1e=ifelse(phase1==1 & nrdname=='Lower Republican' & in_east==1,1,0),
		treat1w=ifelse(phase1==1 & nrdname=='Lower Republican' & in_west==1,1,0),
		treat2=ifelse(((nrdname=='Lower Republican' & phase2==1) | (t5r22==1& year>=2009)),TRUE,FALSE),
		treat1e_pin=treat1e*precip_in,
		treat1w_pin=treat1w*precip_in,
		treat2_pin=treat2*precip_in
		) %>%
	data.table() %>% 
	.[,`:=`(
		mean_precip=mean(precip_in),
		mean_gdd=mean(gdd_in),
		mean_tmin=mean(tmin_in),
		mean_tmax=mean(tmax_in)
	),by=wellid]

#/*----------------------------------*/
#' ## individual 
#/*----------------------------------*/

cov_ls <- c('precip_in','gdd_in','slope','kv','awc')

all_vars <- c(cov_ls,'usage','treat2','tr','year')

data_reg[,year] %>%  unique() %>% sort()

data_reg <- copy(data_w) %>% 
	.[year>=2008,] %>% 
	.[,..all_vars] %>% 
	.[usage<=40,] %>% 
	.[,tr_year:=factor(paste0(tr,year))] %>% 
	na.omit()

tau_forest <- causal_forest(
  data_reg[,..cov_ls],
  data_reg[,usage],
  data_reg[,treat2+0],
  # min.node.size = 10,
	cluster=data_reg[,tr_year],
	tune.parameters='all',
  num.trees=4000
)

gen_impact_viz(
	cf_res=tau_forest,
	data_base=data_reg,
	treat_var='treat2',
	var_ls=cov_ls
)



#/*----------------------------------*/
#' ## Mean 
#/*----------------------------------*/
data_cov_mean <- data_w[year>=2008 & usage<=40,] %>%
	.[,.(wellid,mean_precip,mean_gdd,mean_tmin,mean_tmax,sand_pct,silt_pct,slope,kv,awc)]  %>%
	group_by(wellid) %>%
	summarize_all(mean) %>%
	data.table()

y_mean <- data_w[year>=2009,.(usage=mean( usage )),by=wellid]
trs_data <- unique( data_w ,by='wellid') %>%
	.[,.(wellid,tr,trs,treat2)]

data_CF_mean <- data_cov_mean[y_mean,on='wellid'] %>%
	.[trs_data,on='wellid'] %>%
	na.omit() %>% 
	.[usage<20,]

# data_CF_mean[,usage] %>% hist()

cov_ls <- c('mean_precip','mean_gdd','slope','kv','awc')


tau_forest_mean <- causal_forest(
  data_CF_mean[,..cov_ls],
  data_CF_mean[,usage],
  data_CF_mean[,treat2],
  # min.node.size = 20,
	cluster=data_CF_mean[,factor(tr)],
	tune.parameters='all',
  num.trees=4000
)

gen_impact_viz(
	cf_res=tau_forest_mean,
	data_base=data_CF_mean,
	treat_var='treat2',
	var_ls=cov_ls
)

