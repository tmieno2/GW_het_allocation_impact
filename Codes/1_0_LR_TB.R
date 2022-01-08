# ==========================================================================
# Organize Data
# ==========================================================================
#' Objective: 
#' + Organize data (ir_reg.rds) for the the water analysis focusing 
#' on regions of TB (Tri-Basin, control group)vs LR (Lower-Republican, treatment group)


# /*===== library =====*/
library(here)
source(here("GitControlled/Codes/0_1_ls_packages.R"))

# /*===== import data =====*/
ir_data <- readRDS(here('Shared/Data/ir_reg.rds')) %>%
	filter(source %in% c('Meter','METER','metered')) %>%
	mutate(
		trs=paste(twnid,rngid,section,sep='_'),
		tr=paste(twnid,rngid,sep='_')
		) %>%
	data.table() %>%
	.[,phase1:=ifelse(year<2008,1,0)] %>%
	.[,phase2:=ifelse(year>=2008,1,0)]


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

saveRDS(data_w_W1, here("Shared/Data/WaterAnalysis/data_w_LR_TB.rds"))

#' NOTE: `data_w_W1` includes three cases; 
#' Case 1: 11 inches (LR east) vs no limit (TB) :2007
#' Case 2: 12 inches (LR west) vs no limit (TB) :2007
#' Case 3: 11 inches (LR) vs no limit (TB) :2008 - 2015


# If you want to focus on Case 3, use the following codes to filter the data
data_reg_case3 <- data_w_W1 %>%
	.[year >= 2008, ] %>%
	.[,..all_vars] %>% 
	.[usage<=40,] %>% # Why?? -> make upper limit
	.[,tr_year:=factor(paste0(tr,year))] %>% 
	na.omit()


