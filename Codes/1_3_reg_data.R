# ==========================================================================
# Create regression data 
# ==========================================================================
#' Object: 
#' + merge all the data (weather and soil data) together
#' 	- weather: gridMET_ready.rds
#' 	- soil: ssurgo_ready.rds
#' + create the complete data for regression analysis
#' + create aggregated data
#' 	- data only for 2008-2012, not for the entire period of phase 2 ( 2008-2015)



# /*=================================================*/
#' # Preparation
# /*=================================================*/
library(here)
# --- load packages  --- #
source(here("GitControlled/Codes/0_1_ls_packages.R"))

# --- load functions --- #
source(here("GitControlled/Codes/0_0_functions.R"))


# /*===== original regression data =====*/
data_w_LR_TB <- readRDS(here("Shared/Data/WaterAnalysis/data_w_LR_TB.rds"))


# /*===== weather and soil data =====*/
gmet_dt <- readRDS(here("Shared/Data/WaterAnalysis/gridMET_ready.rds"))

ssurgo_dt <- readRDS(here("Shared/Data/WaterAnalysis/ssurgo_ready.rds"))

gmet_ssurgo_dt <- ssurgo_dt[gmet_dt, on = "wellid"]


# /*=================================================*/
#' # Create new regression data
# /*=================================================*/

# /*===== substruct data to update some columns=====*/

var_ls <- c(
	"wellid",
	"nrdname",
	"year",
	"trs", # index for clustering,  
	"tr", # index for clustering, 
	"treat1e", # treatment indicator for phase1: LR east vs TB (2007)
	"treat1w", # treatment indicator for phase1: LR west vs TB (2007 - 2008)
	"treat2", # treatment indicator for phase2: LR vs TB (2008 - 2015)
	"usage" # dependent variable
	)

# /*---- check ----*/
# lapply(var_ls, function(x){any(is.na(data_w_LR_TB[[x]]))})
# So, this means that for some wells in some years, "usage" is missing

# --- for example --- #
# data_w_LR_TB[wellid==709,..var_ls]
# well_ssurgo_gmet_dt[wellid==709]

# /*-------- end ---------*/

sub_data_w_LR_TB <- data_w_LR_TB [usage <= 100, ..var_ls] %>%
	na.omit()


# /*===== merge source data with weather and soil data =====*/
# + well_ssurgo_gmet_dt has NA 
comp_reg_dt <- gmet_ssurgo_dt[sub_data_w_LR_TB, on=c("year", "wellid")] %>%
	na.omit()

saveRDS(comp_reg_dt, here("Shared/Data/WaterAnalysis/comp_reg_dt.rds"))


#/*----------------------------------*/
#' ## aggregated data by years
#/*----------------------------------*/

agg_comp_reg_dt <- comp_reg_dt %>%
  .[year %in% 2008:2012 & usage <= 100,] %>%
  .[,`:=`(
    sum_usage = sum(usage),
    pr_in = sum(pr_in),
    mean_tmin = mean(tmmn_in),
    mean_tmax = mean(tmmx_in),
    sum_gdd = sum(gdd_in)
  ),by=wellid] %>%
  unique(., by="wellid")

# summary(agg_comp_reg_dt[["usage"]])
# length(unique(agg_comp_reg_dt[,wellid]))


saveRDS(agg_comp_reg_dt, here("Shared/Data/WaterAnalysis/agg_comp_reg_dt.rds"))

































