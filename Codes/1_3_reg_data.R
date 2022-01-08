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



# /*===== Preparation =====*/
library(here)

# --- load packages  --- #
source(here("GitControlled/Codes/0_1_ls_packages.R"))

# --- load functions --- #
source(here("GitControlled/Codes/0_0_functions.R"))


#/*----------------------------------*/
#' ## Load data
#/*----------------------------------*/
# /*===== original regression data =====*/
data_w_LR_TB <- readRDS(here("Shared/Data/WaterAnalysis/data_w_LR_TB.rds"))

unique_well_sf <- data_w_LR_TB %>%
  unique(.,by="wellid") %>%
  .[,.(wellid, longdd, latdd)] %>%
  # --- the geographic coordinate in the well data is NAD83 (espg=4269)--- #
  st_as_sf(., coords = c("longdd","latdd"), crs = 4269)


target_crs <- st_crs(unique_well_sf)


# /*===== weather and soil data =====*/
res_gmet_dt <- readRDS(here("Shared/Data/WaterAnalysis/gridMET_ready.rds"))

res_ssurgo_sf <- readRDS(here("Shared/Data/WaterAnalysis/ssurgo_ready.rds")) %>%
	st_transform(crs=target_crs)


# /*=================================================*/
#' # Organizing data
# /*=================================================*/

#/*----------------------------------*/
#' ## compile ssurgo data and gridMET data
#/*----------------------------------*/
# /*===== mach well location with ssurgo data =====*/
well_ssurgo_sf <- st_join(unique_well_sf, res_ssurgo_sf)

# /*===== merge weather data =====*/
well_ssurgo_gmet_dt <- left_join(well_ssurgo_sf, res_gmet_dt, by="wellid") %>%
	data.table()


#/*----------------------------------*/
#' ## create new regression data
#/*----------------------------------*/
# /*===== substruct data to update some columns=====*/
var_ls <- c(
	"wellid",
	"year",
	"trs", # index for clustering,  
	"tr", # index for clustering, 
	"treat1e", # treatment indicator for phase1: LR east vs TB (2007)
	"treat1w", # treatment indicator for phase1: LR west vs TB (2007 - 2008)
	"treat2", # treatment indicator for phase2: LR vs TB (2008 - 2015)
	"usage" # dependent variable
	)

# /*---- test ----*/
lapply(var_ls, function(x){any(is.na(data_w_LR_TB[[x]]))})
# So, this means that for some wells in some years, "usage" is missing

# --- for example --- #
data_w_LR_TB[wellid==709,..var_ls]
well_ssurgo_gmet_dt[wellid==709]

# /*----------*/


sub_data_w_LR_TB <- data_w_LR_TB[, ..var_ls] %>%
	na.omit()


# /*===== merge source data with weather and soil data =====*/
# + well_ssurgo_gmet_dt has NA 
comp_reg_dt <- well_ssurgo_gmet_dt[sub_data_w_LR_TB, on=c("year", "wellid")] %>%
	na.omit()

saveRDS(comp_reg_dt, here("Shared/Data/WaterAnalysis/comp_reg_dt.rds"))



#/*----------------------------------*/
#' ## aggregated data (data only for 2008-2012)
#/*----------------------------------*/


agg_comp_reg_dt <- comp_reg_dt %>%
  .[year %in% 2008:2012 & usage <= 40,] %>%
  .[,`:=`(
    sum_usage = sum(usage),
    pr_in = sum(pr_in),
    mean_tmin = mean(tmmn_in),
    mean_tmax = mean(tmmx_in),
    sum_gdd = sum(gdd_in)
  ),by=wellid] %>%
  unique(., by="wellid")

saveRDS(agg_comp_reg_dt, here("Shared/Data/WaterAnalysis/agg_comp_reg_dt.rds"))

































