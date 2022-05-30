# ==========================================================================
# Create regression data 
# ==========================================================================
#' Object: 
#' + merge all the data (weather and soil data) together
#' 	- weather: gridMET_ready.rds
#' 	- soil: ssurgo_ready.rds
#' + create data for regression analysis


# /*=================================================*/
#' # Preparation
# /*=================================================*/
library(here)
# --- load packages  --- #
source(here("GitControlled/Codes/0_1_ls_packages.R"))
 
# --- load functions --- #
source(here("GitControlled/Codes/0_0_functions.R"))


# /*===== original regression data =====*/
data_w_LR_TB <- readRDS(here("Shared/Data/WaterAnalysis/data_w_LR_TB_nw.rds"))

# /*===== weather and soil data =====*/
gmet_dt <- readRDS(here("Shared/Data/WaterAnalysis/gridMET_ready.rds"))

ssurgo_dt <- readRDS(here("Shared/Data/WaterAnalysis/ssurgo_ready.rds"))


# /*=================================================*/
#' # Merge irrigation data with weather and soil data
# /*=================================================*/
w_analysis_dt <- 
	data_w_LR_TB %>%
	gmet_dt[., on=c("year", "latdd", "longdd")] %>%
	ssurgo_dt[., on = "wellid"] %>%
	.[,owner_name := paste0(firstname, "_", lastname)]

# === Check === #
sapply(w_analysis_dt, function(x) any(is.na(x)))

saveRDS(w_analysis_dt, here("Shared/Data/WaterAnalysis/w_analysis_dt.rds"))

