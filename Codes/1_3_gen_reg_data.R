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

ssurgo_dt <- readRDS(here("Shared/Data/WaterAnalysis/ssurgo_ready_new.rds"))

# ===  Confirm that weather and soil data have exactly same number of wellid 
# that the original regression data has === #
# data_w_LR_TB[,wellid] %>% unique() %>% length()
# ssurgo_dt[,wellid] %>% unique() %>% length()
# gmet_dt[,wellid] %>% unique() %>% length()


# /*=================================================*/
#' # Create new regression data
# /*=================================================*/

# === Variables to be kept for the new regression data=== #
var_ls <- c(
	# --- ID --- #
	"wellid",
	"ownerid",
	"lastname",
	"firstname",
	"wellstatusid",
	"nrdname",
	"year",
	"county", 
  	"longdd", "latdd",
	"twnid", "rngid", "rngdir", "section", "trs", "tr", 
	# --- well characteristics --- #
	"swl",
	"pwl",
	"totaldepth",
	"pumprate",
	"volaf",
	"i.acres",
	"acres",
	# --- main variables --- #
	"treat1e", # treatment indicator for phase1: LR east vs TB (2007)
	"treat1w", # treatment indicator for phase1: LR west vs TB (2007 - 2008)
	"treat2", # treatment indicator for phase2: LR vs TB (2008 - 2015)
	"usage"
	)

# /*---- check ----*/
# temp <- data_w_LR_TB[, ..var_ls]
# sapply(temp, function(x) any(is.na(x)))
# Some wells has missing values in "usage".


sub_data_w_LR_TB <- data_w_LR_TB[, ..var_ls] %>%
	# remove rows where usage is missing
	na.omit(., cols = "usage")


# /*===== merge source data with weather and soil data =====*/
# + ssurgo_dt has missing values 
comp_reg_dt <- 
	sub_data_w_LR_TB %>%
	gmet_dt[., on=c("year", "wellid")] %>%
	ssurgo_dt[., on = "wellid"] 

# sapply(comp_reg_dt, function(x) any(is.na(x)))

saveRDS(comp_reg_dt, here("Shared/Data/WaterAnalysis/comp_reg_dt.rds"))










