# ==========================================================================
# Organize Data
# ==========================================================================

# /*===== library =====*/
library(here)
source(here("GitControlled/Codes/0_1_ls_packages.R"))

# /*===== Data =====*/
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
	"cntyname","twnid", "rngid", "rngdir", "section", "trs", "tr", "in_east", "in_west",
	"in_5mi", "in_10mi", "t5r22",
	"phase1", "phase2",
	# --- well characteristics --- #
	"swl",
	"pwl",
	"totaldepth",
	"pumprate",
	"volaf",
	"i.acres",
	"acres",
	# --- main variables --- #
	"usage"
	)

data_w_LR_TB <- 
	here("Shared/Data/WaterAnalysis/ir_data_nw.rds") %>%
	readRDS() %>%
	# --- select wells in 10 miles buffer --- #
	.[in_10mi==1, ..var_ls] %>%
	# --- treatment indicator --- #
	.[,`:=`(
		#--- LR east vs TB (2007-2008)---#
		treat1e=ifelse(phase1==1 & nrdname=='Lower Republican' & in_east==1, 1, 0),
		#--- LR west vs TB (2007 - 2008) ---#
		treat1w=ifelse(phase1==1 & nrdname=='Lower Republican' & in_west==1, 1, 0),
		#--- LR vs TB (2008 - 2015)---#
		treat2=ifelse(((nrdname=='Lower Republican' & phase2==1) | (t5r22==1& year>=2009)), 1, 0)
	)] %>%
	.[,owner_name := paste0(firstname, "_", lastname)] %>%
	# --- remove rows where "usage" is NA --- #
	na.omit(., cols = "usage")



#/*--------------------------------*/
#' ## Check
#/*--------------------------------*/
sapply(data_w_LR_TB, function(x) any(is.na(x)))

# === Check 1 === #
# summary(data_w_LR_TB$usage)
# this indicates some well's usage is Inf.
# Find those wells
# data_w_LR_TB[usage==Inf]
# they have Inf of usage because they have zero values in acres

# remove those
data_w_LR_TB <- data_w_LR_TB[!usage == Inf]

# summary(data_w_LR_TB$usage)

saveRDS(data_w_LR_TB, here("Shared/Data/WaterAnalysis/data_w_LR_TB_nw.rds"))



