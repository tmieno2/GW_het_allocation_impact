# ==========================================================================
# Get ssurgo data for individual well
# ==========================================================================
#' Objective
#' + Download SSURGO Dataset for individual well
#' 		+ The SSURGO database contains information about soil as collected 
#' 			by the National Cooperative Soil Survey over the course of a century.

# /*===== Preparation =====*/
library(here)

# --- load packages  --- #
source(here("GitControlled/Codes/0_1_ls_packages.R"))

library(soilDB)
library(aqp)
library(sp)

# --- load functions --- #
source(here("GitControlled/Codes/0_0_functions.R"))



#/*----------------------------------*/
#' ## Preparation
#/*----------------------------------*/

# /*===== point data for well location =====*/
data_w_LR_TB <- readRDS(here("Shared/Data/WaterAnalysis/data_w_LR_TB.rds"))

unique_well_sf <- data_w_LR_TB %>%
  unique(.,by="wellid") %>%
  .[,.(wellid, longdd, latdd)] %>%
  # --- the geographic coordinate in the well data is NAD83 (espg=4269)--- #
  st_as_sf(., coords = c("longdd","latdd"), crs = 4269)


unique_well_sp <- as(unique_well_sf, "Spatial")


# /*===== Get SSURGO (Soil Survey Geographic Database) data =====*/
soil_var <- c(
	# --- "Horizon" --- #
	"sandtotal_r", # a fraction of sand(%)
	"claytotal_r", # a fraction of clay (%)
	"silttotal_r", # a faction of silt (%) 
	"ksat_r", # hydraulic conductivity (um/m)
	"awc_r", # available water capacity (cm/cm)
	# --- "Component" --- #
	"slope_r" # The difference in elevation between two points, expressed as a percentage of the distance between those points. (SSM), "component"
	)

res_ssurgo_sf <- get_ssurgo_props(
	field = unique_well_sp,
	vars = soil_var,
	summarize = FALSE
	) %>%
	dplyr::select("mukey", any_of(soil_var))


saveRDS(res_ssurgo_sf, here("Shared/Data/WaterAnalysis/ssurgo_ready.rds"))

