# ==========================================================================
# Get ssurgo data for individual well
# ==========================================================================
#' Objective
#' + Download c Dataset for individual well
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


# /*=================================================*/
#' # Preparation
# /*=================================================*/

# /*===== point data for well location =====*/
data_w_LR_TB <- readRDS(here("Shared/Data/WaterAnalysis/data_w_LR_TB_nw.rds"))

unique_wells_loc_sf <- 
	data_w_LR_TB %>%
  unique(.,by=c("acres", "latdd", "longdd")) %>%  
   .[,.(wellid, acres, latdd, longdd)] %>%
  # --- acre to square meter --- #
  .[,field_radius := round(sqrt(4046.8564224*acres/3.1416),2)] %>%
  # --- the geographic coordinate in the well data is NAD83 (espg=4269)--- #
  st_as_sf(., coords = c("longdd","latdd"), crs = 4269) %>%
  #--- project to WGS UTM 14 ---#
  st_transform(32614)

# hist(unique_wells_loc_sf$field_radius)


# /*===== make buffer around each well  =====*/
# the radius of each buffer varies by the size of the field 
well_buffer_sf <- 
	unique_wells_loc_sf %>%
  # --- make buffers--- #
  st_buffer(., dist = unique_wells_loc_sf$field_radius)
 
ggplot()+
	geom_sf(data=unique_wells_loc_sf, size=0.5) +
	geom_sf(data=well_buffer_sf, color="red", fill=NA)

well_buffer_sp <- as(well_buffer_sf, "Spatial")


# /*=================================================*/
#' # Get SSURGO (Soil Survey Geographic Database) data
# /*=================================================*/

soil_var <- c(
	# --- "Horizon" --- #
	"sandtotal_r", # a fraction of sand (0.05mm to 2.0mm)(%)
	"claytotal_r", # a fraction of clay (%)
	"silttotal_r", # a faction of silt (%) 
	"ksat_r", # hydraulic conductivity (um/m)
	"awc_r", # available water capacity (cm/cm)
	# --- "Component" --- #
	"slope_r" # The difference in elevation between two points, expressed as a percentage of the distance between those points. (SSM), "component"
	)

# NOTE: each filed (buffer) data should be separately passed on get_ssurgo_props()
res_ssurgo <- lapply(
	seq_len(nrow(well_buffer_sp)), 
	function(x) {
			print(paste0("working on wellid: ", well_buffer_sp[x, ]$wellid, " field"))
		# x=1
			get_ssurgo_props(
				field = well_buffer_sp[x,],
				vars = soil_var,
				summarize = TRUE
			) %>%
			.[, wellid := well_buffer_sp[x, ]$wellid]
	}) %>%
	bind_rows()

saveRDS(res_ssurgo, here("Shared/Data/WaterAnalysis/ssurgo_ready.rds"))







