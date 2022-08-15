# /*===========================================*/
#'=  Set up =
# /*===========================================*/
library(here)

# == spatial management == #
library(terra)
library(sf)
library(exactextractr)

library(tigris)
library(CropScapeR)

# == data wangling == #
library(tidyverse)
library(data.table)
# library(units)
library(measurements)
library(parallel)

# ==  visualization == #
library(ggplot2)
library(tmap)
library(gt)
httpgd::hgd()
httpgd::hgd_browse()
# dev.off()



# /*===========================================*/
#'=  1. Data Preparation  =
# /*===========================================*/
#' **Objective**
#' (1) Find counties that are intersecting Ogallla aquifer
#' (2) For those counties, download CDL data of the years 2008-2020
#' (3) Extract CDL values to each county polygon



# --- Ogallala Boundary --- #
hp_bd <- 
	here("Shared/Data/Boundary_HighPlains/hp_bound2010.shp") %>%
	st_read()

# --- The borders of counties intersecting with the Ogallala Boundary --- #
data(fips_codes)
fips_codes_mod <- 	
	data.table(fips_codes) %>%
	.[,.(state, state_code)] %>%
	distinct()


hp_counties_sf <- 
	tigris::counties() %>%
	setnames(names(.), tolower(names(.))) %>%
	st_transform(crs=st_crs(hp_bd)) %>%
	# select counties that are intersected with Ogallala 
	.[hp_bd, ] %>%
	# add state names
	left_join(., fips_codes_mod, by = c("statefp"="state_code")) %>%
	mutate(
    total_area_acres = conv_unit(aland+awater, "m2", "acre"),
		county_id = seq_len(nrow(.))
		)


# Check
tm_shape(hp_counties_sf)+
	tm_borders()+
tm_shape(hp_bd)+
	tm_polygons(col="blue", alpah=0.5)



#/*--------------------------------*/
#' ## 1.1 Download CDL data
#/*--------------------------------*/


# for (y in 2008:2020){
# 	# y=2008
#   	GetCDLData(
#   		aoi = hp_counties_sf, 
#   		year = y,
#   		type = "b",
#   		save_path = here(paste0("Shared/Data/CDL/hp_counties_cdl_", y, ".tif")),
#     	readr = FALSE,
#   		tol_time = 180
# 		)
# }



#/*--------------------------------*/
#' ## 1.2 Extracting CDL values to each county polygon
#/*--------------------------------*/
# NOTE: Error (vector memory exhausted) occurs when extracting cdl values to an entire region at once.
# 


# === Functions to extract cdl values to a county polygon === #
get_cdl_value <- function(cdl_raster, tg_sf){
  # cdl_raster=temp_cdl; tg_sf=cases$temp_county_sf[[1]]
  return <- exact_extract(
          cdl_raster, 
          tg_sf,
          progress = FALSE
          ) %>%
        rbindlist() %>%
        # calculate land share of each land-use category 
        .[, .(coverage_fraction = sum(coverage_fraction)), by = .(value)] %>%
        .[, share := coverage_fraction/sum(coverage_fraction)] %>%
        .[, county_id := tg_sf$county_id,]

  return(return)
}



# === looping over years === #
cases <- tibble(
  temp_county_sf = split(hp_counties_sf, f = hp_counties_sf$county_id) 
  )


cdl_value_counties_raw <- 
  lapply(2008:2020,
    function(y){
      print(paste0("working on", y))
      # y=2010
      # Given a cdl layer of a specific year, extract CDL values by county
      temp_cdl <- 
        rast(here(paste0("Shared/Data/CDL/", "hp_counties_cdl_", y, ".tif")))

      res_cdl_single_year <- 
      cases %>%
      rowwise() %>%
      mutate(
        raw_cdl_value = list(
          get_cdl_value(
            cdl_raster = temp_cdl,
            tg_sf = temp_county_sf
          )
        )
      ) %>%
      ungroup() %>%
      select(!temp_county_sf) %>%
      unnest(., cols= "raw_cdl_value") %>%
      data.table() %>%
      .[, year:= y]
      # .[, year:= paste0("cdl",y)]

      return(res_cdl_single_year)
    }
  ) %>%
  rbindlist()


data("linkdata")
res_cdl_value_counties_raw <- linkdata[cdl_value_counties_raw, on = "MasterCat==value"]


saveRDS(res_cdl_value_counties_raw, here("Shared/Data/CDL/res_cdl_value_counties_raw.rds"))




# /*===========================================*/
#'= 2. Data processing  =
# /*===========================================*/
# **Objective**: 
#' (1) Calculate the proportion of 1)Corn lands and 2)Soybean lands 
#' to the entire area of ag-land by county 

# /*===== CDL value raw data =====*/
res_cdl_value_counties_raw <- 
  readRDS(here("Shared/Data/CDL/res_cdl_value_counties_raw.rds"))


# /*===== Find MasterCat numbers for Non-ag land  =====*/
res_cdl_value_counties_raw[,.N, by = .(year, Crop, MasterCat)] %>%
  dcast(MasterCat+Crop ~ year, value.var = "N") %>%
  .[order(MasterCat)] %>%
  gt()


ls_nonag_MasterCat <- 
  c(
  	0, # NoData
    63, # Forest
  	111, # Open_Water
  	112, # Perennial_Ice/Snow_
    121, # Developed/Open_Space
    122, # Developed/Low_Intensity
    123, # Developed/Med_Intensity
    124, # Developed/High_Intensity 
    131, # Barren
    141, # Deciduous_Forest
    142, #Evergreen_Forest
    143 #Mixed_Forest
    ) 



# === Total share of Ag-lands by county === #
county_Agland <- 
  res_cdl_value_counties_raw %>%
  .[!(MasterCat %in% ls_nonag_MasterCat)] %>%
  # Get the proportions of Ag lands to entire county areas 
  .[,.(ag_share = sum(share)), by = .(county_id, year)] 


# === Corn and Soybean lands by county === #
county_corn_soy <- 
  res_cdl_value_counties_raw %>%
  .[Crop %in% c("Corn", "Soybeans"),] %>%
  # Get the proportion of corn lands and soybean lands to entire county area 
  .[,.(share = sum(share)), by = .(Crop, county_id, year)] %>%
  dcast(county_id+year ~ Crop, value.var="share") %>%
  setnames(c("Corn", "Soybeans"), paste0(c("corn", "soybeans"), "_share"))



# === Get proportions of corn and soybean lands to the Ag lands by County === #
hp_counties_dt <-
  st_drop_geometry(hp_counties_sf) %>%
  data.table() %>%
  .[,.(county_id, statefp, countyfp, name, state, total_area_acres)]


county_Agland_corn_soy <- 
  # --- Merge county_corn_soy with county_Agland --- #
  county_corn_soy[county_Agland, on=c("county_id", "year")] %>%
  imputeTS::na_replace(fill = 0) %>%
  # --- Add information about counties --- #
  hp_counties_dt[, on = "county_id"] %>%
  # --- Calculation --- #
  .[,`:=`(
    # Proportions to Ag-land areas
    corn_share_ag = corn_share/ag_share,
    soybeans_share_ag = soybeans_share/ag_share,
    # Acres of Ag-lands, corn-lands and soybean-lands
    agland_acres = ag_share*total_area_acres,
    corn_acres = corn_share*total_area_acres,
    soybeans_acres = soybeans_share*total_area_acres
    )] %>%
  .[,`:=`(
    corn_share = NULL,
    soybeans_share = NULL,
    ag_share = NULL
    )]


saveRDS(county_Agland_corn_soy, here("Shared/Data/CDL/county_Agland_corn_soy.rds"))




# /*===========================================*/
#'=  Visualization =
# /*===========================================*/
test_sf <- 
  left_join(select(hp_counties_sf, county_id), county_Agland_corn_soy, by="county_id")

y=2015
# Corn
test_sf %>%
  filter(year==y) %>%
  tm_shape(.)+
    tm_polygons(col="corn_share_ag", style ="cont")

# Soy 
test_sf %>%
  filter(year==y) %>%
  tm_shape(.)+
    tm_polygons(col="soybeans_share_ag", style ="cont")
