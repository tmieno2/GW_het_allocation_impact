# ==========================================================================
# Get climatic variables (written based on Taro's code "0_5_get_weather.R" of DroughtRiskAquifer)
# ==========================================================================
# objective:
# + Download gridMET Dataset 
# + extract gridMET values to well-level point data
#   * only for in-season (April to September)
#   * Summaries all weather variables by years
# + merge with the original dataset

# /*===== Preparation ======*/
library(here)

# === load packages === #
source(here("GitControlled/Codes/0_1_ls_packages.R"))

# === load functions === #
source(here("GitControlled/Codes/0_0_functions.R"))



# /*=================================================*/
#' # Download gridMET data (2007-2015)
# /*=================================================*/

# /*----------------------------------*/
#' ## Set up parameters
# /*----------------------------------*/

# === list of weather variables ===#
var_ls <-
  c(
  	"pr", # precipitation -> sum
    "tmmn", # tmin (Kelvin) -> mean
    "tmmx", # tmax (Kelvin) -> mean
    "pet" # et -> sum
  )

# === list of years ===#
year_ls <- 2007:2015

# === var-year data ===#
par_data <-
  expand.grid(
    var_name = var_ls,
    year = year_ls
  ) %>%
  data.table() %>%
  .[, var_name := as.character(var_name)]

# /*----------------------------------*/
#' ## Download gridMET data
# /*----------------------------------*/

# lapply(
#   seq_len(nrow(par_data)),
#   function(x) get_grid_MET(par_data[x, var_name], par_data[x, year])
# )

# /*=================================================*/
#' # Extract Rasters to Points
# /*=================================================*/

#/*----------------------------------*/
#' ## Preparation
#/*----------------------------------*/

# /*===== sample gridMET ======*/
sample_gmet <- 
  here("Shared/Data/gridMET-historical/pet_2015.nc") %>%
  terra::rast()

# library(raster)
# test <- 
#   here("Shared/Data/gridMET-historical/pet_2015.nc") %>%
#   raster::brick()

# plot(sample_gmet[[1]]) 

gmet_crs <- terra::crs(sample_gmet)

# library(ncdf4)
# tmmx_2010.nc <- ncdf4::nc_open(here("Shared/Data/gridMET-historical/tmmx_2010.nc"))
# head(names(sample_gmet))
# ymd("1900-01-01") + 40177 # "2010-01-01"
# (ymd("2010-01-01") - ymd("1900-01-01")) %>% as.numeric

pr <- ncdf4::nc_open(here("Shared/Data/gridMET-historical/pr_2010.nc"))
pet <- ncdf4::nc_open(here("Shared/Data/gridMET-historical/pet_2010.nc"))

# /*===== point data for well location  ======*/
data_w_LR_TB <- readRDS(here("Shared/Data/WaterAnalysis/data_w_LR_TB_nw.rds"))

# /*---- wellid data with ID ----*/
unique_well_dt <- data_w_LR_TB %>%
  unique(.,by=c("latdd", "longdd")) %>%
  .[,.(latdd, longdd)] %>%
  .[,ID:=seq_len(nrow(.))]

unique_wells_loc_sf <-
  st_as_sf(unique_well_dt, coords = c("longdd","latdd"), crs = 4269) %>%
  st_transform(crs=gmet_crs)

#/*----------------------------------*/
#' ## Extract values
#/*----------------------------------*/

# /*===== get in-season daily weather data =====*/
daily_gmet <- lapply(seq_len(nrow(par_data)),
  function(x) get_in_values_gridMET(
    var_name = par_data[x, var_name],
    year = par_data[x, year]
    )
  ) %>%
  rbindlist()
  


# === Convert units and get GDD === #
#' Units
#' pr: mm
#' 1 mm of rain refers to the "depth" of rain that would be received in 1 m^2.
#' 1 mm of rain = every square metre receives 1 litre of rain water

#' pet: mm
#'  

res_daily_gmet <- daily_gmet %>%
  dcast(ID + year + day ~ varibale, value.name = "value") %>%
  .[,`:=` (
  # --- pr: mm to inch--- #
    pr = pr * 0.0393701,
    # --- tmmn, tmmx: K to C --- #
    # tmmn = (tmmn - 273.15)*9/5 + 32, #(to F)
    # tmmx = (tmmx - 273.15)*9/5 + 32, #(to F)
    tmmn = tmmn - 273.15, #(to C)
    tmmx = tmmx - 273.15, #(to C)
  # --- pet: mm to inch --- #
    pet = pet * 0.0393701
    )] %>%
  # --- calculate daily GDD--- #  
  # .[,gdd :=  pmax((pmin(tmmx, 86) + pmax(tmmn, 50))/2 - 50, 0)] #(F version)
  .[,gdd :=  pmax((pmin(tmmx, 30) + pmax(tmmn, 10))/2 - 10, 0)] #(C version)




# /*===== in-season yearly weather data =====*/
res_in_gmet <- res_daily_gmet %>%
  .[,.(
    pr_in = sum(pr),
    tmmn_in = mean(tmmn),
    tmmx_in = mean(tmmx),
    pet_in = sum(pet),
    gdd_in = sum(gdd)
    ),
    by = .(ID, year)] %>%
    unique_well_dt[., on = "ID"]

saveRDS(res_in_gmet, here("Shared/Data/WaterAnalysis/gridMET_ready.rds"))





