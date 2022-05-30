# ==========================================================================
# Create "in_5mi" and "in_10mi" columns 
# ==========================================================================


# /*===== library =====*/
library(here)
source(here("GitControlled/Codes/0_1_ls_packages.R"))

crs <- 4269


# /*===========================================*/
#'=  Load Data and Preparation=
# /*===========================================*/

# --- buffer data --- #
bd_buffer_5 <- 
	readRDS(here("Shared/Data/WaterAnalysis/bd_buffer_5.rds")) %>%
	st_transform(crs)

bd_buffer_10 <- 
	readRDS(here("Shared/Data/WaterAnalysis/bd_buffer_10.rds")) %>%
	st_transform(crs)

# --- NRD boundary --- #
nrd_bound <- 
    here("Shared/Data/WaterAnalysis/NRD_bd/BND_NaturalResourceDistricts_DNR.shp") %>%
    st_read() %>%
    filter(NRD_Name %in% c("Lower Republican", "Tri-Basin")) %>%
    st_transform(crs) # WGS UTM 14 (Cartesian 2D CS covering NE)


# /*===== Well data =====*/
ir_data_source <- 
	here('Shared/Data/ir_reg.rds') %>%
	readRDS() %>%
	filter(source %in% c('Meter','METER','metered')) %>%
	filter(nrdname %in% c("Lower Republican", "Tri-Basin")) %>%
	mutate(
		trs=paste(twnid,rngid,section,sep='_'),
		tr=paste(twnid,rngid,sep='_')
		) %>%
	data.table() %>%
	.[,phase1:=ifelse(year<2008,1,0)] %>%
	.[,phase2:=ifelse(year>=2008,1,0)]


unique_wells_loc_sf <-
	ir_data_source %>%
	unique(.,by=c("latdd", "longdd")) %>%
	.[,.(wellid, latdd, longdd)] %>%
	st_as_sf(., coords = c("longdd","latdd"), crs = crs)


# /*===========================================*/
#'=  Create index: in_5mi and in_10mi =
# /*===========================================*/
# === wells located within in 5 miles buffer  === #
wells_5miles <- 
	st_filter(unique_wells_loc_sf, bd_buffer_5) %>%
	.$wellid

# === wells located within in 5 miles buffer  === #
wells_10miles <- 
	st_filter(unique_wells_loc_sf, bd_buffer_10) %>%
	.$wellid

# === Create "in_5mi" and "in_10mi" columns=== #
ir_data <- 
	ir_data_source %>%
	.[,`:=`(
		in_5mi = ifelse(wellid %in% wells_5miles, 1, 0),
		in_10mi = ifelse(wellid %in% wells_10miles, 1, 0)
	)]


saveRDS(ir_data, here("Shared/Data/WaterAnalysis/ir_data_nw.rds"))



# === check === #
ir_data_sf <- ir_data %>%
	.[nrdname %in% c("Lower Republican", "Tri-Basin")] %>%
	unique(.,by=c("latdd", "longdd")) %>%
	st_as_sf(., coords = c("longdd","latdd"), crs = crs)

ggplot()+
	geom_sf(data=nrd_bound) +
	geom_sf(data=bd_buffer_5, fill="green", alpha=0.6)+
	geom_sf(data=ir_data_sf, aes(color=in_5mi), size=0.1)

ggplot()+
	geom_sf(data=nrd_bound) +
	geom_sf(data=bd_buffer_10, fill="green", alpha=0.6)+
	geom_sf(data=ir_data_sf, aes(color=in_10mi), size=0.1)




