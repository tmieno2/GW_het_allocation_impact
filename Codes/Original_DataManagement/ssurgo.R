######################################
# Download SSURGO datasets
######################################
# James Keeler 11/1/2016
#   + Derivative of rrb_buffer.R by Taro Mieno
#   + Needs more documentation and testing
# Taro Mieno 11/10/2016
#   + Modified the organization of the codes

#===================================
# Preparation
#===================================
#--- Install/Load packages with pacman ---#
source('~/Dropbox/R_libraries_load/library.R')
library(FedData)

#--- Set working directory ---#
setwd('~/Dropbox/CollaborativeResearch/AllocationImpacts')

#===================================
# Which well in which soil polygon?
#===================================
# Strategy:
# 1. clip relevant soil data by buffer
# 2. for each well within a buffer, identify soil map unit key

#--------------------------
# Import datasets
#--------------------------
#--- relevant wells ---#	
buf_ids <- readRDS('./Data/Geographic/NRD_buffers/buffer_identified.rds') %>% 
	filter(
		Low_Mid_10mi==1 | Low_Tri_10mi==1 | Mid_Tri_10mi==1 | Upp_Mid_10mi==1 
		) %>% 
	select(wellid) %>% 
	unlist()

#--- relevant wells from the well registration data ---#
rgs <- readRDS('~/Dropbox/NebraskaWaterProjectsData/Data/WellRegistrationRaw/registration.rds') %>% 
	select(wellid,longdd,latdd) %>% 
	filter(
		wellid %in% buf_ids
		) %>% 
	data.table()

#--- as sf ---#
rgs_sf <- SpatialPointsDataFrame(
	coords=rgs[,.(longdd,latdd)], 
  data=rgs, 
  proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs")
	) %>% 
	spTransform(CRS('+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs')) %>% 
	st_as_sf()

#++++++++++++++++
# Soil
#++++++++++++++++
# Notes: if you transform sp data with data.table as its data component into a sf object,
# numerical indexing would not work
ssurgo_spoldf <- readRDS('~/Dropbox/NebraskaWaterProjectsData/Data/SSURGO/SSURGO_RRB_spoldf.rds') 
ssurgo_spoldf@data <- as.data.frame(ssurgo_spoldf@data)  
ssurgo_spoldf <- spTransform(ssurgo_spoldf,CRS('+proj=utm +zone=14 +datum=NAD83 +units=m +no_defs'))

#--- find bbox for each element ---#	
bbox_series <- purrr::map(ssurgo_spoldf@polygons,bbox) %>% 
  purrr::map(as.vector) %>% 
  purrr::map(t) %>% 
  purrr::map(as.data.table) %>% 
  rbindlist() %>% 
  setnames(names(.),c('xmin','ymin','xmax','ymax'))

#--- spatial feature ---#
ssurgo_sf <- st_as_sf(ssurgo_spoldf)  

#--- soil characteristics data ---#
soil_RRB_mu <- readRDS('./Data/Geographic/SSURGO/soil_RRB_mu.rds') 

#===================================
# Find the area of overlapping mukey 
#===================================
mukey_area_find <- function(i){
	print(i)
	temp_well <- rgs_sf[i,]
	temp_buf <- st_buffer(temp_well,dist=1609/4)
	temp_buf_bbox <- st_bbox(temp_buf)

	index <- bbox_series[,!(
		xmax <= temp_buf_bbox['xmin'] | 
		xmin >= temp_buf_bbox['xmax'] | 
		ymax <= temp_buf_bbox['ymin'] | 
		ymin >= temp_buf_bbox['ymax']
		)] 

	temp_soil <- ssurgo_sf[index,]
  intersected <- st_intersection(temp_buf,temp_soil) %>% 
  	mutate(
  		area=as.numeric(st_area(.))
  		) %>% 
  	data.table() %>% 
  	.[,.(wellid,MUKEY,area)]
  return(intersected)
}

mukey_area <- mclapply(1:nrow(rgs_sf),mukey_area_find,mc.cores=6) %>% 
	rbindlist() 
	  
mukey_area <- mukey_area %>% 
	rename(mukey=MUKEY) %>% 
	mutate(mukey=as.integer(as.character(mukey))) %>% 
	data.table()


soil_data <- left_join(mukey_area,soil_RRB_mu,by='mukey') %>% 
	na.omit() %>% 
	data.table() %>% 
    .[,.(
      sand_pct=sum(sand_pct*area)/sum(area),
      clay_pct=sum(clay_pct*area)/sum(area),
      silt_pct=sum(silt_pct*area)/sum(area),
      kv=sum(kv*area)/sum(area),
      awc=sum(awc*area)/sum(area),
      slope=sum(slope*area)/sum(area)
      )
    ,by=wellid] 

#===================================
# save
#===================================
saveRDS(soil_data,'./Data/ssurgo_soil.rds')

    