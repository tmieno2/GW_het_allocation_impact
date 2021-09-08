######################################
# Identify which wells in the LRNRD fall on the east or west side of the US highway 183
######################################
# Taro Mieno 03/29/2017
 
#===================================
# Preparation
#===================================
#--- Install/Load packages with pacman ---#
source('~/Dropbox/R_libraries_load/library.R')

#--- set wd ---#
setwd('~/Dropbox/CollaborativeResearch/AllocationImpacts')

#--------------------------
# Import datasets  
#--------------------------
#--- LRNRD  ---#
NRD_LR <- st_read(dsn = './Data/Geographic/NRDUTM', 'NRDUTM') %>% 
  filter(NRD_Name=='Lower Republican')  

#--- Registration ---#
rgs <- readRDS('~/Dropbox/NebraskaWaterProjectsData/Data/WellRegistrationRaw/registration.rds') %>% 
	SpatialPointsDataFrame( # create SpatialPointsDataFrame
  	coords=.[,list(longdd,latdd)], 
  	data=., 
  	proj4string=CRS("+proj=longlat")
  ) %>% # convert to an sf object
  st_as_sf() %>% 
	filter(nrdname=='Lower Republican') %>% 
	st_transform(st_crs(NRD_LR))  
st_crs(rgs) <- st_crs(NRD_LR)

#--- highway 183 ---#
hw183_buf <- readRDS(file='./Data/Geographic/ne_roads/hw183_LRNRD.rds') %>% 
  st_as_sf() %>% 
  st_buffer(dist=0.0001) %>% 
  st_transform(st_crs(NRD_LR))  
st_crs(hw183_buf) <- st_crs(NRD_LR)

#===================================
# Identify which side each well is in
#===================================
#--------------------------
# split the LR NRD into two 
#--------------------------
split_LR <- gDifference(as(NRD_LR, "Spatial"),as(hw183_buf, "Spatial")) %>% 
  disaggregate() %>% # disaggregate the polygon into two
  st_as_sfc 
st_crs(split_LR) <- st_crs(NRD_LR)

#++++++++++++++++
# which in the east side
#++++++++++++++++
in_east_ls <- rgs[split_LR[1],] %>% 
	select(wellid) %>% 
	data.table() %>% 
	.[,geometry:=NULL] %>% 
	.[,in_east:=1] %>%   
	.[,in_west:=0]    

#++++++++++++++++
# which in the west side
#++++++++++++++++
in_west_ls <- rgs[split_LR[2],] %>% 
	select(wellid) %>% 
	data.table() %>% 
	.[,geometry:=NULL] %>% 
	.[,in_east:=0] %>%   
	.[,in_west:=1]    

#++++++++++++++++
# combine
#++++++++++++++++
east_or_west <- rbind(in_east_ls,in_west_ls)	

#--------------------------
# save
#--------------------------
saveRDS(east_or_west,'./Data/east_or_west.rds')


