######################################
# Prepare the irrigation dataset ready for regression
######################################
# Taro Mieno 11/22/2016
# 	+ Modified the organization of the codes
#
# Related R files that precedes:
# 1. rrb_buffer_identify.R: 
#		1.1 create buffers around the NRD borders
#		1.2 identify whether wells are located within the created buffers
# 2. ssurgo.R:
#		2.1 donwload SSURGO data 
#		2.2 identify which map unit each well falls within 
#		2.3 associate each well with soil characteristics 
# 
# Notes:
# 1. weather data is from PRISM. It is managed by 
# 	 "/Users/tmieno2/Dropbox/MyResearch/GW_strategic/Code/DataManagement/prism.r" 
# 2. combined with the irrigation data by
# 	 "/Users/tmieno2/Dropbox/MyResearch/GW_strategic/Code/DataManagement/merge_data.R" 
#
# Modified on Taro Mieno 04/07/2017
#   

#===================================
# Preparation
#===================================
#--- Install/Load packages with pacman ---#
source('~/Dropbox/R_libraries_load/library.R')

#===================================
# Import and merge data from NebraskaWaterProjectsData 
#===================================
setwd('~/Dropbox/NebraskaWaterProjectsData')

#--------------------------
# Import datasets  
#--------------------------
#--- Pumping ---#
pump_comp <- readRDS('./Data/Pumping/Processed/pumping.rds') %>% 
	setkey(wellid)

#--- Registration ---#
rgs <- readRDS('./Data/WellRegistrationRaw/registration.rds') %>% 
	setkey(wellid)

#--- PRISM ---#
weather_y <- readRDS('./Data/PRISM/prism_y_81_16.rds') %>% 
	setkey(prism_id,year)  
	
well_to_grid <- readRDS('./Data/PRISM/well_to_grid.rds') %>% 
	setkey(wellid)

#--- Set working directory ---#
setwd('~/Dropbox/CollaborativeResearch/AllocationImpacts')

#--- highway 183  ---#
east_or_west <- readRDS('./Data/east_or_west.rds') %>% 
	setkey(wellid)


#--------------------------
# Merge
#--------------------------
#--- registration ---#
data <- rgs[pump_comp]

# find wells in the pumping records that do not have any 
# records in the registration data 
data[is.na(latdd),wellid] %>% unique()
data <- data[!is.na(latdd),]

#--- weather ---#
data <- well_to_grid[data] %>% setkey(prism_id,year)
data <- weather_y[data] %>% setkey(wellid,year)

#--- historical weather for matching ---#
h_weather_82_02 <- weather_y[year<=2002,.(
	h_precip_in_82_02 = mean(precip_in),
	h_precip_off_82_02 = mean(precip_off),
	h_tmin_in_82_02 = mean(tmin_in),
	h_tmin_off_82_02 = mean(tmin_off),
	h_tmax_in_82_02 = mean(tmax_in),
	h_tmax_off_82_02 = mean(tmax_off),
	h_gdd_in_82_02 = mean(gdd_in)
	),by=.(prism_id)] 

h_weather_82_06 <- weather_y[year<=2006,.(
	h_precip_in_82_06 = mean(precip_in),
	h_precip_off_82_06 = mean(precip_off),
	h_tmin_in_82_06 = mean(tmin_in),
	h_tmin_off_82_06 = mean(tmin_off),
	h_tmax_in_82_06 = mean(tmax_in),
	h_tmax_off_82_06 = mean(tmax_off),
	h_gdd_in_82_06 = mean(gdd_in)
	),by=.(prism_id)] 

h_weather_82_15 <- weather_y[,.(
	h_precip_in_82_15 = mean(precip_in),
	h_precip_off_82_15 = mean(precip_off),
	h_tmin_in_82_15 = mean(tmin_in),
	h_tmin_off_82_15 = mean(tmin_off),
	h_tmax_in_82_15 = mean(tmax_in),
	h_tmax_off_82_15 = mean(tmax_off),
	h_gdd_in_82_15 = mean(gdd_in)
	),by=.(prism_id)] 

h_weather_08_15 <- weather_y[year>=2008,.(
	h_precip_in_08_15 = mean(precip_in),
	h_precip_off_08_15 = mean(precip_off),
	h_tmin_in_08_15 = mean(tmin_in),
	h_tmin_off_08_15 = mean(tmin_off),
	h_tmax_in_08_15 = mean(tmax_in),
	h_tmax_off_08_15 = mean(tmax_off),
	h_gdd_in_08_15 = mean(gdd_in)
	),by=.(prism_id)] 

data <- data %>% 
	left_join(.,h_weather_82_02,by='prism_id') %>% 
	left_join(.,h_weather_82_06,by='prism_id') %>% 
	left_join(.,h_weather_82_15,by='prism_id') %>% 
	left_join(.,h_weather_08_15,by='prism_id') %>% 
	data.table()

#--- east or west of highway 183 ---#
data <- left_join(data,east_or_west,by='wellid') %>% 
	mutate(
		in_east=ifelse(is.na(in_east),0,in_east),
		in_west=ifelse(is.na(in_west),0,in_west)
		) %>% 
	data.table()

#--------------------------
# Save as an intermediate dataset
#--------------------------
# this data set is used to create buffers and well-buffer identification
saveRDS(data,'./Data/merged_data.rds')

#===================================
# Merge data from the current project
#===================================
data <- readRDS('./Data/merged_data.rds')

#--------------------------
# Buffers
#--------------------------
#--- Create and manage buffers ---#
# source('./Code/DataManagement/rrb_buffer_identify.R')

#--- merge ---#
buf_id <- readRDS('./Data/Geographic/NRD_buffers/buffer_identified.rds') 

#--- with buffer dummies ---#
data <- left_join(data,buf_id,by='wellid') %>% 
	data.table()

#--------------------------
# SSURGO
#--------------------------
#--- generate SSURGO data ---#
# this operation depends on ir_buffers.rds
# source('./Code/DataManagement/ssurgo.R')

#--- merge ---#
well_soil <- readRDS('./Data/ssurgo_soil.rds')  

#--- merge ---#
data <- left_join(data,well_soil,by='wellid') %>% 
	data.table()

#===================================
# Define some variables 
#===================================
#--- ir/acre ---#
data[,usage:=volaf*12/acres]

#--- The township in Phase 3 (Tri-Basin) ---#
# Gosper County: township 5, range 22, all sections.
data[,t5r22:=ifelse(twnid==5 & rngid==22,1,0)]
data[,Phase3:=ifelse(twnid==5 & rngid==22 & year>=2009,1,0)]

#--- the neighboring townships ---#
# township 5, range 23
# township 5, range 21
# township 6, range 22
# township 6, range 23
# township 6, range 21
data[,t5r22_neighbors:=ifelse(
	(twnid==5 & rngid==21) |
	(twnid==5 & rngid==23) |
	(twnid==6 & rngid==21) |
	(twnid==6 & rngid==22) |
	(twnid==6 & rngid==23) 
	,1,0
	)]

#===================================
# Save
#===================================
#--- as an RDS file ---#
saveRDS(data,file='./Data/ir_reg.rds')

#--------------------------
# as a stata file
#--------------------------
library(readstata13)

#--- drop county ---#
# county variable is causing error when saving 
data[,county:=NULL]

#--- save ---#
save.dta13(data,file='./Data/ir_reg.dta')





