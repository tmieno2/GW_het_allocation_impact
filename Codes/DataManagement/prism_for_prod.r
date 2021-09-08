######################################
# Prism Data
######################################
# written by Taro Mieno on 08/04/2016

#===================================
# Preparation
#===================================
setwd('~/Dropbox/CollaborativeResearch/AllocationImpacts')
source('~/Dropbox/R_libraries_load/library.R')
library(prism)

#--------------------------
# Import datasets
#--------------------------
#--- Section data ---#
RRB_sec <- readRDS('./Data/Geographic/RRB_sec.rds') 

RRB_sec@data <- RRB_sec@data %>% 
	mutate(plss=paste(twnshp,twsp_SN,rng,rng_WE,section_,sep='')) %>% 
	data.table()

#--- Registration data ---#
rgs <- readRDS(,file='~/Dropbox/MyResearch/GW_strategic/Data/registration.rds')

#--- template prism data ---#
# weather <- readGDAL('/Volumes/data_storage/Data/PRISM_ppt_stable_4kmD2_20070401_bil/PRISM_ppt_stable_4kmD2_20070401_bil.bil')

#===================================
# define the area of interest of the prism data
#===================================
#--------------------------
# Define the offset and region
#--------------------------
#--- Find the bounding box of the RRB ---#
# use registration data to find the geographic extent of the data
rrb_bbox <- rgs[,c(min(longdd)-0.2,max(longdd)+0.2,max(latdd)+0.2,min(latdd)-0.2)]
wellid_ls <- rgs[,list(wellid,longdd,latdd)] %>% unique(by='wellid')

#--- Define the offset and region ---#
# offset: Number of rows and columns from the origin (usually the upper left corner) to begin reading from; presently ordered (y,x)
# region: The number of rows and columns to read from the dataset; presently ordered (y,x)
# Notes: offset from the upper left corner!!
weather <- readGDAL('/Users/tmieno2/Dropbox/MyResearch/MeasurementError/Data/PRISM/PRISM_tmean_stable_4kmM2_201207_bil/PRISM_tmean_stable_4kmM2_201207_bil.bil')
weather_offset <- weather@grid@cellcentre.offset
weather_size <- weather@grid@cellsize
weather_dim <- weather@grid@cells.dim

offset_x <- floor((rrb_bbox[1] - weather_offset[1])/weather_size[1])
region_x <- ceiling((rrb_bbox[2] - weather_offset[1])/weather_size[1]) - offset_x 
offset_y <- floor((weather_offset[2]+weather_size[2]*(weather_dim[2]-1) -rrb_bbox[3])/weather_size[2])
region_y <- ceiling((rrb_bbox[3]-rrb_bbox[4])/weather_size[2]) 

#--------------------------
# Define prism ids for the grids over RRB
#--------------------------
weather <- readGDAL('/Users/tmieno2/Dropbox/MyResearch/MeasurementError/Data/PRISM/PRISM_tmean_stable_4kmM2_201207_bil/PRISM_tmean_stable_4kmM2_201207_bil.bil',
	offset=c(offset_y,offset_x),region.dim=c(region_y,region_x))
# weather <- readGDAL('/Volumes/data_storage/Data/PRISM_ppt_stable_4kmD2_20070401_bil/PRISM_ppt_stable_4kmD2_20070401_bil.bil',
# 	offset=c(offset_y,offset_x),region.dim=c(region_y,region_x))
weather@data$band1 <- seq(1,nrow(weather@data))
weather@data$prism_id <- seq(1,nrow(weather@data))
prism_ids <- seq(1,nrow(weather@data))

# saveRDS(weather,'./Data/PRISM/prism_grids_ref.rds')

#===================================
# Put together prism weather for the relevant time periods  
#===================================
#--------------------------
# combine daily prism datasets into one
#--------------------------
var_list <- c('ppt','tmin','tmax')
year_list <- 2002:2009
month_list <- 1:12
day_list <- 1:31

comp_ls <- expand.grid(var_list,year_list,month_list,day_list)
comp_len <- nrow(comp_ls)

prism_combine <- function(i){
	var <- comp_ls[i,1]
	if (var=='ppt'){
		D_type <- 2
	} else{
		D_type <- 1
	}

	c_year <- comp_ls[i,2]
	c_month <- comp_ls[i,3]
	if (c_month<10) {
		month_txt <- paste('0',c_month,sep='')
	} else{
		month_txt <- c_month
	}

	c_day <- comp_ls[i,4]
	if (c_day<10) {
		day_txt <- paste('0',c_day,sep='')
	} else{
		day_txt <- c_day
	}

	# PRISM_ppt_stable_4kmD2_20070401_bil
	d_name <- paste('PRISM_',var,'_stable_4kmD',D_type,'_',c_year,month_txt,day_txt,'_bil',sep='')
	f_name <- paste('PRISM_',var,'_stable_4kmD',D_type,'_',c_year,month_txt,day_txt,'_bil.bil',sep='')
	f_path <- paste('/Volumes/data_storage/Data/',d_name,'/',f_name,sep='')

	weather_temp <- try(readGDAL(f_path,	
		offset=c(offset_y,offset_x),region.dim=c(region_y,region_x)),silent=T)
	if (class(weather_temp)!='try-error'){ # if data importing is successful
		temp_data <- data.table(weather_temp@data)
		temp_data[,prism_id:=prism_ids]
		temp_data[,year:=c_year]
		temp_data[,month:=c_month]
		temp_data[,day:=c_day]
		temp_data[,type:=var]
		return(temp_data)
	} else{
		# do nothing
	} 
}

weather <- mclapply(1:comp_len,prism_combine,mc.cores=6) %>% 
	rbindlist() %>% 
	dcast(weather,prism_id+year+month+day~type,value.var='band1') %>% 
	data.table()

#--- mm to inch ---#
weather[,ppt:=ppt*0.0393701]

#--- calculate GDD ---#
weather[,gdd_daily:=pmax((tmax+tmin)/2-10,0)] 

#--- save ---#
saveRDS(weather,'./Data/PRISM/prism_02_09.rds')

#===================================
# Aggregate daily prism data
#===================================
weather <- readRDS('./Data/PRISM/prism_02_09.rds')

# #--------------------------
# # monthly data
# #--------------------------
# #--- monthly mean ---#
# weather_m <- weather[,.(precip=sum(ppt),tmin=mean(tmin,na.rm=TRUE),tmax=mean(tmax,na.rm=TRUE)),by=.(prism_id,year,month)]

# precip_m <- dcast(weather_m[.(prism_id,year,month,precip)],prism_id+year~month)  
# colnames(precip_m)[3:8] <- paste('precip_',4:9,sep='') 

# tmin_m <- dcast(weather_m[.(prism_id,year,month,tmin)],prism_id+year~month)
# colnames(tmin_m)[3:8] <- paste('tmin_',4:9,sep='') 

# tmax_m <- dcast(weather_m[.(prism_id,year,month,tmax)],prism_id+year~month)
# colnames(tmax_m)[3:8] <- paste('tmax_',4:9,sep='') 

# weather_m <- cbind(precip_m,tmin_m[,3:8],tmax_m[,3:8]) %>% data.table() 
# saveRDS(weather_m,file='./Data/PRISM/prism_m.rds')

#--------------------------
# yearly data
#--------------------------
prod_m <- 4:9

#--- yearly (production season) ---#
weather_in <- weather[month %in% prod_m,.(
	precip_in=sum(ppt),
	tmin_in=mean(tmin,na.rm=TRUE),
	tmax_in=mean(tmax,na.rm=TRUE),
	gdd_in=sum(gdd_daily,na.rm=TRUE)
	),by=.(prism_id,year)] %>% setkey(prism_id,year)

# saveRDS(weather_y_in,file='./Data/PRISM/prism_y_in.rds')

#--- yearly (off season) ---#
# 10-12 previous years and 1-3 this year
year_list <- 2002:2009
weather_off_store <- list()
for (i in 1:length(year_list)){
	c_year <- year_list[i]

	weather_off_temp <- weather[
	(year < c_year & month >=10) | 
	(year == c_year & month <=3),
	.(
	precip_off=sum(ppt),
	tmin_off=mean(tmin,na.rm=TRUE),
	tmax_off=mean(tmax,na.rm=TRUE)
	),by=.(prism_id)]
	weather_off_temp[,year:=c_year]

	weather_off_store[[i]] <- weather_off_temp
}
weather_off <- rbindlist(weather_off_store) %>% 
	setkey(prism_id,year)

weather_y <- weather_in[weather_off] 

saveRDS(weather_y,file='./Data/PRISM/prism_y.rds')

#===================================
# Identify over-lapping prism grids by section
#===================================
# 1. convert the prism grids sg to spol as sg is not compatible with
# 	 gIntersection().
# 2. discard sections that are not going to be used for regression 
#    to cut down computation time when sections and prism grids are 
#		 intersected.
# 3. for each section identify prism grids it overlaps with

#--------------------------
# Prepare prism grids for clipping  
#--------------------------
#--- convert prism grids to SPolDF ---#
library(Grid2Polygons)
weather_spoldf <- Grid2Polygons(weather,zcol=2)
spoldf_ids <- getSpPPolygonsIDSlots(weather_spoldf)

#--- change the projection method ---#
weather_spoldf <- spTransform(weather_spoldf,RRB_sec@proj4string)

#--------------------------
# identify relevant sections
#--------------------------
# Only some of the sections are used in regression analysis later.
# Merge with the main dataset so you can use the 10 miles buffer dummies
# to identify only the wells that are relevant  
prod_RRB <- readRDS('./Data/Production/prod_RRB.rds')

nrd_comb_ls <- c('Low_Mid','Low_Tri','Upp_Mid','Mid_Tri')
nrd_comb_len <- length(nrd_comb_ls)

rel_sec_ls <- list()
for (i in 1:nrd_comb_len){
	#--- identify the sections that are relevant ---#
	sec_ls <- prod_RRB %>% 
		filter_(paste(nrd_comb_ls[i],'_10mi==1',sep='')) %>% 
		distinct(plss) %>% 
		data.table()

	#--- subset to only the relevant sections ---#
	rel_sec_index <- RRB_sec@data[,plss %in% sec_ls[,plss]]
	rel_sec <- RRB_sec[rel_sec_index,]
	rel_sec_ls[[i]] <- rel_sec
}

#--------------------------
# which prism grids?
#--------------------------
which_prism <- function(i,rel_sec,prism_spoldf){
	print(i)

	temp_sec <- rel_sec[i,]
	bbox(temp_sec)
	temp_int <- gIntersection(prism_spoldf,temp_sec, byid=TRUE)
	# getSpPPolygonsIDSlots(temp)
	temp_prism_id <- gsub('X','',gsub( ' .*$', '',getSpPPolygonsIDSlots(temp_int))) %>% as.numeric
	temp_area <- gArea(temp_int,byid=TRUE)

	temp_data <- data.table(
		plss=temp_sec@data$plss,
		prism_id=temp_prism_id,
		area=temp_area
		)  

	return(temp_data)
}


all_plss_prism <- list()
for (i in 1:nrd_comb_len){
	#--- working sections ---#
	rel_sec <- rel_sec_ls[[i]]
	sec_len <- length(rel_sec)

	#--- clips out prism grids that overlap ---#
	prism_temp <- weather_spoldf[rel_sec,]

	#--- identify prism ids ---#
	plss_prism <- mclapply(1:sec_len,function(x) which_prism(x,rel_sec,prism_temp),mc.cores=6) %>% 
		rbindlist()
	all_plss_prism[[i]] <- plss_prism
}
plss_prism <- rbindlist(all_plss_prism)

saveRDS(plss_prism,file='./Data/plss_prism.rds')

#===================================
# area-weighted weather for each section
#===================================
plss_prism <- readRDS('./Data/plss_prism.rds') %>% 
	mutate(prism_id=as.numeric(prism_id)) %>% 
	data.table() %>% 
	setkey(prism_id)  

prism <- readRDS('./Data/PRISM/prism_y.rds') %>% 
	setkey(prism_id)

prism_plss <- prism[plss_prism]

var_ls <- c('precip_in','tmin_in','tmax_in','gdd_in','precip_off','tmin_off','tmax_off')
expr <- paste(var_ls,'=sum(area*',var_ls,')/sum(area)',sep='',collapse=',')
prism_plss_final <- eval(parse(text=paste('prism_plss[,.(',expr,'),by=.(plss,year)]',sep="")))

saveRDS(prism_plss_final,file='./Data/plss_prism_final.rds')





