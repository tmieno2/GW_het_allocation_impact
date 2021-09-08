######################################
# Create spatial blocks within each buffer
######################################
# written by Taro Mieno on 04/15/2015
# objectives:

#===================================
# 0. Preliminary Operations
#===================================
library(data.table)
library(dplyr)
library(rgdal)
library(foreign)
library(ggplot2)
library(gpclib)
library(rgeos)
library(fields)
setwd("~/Dropbox/MyResearch/WaterUseSmoothing/Data")

#--- import data ---#
buffer <- 20000
load(paste('within_buffer_',buffer,'.RData',sep='')) # rrb

#--- import NRD combinations ---#
load('NRDs_combination.RData') # nrd_comb_list
comb_len <- length(nrd_comb_list)

#--- import buffers ---#
load('buffers.RData') # buffers

#--- import NRD boundaries ---#
nrd_boundary <- readOGR(dsn = ".", "rrb_nrd_boundaries")

#===================================
# Define Functions
#===================================
sp_block_lat <- function(N,bbox,nrd_buf,nrd_comb,var_name){
	eval(parse(text=paste('rrb[,',var_name,':=0]',sep="")))
	divider <- seq(bbox[2,1],bbox[2,2],length=N+1) 
	for (i in 1:N){

		#--- assign spatial block status ---#
		eval(parse(text=paste(
			'rrb[',nrd_comb,'==1 
			& lat >= divider[i] & lat <= divider[i+1]
			& long <= bbox[1,2] & long >= bbox[1,1],'
			,var_name,':=i]'
			,sep='')))
	
		#--- spatial block ---#
		bbox[2,1] <- divider[i]
		bbox[2,2] <- divider[i+1]
		b_poly <- as(extent(bbox),'SpatialPolygons')
		proj4string(b_poly) <- nrd_buf@proj4string
	
		temp_buf <-  gIntersection(nrd_buf, b_poly)
		buf_data <- SpatialPolygonsDataFrame(temp_buf, 
	      data=data.table(sp_id=i))
	  buf_data@polygons[[1]]@ID = paste(i)
	
		if (i==1){
			buf_new <- buf_data
		} else{
			buf_new <- spRbind(buf_new,buf_data)
		}
	}
	return(buf_new)
}

sp_block_lon <- function(N,bbox,nrd_buf,nrd_comb,var_name){
	eval(parse(text=paste('rrb[,',var_name,':=0]',sep="")))
	divider <- seq(bbox[1,1],bbox[1,2],length=N+1) 
	for (i in 1:N){

		#--- assign spatial block status ---#
		eval(parse(text=paste(
			'rrb[',nrd_comb,'==1 
			& long >= divider[i] & long <= divider[i+1]
			& lat <= bbox[2,2] & lat >= bbox[2,1],'
			,var_name,':=i]'
			,sep='')))
	
		#--- spatial block ---#
		bbox[1,1] <- divider[i]
		bbox[1,2] <- divider[i+1]
		b_poly <- as(extent(bbox),'SpatialPolygons')
		proj4string(b_poly) <- nrd_buf@proj4string
	
		temp_buf <-  gIntersection(nrd_buf, b_poly)
		buf_data <- SpatialPolygonsDataFrame(temp_buf, 
	      data=data.table(sp_id=i))
	  buf_data@polygons[[1]]@ID = paste(i)
	
		if (i==1){
			buf_new <- buf_data
		} else{
			buf_new <- spRbind(buf_new,buf_data)
		}
	}
	return(buf_new)
}

res_bd <- function(bbox,nrd_bd){
	b_poly <- as(extent(bbox),'SpatialPolygons')
	proj4string(b_poly) <- nrd_bd@proj4string
	temp_bd_1 <- gIntersection(nrd_bd[1,], b_poly)
	temp_bd_1@polygons[[1]]@ID = '1'
	temp_bd_2 <- gIntersection(nrd_bd[2,], b_poly)
	temp_bd_2@polygons[[1]]@ID = '2'
	bd <- spRbind(temp_bd_1,temp_bd_2)
	return(bd)
}

#===================================
# Lower and Kearney
#===================================
#--------------------------
# create spatial blocks and identify which wells are in each of them
#--------------------------
#--- select relevant nrds ---#
lk_bd <- nrd_boundary[nrd_boundary@data$NRD_Name %in% c('Lower Republican','KEA_Tri'),]
lk_buf <- buffers[buffers@data$id=='Low_KEA',]

#--- define an extended bbox ---#
bb <- lk_buf@bbox
bb[2,1] <- bb[2,1] - 1000
bb[2,2] <- bb[2,2] + 1000
bb[1,1] <- min(rrb[Low_KEA==1 & county == 'KEARNEY',long]) - 200
bb[1,2] <- max(rrb[Low_KEA==1 & county == 'KEARNEY',long]) + 100 

#--- assign spatial block dummies and create block buffers ---#
lk_buf_new <- sp_block_lon(10,bb,lk_buf,'Low_KEA','sp_block_lk')

#--------------------------
# Mapping
#--------------------------
#--- restrict geographic focus ---#
bb_nrd <- bb
bb_nrd[1,1] <- min_long_well_K - 50000
bb_nrd[1,2] <- 2040250
bb_nrd[2,1] <- lk_bd@bbox[2,1] - 1000
bb_nrd[2,2] <- lk_bd@bbox[2,2] + 1000

bd_new <- res_bd(bb_nrd,lk_bd)

#--- centroids ---#
centroids <- gCentroid(bd_new,byid=TRUE)
nrd_name <- coordinates(centroids) %>% data.table()
rownames(nrd_name) <-	c('Lower Republican NRD','Kearney (Tri-Basin)')

#--- mapping ---#
bd_new_f <- fortify(bd_new,region='ID')
lk_buf_new_f <- fortify(lk_buf_new,region='sp_id')
g <- ggplot(data=bd_new_f,aes(x=long,y=lat)) +
	geom_polygon(aes(group=group),fill='white',colour="black",size=0.2) +
	geom_polygon(data=lk_buf_new_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_point(data=rrb[NRD %in% c('Lower Republican','KEARNEY_Tri') & 
		long >= bb_nrd[1,1] & long <= bb_nrd[1,2],],aes(x=long,y=lat),colour='red',size=0.6) +
	geom_point(data=rrb[Low_KEA==1 & sp_block_lk != 0,],aes(x=long,y=lat),colour='blue',size=0.6) +
	geom_text(data=nrd_name,aes(x=x,y=y,label=rownames(nrd_name)),size=4) +
	coord_equal(ratio=1) +
  labs(x="", y="") + #labels
  	theme(
  		axis.ticks.y = element_blank(),
  		axis.text.y = element_blank(), # get rid of x ticks/text
  		axis.ticks.x = element_blank(),
  		axis.text.x = element_blank(),
  		panel.background = element_rect(fill='white'),
      legend.title = element_blank()
  	)
ggsave("~/Dropbox/MyResearch/WaterUseSmoothing/Graphs/by_nrd_pair/Low_KEA.pdf")

#===================================
# Lower and Phelps
#===================================
#--------------------------
# create spatial blocks and identify which wells are in each of them
#--------------------------
#--- select relevant nrds ---#
lp_bd <- nrd_boundary[nrd_boundary@data$NRD_Name %in% c('Lower Republican','PHE_Tri'),]
lp_buf <- buffers[buffers@data$id=='Low_PHE',]

#--- define the extent of longitude ---#
lp_bd_coords <- fortify(lp_bd,region='NRD_Name') %>% data.table()
bb <- lp_buf@bbox
bb[1,1] <- min(lp_bd_coords[id == 'PHE_Tri',long]) - 180
bb[1,2] <- max(lp_bd_coords[id == 'PHE_Tri',long])
bb[2,1] <- bb[2,1] - 1000
bb[2,2] <- bb[2,2] + 1000

#--- assign spatial block dummies and create block buffers ---#
lp_buf_new <- sp_block_lon(15,bb,lp_buf,'Low_PHE','sp_block_lp')

#--------------------------
# Mapping
#--------------------------
#--- restrict geographic focus ---#
bb_nrd <- bb
bb_nrd[1,1] <- bb_nrd[1,1] - 20000
bb_nrd[1,2] <- bb_nrd[1,2] + 20000
bb_nrd[2,1] <- lp_bd@bbox[2,1] - 1000
bb_nrd[2,2] <- lp_bd@bbox[2,2] + 1000

bd_new <- res_bd(bb_nrd,lp_bd)

#--- centroids ---#
centroids <- gCentroid(bd_new,byid=TRUE)
nrd_name <- coordinates(centroids) %>% data.table()
rownames(nrd_name) <-	c('Lower Republican NRD','Phelps (Tri-Basin)')

#--- mapping ---#
bd_new_f <- fortify(bd_new,region='ID')
lp_buf_new_f <- fortify(lp_buf_new,region='sp_id')
g <- ggplot(data=bd_new_f,aes(x=long,y=lat)) +
	geom_polygon(aes(group=group),fill='white',colour="black",size=0.2) +
	geom_polygon(data=lp_buf_new_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_point(data=rrb[NRD %in% c('Lower Republican','PHELPS_Tri') & 
		long >= bb_nrd[1,1] & long <= bb_nrd[1,2],],aes(x=long,y=lat),colour='red',size=0.4) +
	geom_point(data=rrb[Low_PHE==1 & sp_block_lp != 0,],aes(x=long,y=lat),colour='blue',size=0.4) +
	geom_text(data=nrd_name,aes(x=x,y=y,label=rownames(nrd_name)),size=4) +
	coord_equal(ratio=1) +
  labs(x="", y="") + #labels
  	theme(
  		axis.ticks.y = element_blank(),
  		axis.text.y = element_blank(), # get rid of x ticks/text
  		axis.ticks.x = element_blank(),
  		axis.text.x = element_blank(),
  		panel.background = element_rect(fill='white'),
      legend.title = element_blank()
  	)
ggsave("~/Dropbox/MyResearch/WaterUseSmoothing/Graphs/by_nrd_pair/Low_PHE.pdf")

#===================================
# Lower and Gosper
#===================================
#--------------------------
# create spatial blocks and identify which wells are in each of them
#--------------------------
#--- select relevant nrds ---#
lg_bd <- nrd_boundary[nrd_boundary@data$NRD_Name %in% c('Lower Republican','GOS_Tri'),]
lg_buf <- buffers[buffers@data$id=='Low_GOS',]

#--- define the extent of longitude ---#
lg_bd_coords <- fortify(lg_bd,region='NRD_Name') %>% data.table()
bb <- lg_buf@bbox
bb[1,1] <- min(lg_bd_coords[id == 'GOS_Tri',long]) - 200
bb[1,2] <- max(lg_bd_coords[id == 'GOS_Tri',long])
bb[2,1] <- bb[2,1] - 1000
bb[2,2] <- bb[2,2] + 1000

#--- assign spatial block dummies and create block buffers ---#
lg_buf_new <- sp_block_lon(15,bb,lg_buf,'Low_GOS','sp_block_lg')

#--------------------------
# Mapping
#--------------------------
#--- restrict geographic focus ---#
bb_nrd <- bb
bb_nrd[1,1] <- bb_nrd[1,1] - 20000
bb_nrd[1,2] <- bb_nrd[1,2] + 20000
bb_nrd[2,1] <- lg_bd@bbox[2,1] - 1000
bb_nrd[2,2] <- lg_bd@bbox[2,2] + 1000

bd_new <- res_bd(bb_nrd,lg_bd)

#--- centroids ---#
centroids <- gCentroid(bd_new,byid=TRUE)
nrd_name <- coordinates(centroids) %>% data.table()
rownames(nrd_name) <-	c('Lower Republican NRD','Gosper (Tri-Basin)')

#--- mapping ---#
bd_new_f <- fortify(bd_new,region='ID')
lg_buf_new_f <- fortify(lg_buf_new,region='sp_id')
g <- ggplot(data=bd_new_f,aes(x=long,y=lat)) +
	geom_polygon(aes(group=group),fill='white',colour="black",size=0.2) +
	geom_polygon(data=lg_buf_new_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_point(data=rrb[NRD %in% c('Lower Republican','GOSPER_Tri') & 
		long >= bb_nrd[1,1] & long <= bb_nrd[1,2],],aes(x=long,y=lat),colour='red',size=0.4) +
	geom_point(data=rrb[Low_GOS==1 & sp_block_lg != 0,],aes(x=long,y=lat),colour='blue',size=0.4) +
	geom_text(data=nrd_name,aes(x=x,y=y,label=rownames(nrd_name)),size=4) +
	coord_equal(ratio=1) +
  labs(x="", y="") + #labels
  	theme(
  		axis.ticks.y = element_blank(),
  		axis.text.y = element_blank(), # get rid of x ticks/text
  		axis.ticks.x = element_blank(),
  		axis.text.x = element_blank(),
  		panel.background = element_rect(fill='white'),
      legend.title = element_blank()
  	)
ggsave("~/Dropbox/MyResearch/WaterUseSmoothing/Graphs/by_nrd_pair/Low_GOS.pdf")


#===================================
# All Tri-Basin and Lower
#===================================
lt_bd <- nrd_boundary[nrd_boundary@data$NRD_Name %in% c('Lower Republican','KEA_Tri','GOS_Tri','PHE_Tri'),]
lt_bd_f <- fortify(lt_bd,region='NRD_Name')

plot_rrb <- rrb[Low_GOS==1 | Low_PHE==1 | Low_KEA==1,]
plot_rrb <- plot_rrb[sp_block_lg != 0 | sp_block_lp != 0 | sp_block_lk != 0,]

g <- ggplot(data=lt_bd_f,aes(x=long,y=lat)) +
	geom_polygon(aes(group=group),fill='white',colour="black",size=0.2) +
	geom_polygon(data=lk_buf_new_f,aes(x=long,y=lat,group=group),fill='orange',colour="black",alpha=0.4,size=0.3) +
	geom_polygon(data=lp_buf_new_f,aes(x=long,y=lat,group=group),fill='orange',colour="black",alpha=0.4,size=0.3) +
	geom_polygon(data=lg_buf_new_f,aes(x=long,y=lat,group=group),fill='orange',colour="black",alpha=0.4,size=0.3) +
	geom_point(data=rrb[NRD %in% c('Lower Republican','GOSPER_Tri','PHELPS_Tri','KEARNEY_Tri'),],
		aes(x=long,y=lat),colour='red',size=0.4) +
	geom_point(data=plot_rrb,aes(x=long,y=lat),colour='blue',size=0.4) +
	coord_equal(ratio=1) +
  labs(x="", y="") + 
  	theme(
  		axis.ticks.y = element_blank(),
  		axis.text.y = element_blank(), 
  		axis.ticks.x = element_blank(),
  		axis.text.x = element_blank(),
  		panel.background = element_rect(fill='white'),
      legend.title = element_blank()
  	)
ggsave("~/Dropbox/MyResearch/WaterUseSmoothing/Graphs/by_nrd_pair/Low_all.pdf")


#===================================
#  Kearney and Phelps
#===================================
#--------------------------
# create spatial blocks and identify which wells are in each of them
#--------------------------
#--- select relevant nrds ---#
kp_bd <- nrd_boundary[nrd_boundary@data$NRD_Name %in% c('KEA_Tri','PHE_Tri'),]
kp_buf <- buffers[buffers@data$id=='PHE_KEA',]

#--- define an extended bbox ---#
bb <- kp_buf@bbox
bb[1,1] <- bb[1,1] - 2000
bb[1,2] <- bb[1,2] + 2000
bb[2,1] <- min(rrb[PHE_KEA==1,lat]) - 100 
bb[2,2] <- max(rrb[PHE_KEA==1,lat]) + 100 

#--- assign spatial block dummies and create block buffers ---#
kp_buf_new <- sp_block_lat(6,bb,kp_buf,'PHE_KEA','sp_block_kp')

#--------------------------
# Mapping
#--------------------------
#--- centroids ---#
centroids <- gCentroid(kp_bd,byid=TRUE)
nrd_name <- coordinates(centroids) %>% data.table()
rownames(nrd_name) <-	c('Phelps (Tri-Basin)','Kearney (Tri-Basin)')

#--- mapping ---#
kp_bd_f <- fortify(kp_bd,region='NRD_Name')
kp_buf_new_f <- fortify(kp_buf_new,region='sp_id')
g <- ggplot(data=kp_bd_f,aes(x=long,y=lat)) +
	geom_polygon(aes(group=group),fill='white',colour="black",size=0.2) +
	geom_polygon(data=kp_buf_new_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_point(data=rrb[NRD %in% c('KEARNEY_Tri','PHELPS_Tri')], aes(x=long,y=lat),colour='red',size=0.4) +
	geom_point(data=rrb[PHE_KEA==1 & sp_block_kp != 0,],aes(x=long,y=lat),colour='blue',size=0.4) +
	geom_text(data=nrd_name,aes(x=x,y=y,label=rownames(nrd_name)),size=4) +
	coord_equal(ratio=1) +
  labs(x="", y="") + #labels
  	theme(
  		axis.ticks.y = element_blank(),
  		axis.text.y = element_blank(), # get rid of x ticks/text
  		axis.ticks.x = element_blank(),
  		axis.text.x = element_blank(),
  		panel.background = element_rect(fill='white'),
      legend.title = element_blank()
  	)
ggsave("~/Dropbox/MyResearch/WaterUseSmoothing/Graphs/by_nrd_pair/KEA_PHE.pdf")


#===================================
#  Phelps and Gosper
#===================================
#--------------------------
# create spatial blocks and identify which wells are in each of them
#--------------------------
#--- select relevant nrds ---#
gp_bd <- nrd_boundary[nrd_boundary@data$NRD_Name %in% c('GOS_Tri','PHE_Tri'),]
gp_buf <- buffers[buffers@data$id=='GOS_PHE',]

#--- define an extended bbox ---#
bb <- gp_buf@bbox
bb[1,1] <- bb[1,1] - 2000
bb[1,2] <- bb[1,2] + 2000
bb[2,1] <- min(rrb[GOS_PHE==1,lat]) - 100 
bb[2,2] <- max(rrb[GOS_PHE==1,lat]) + 100 

#--- assign spatial block dummies and create block buffers ---#
gp_buf_new <- sp_block_lat(6,bb,gp_buf,'GOS_PHE','sp_block_gp')

#--------------------------
# Mapping
#--------------------------
#--- centroids ---#
centroids <- gCentroid(gp_bd,byid=TRUE)
nrd_name <- coordinates(centroids) %>% data.table()
rownames(nrd_name) <-	c('Phelps(Tri-Basin)','Gosper (Tri-Basin)')

#--- mapping ---#
gp_bd_f <- fortify(gp_bd,region='NRD_Name')
gp_buf_new_f <- fortify(gp_buf_new,region='sp_id')
g <- ggplot(data=gp_bd_f,aes(x=long,y=lat)) +
	geom_polygon(aes(group=group),fill='white',colour="black",size=0.2) +
	geom_polygon(data=gp_buf_new_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_point(data=rrb[NRD %in% c('GOSPER_Tri','PHELPS_Tri')], aes(x=long,y=lat),colour='red',size=0.4) +
	geom_point(data=rrb[GOS_PHE==1 & sp_block_gp != 0,],aes(x=long,y=lat),colour='blue',size=0.4) +
	geom_text(data=nrd_name,aes(x=x,y=y,label=rownames(nrd_name)),size=4) +
	coord_equal(ratio=1) +
  labs(x="", y="") + #labels
  	theme(
  		axis.ticks.y = element_blank(),
  		axis.text.y = element_blank(), # get rid of x ticks/text
  		axis.ticks.x = element_blank(),
  		axis.text.x = element_blank(),
  		panel.background = element_rect(fill='white'),
      legend.title = element_blank()
  	)
ggsave("~/Dropbox/MyResearch/WaterUseSmoothing/Graphs/by_nrd_pair/GOS_PHE.pdf")


#===================================
# All Tri-Basin
#===================================
#--------------------------
# Mapping
#--------------------------
tri_bd <- nrd_boundary[nrd_boundary@data$NRD_Name %in% c('GOS_Tri','PHE_Tri','KEA_Tri'),]

#--- centroids ---#
centroids <- gCentroid(tri_bd,byid=TRUE)
nrd_name <- coordinates(centroids) %>% data.table()
rownames(nrd_name) <-	c('Phelps','Kearney','Gosper')

#--- mapping ---#
plot_rrb <- rrb[GOS_PHE==1 | PHE_KEA==1,]
plot_rrb <- plot_rrb[sp_block_gp != 0 | sp_block_kp != 0,]

tri_bd_f <- fortify(tri_bd,region='NRD_Name')
g <- ggplot(data=tri_bd_f,aes(x=long,y=lat)) +
	geom_polygon(aes(group=group),fill='white',colour="black",size=0.2) +
	geom_polygon(data=gp_buf_new_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_polygon(data=kp_buf_new_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_point(data=rrb[NRD %in% c('GOSPER_Tri','PHELPS_Tri','KEARNEY_Tri')], aes(x=long,y=lat),colour='red',size=0.4) +
	geom_point(data=plot_rrb,colour='blue',size=0.4) +
	geom_text(data=nrd_name,aes(x=x,y=y,label=rownames(nrd_name)),size=4) +
	coord_equal(ratio=1) +
  labs(x="", y="") + #labels
  	theme(
  		axis.ticks.y = element_blank(),
  		axis.text.y = element_blank(), # get rid of x ticks/text
  		axis.ticks.x = element_blank(),
  		axis.text.x = element_blank(),
  		panel.background = element_rect(fill='white'),
      legend.title = element_blank()
  	)
ggsave("~/Dropbox/MyResearch/WaterUseSmoothing/Graphs/by_nrd_pair/all_Tri.pdf")


#===================================
#  Middle and Lower
#===================================
#--------------------------
# create spatial blocks and identify which wells are in each of them
#--------------------------
#--- select relevant nrds ---#
ml_bd <- nrd_boundary[nrd_boundary@data$NRD_Name %in% c('Lower Republican','Middle Republican'),]
ml_buf <- buffers[buffers@data$id=='Low_Mid',]

#--- define the extent of longitude ---#
ml_bd_coords <- fortify(ml_bd,region='NRD_Name') %>% data.table()
bb <- ml_buf@bbox
bb[2,1] <- min(rrb[Low_Mid==1,lat]) - 180 
bb[2,2] <- max(ml_bd_coords[id == 'Lower Republican',lat]) - 2000 
bb[1,1] <- bb[1,1] - 2000
bb[1,2] <- bb[1,2] - 26500

#--- assign spatial block dummies and create block buffers ---#
ml_buf_new <- sp_block_lat(7,bb,ml_buf,'Low_Mid','sp_block_ml')

#--------------------------
# Mapping
#--------------------------
#--- restrict geographic focus ---#
bb_nrd <- ml_buf@bbox
bb_nrd[1,1] <- bb_nrd[1,1] - 120000
bb_nrd[1,2] <- bb_nrd[1,2] + 120000
bb_nrd[2,2] <- bb_nrd[2,2] + 70000

bd_new <- res_bd(bb_nrd,ml_bd)

#--- centroids ---#
centroids <- gCentroid(bd_new,byid=TRUE)
nrd_name <- coordinates(centroids) %>% data.table()
rownames(nrd_name) <-	c('Lower Republican','Middle Republican')

#--- mapping ---#
bd_new_f <- fortify(bd_new,region='NRD_Name')
ml_buf_new_f <- fortify(ml_buf_new,region='sp_id')
g <- ggplot(data=bd_new_f,aes(x=long,y=lat)) +
	geom_polygon(aes(group=group),fill='white',colour="black",size=0.2) +
	geom_polygon(data=ml_buf_new_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_point(data=rrb[NRD %in% c('Lower Republican','Middle Republican') &
		long >= bb_nrd[1,1] & long <= bb_nrd[1,2] & lat <= bb_nrd[2,2]],
		aes(x=long,y=lat),colour='red',size=0.4) +
	geom_point(data=rrb[Low_Mid==1 & sp_block_ml != 0,],aes(x=long,y=lat),colour='blue',size=0.4) +
	geom_text(data=nrd_name,aes(x=x,y=y,label=rownames(nrd_name)),size=4) +
	coord_equal(ratio=1) +
  labs(x="", y="") + #labels
  	theme(
  		axis.ticks.y = element_blank(),
  		axis.text.y = element_blank(), # get rid of x ticks/text
  		axis.ticks.x = element_blank(),
  		axis.text.x = element_blank(),
  		panel.background = element_rect(fill='white'),
      legend.title = element_blank()
  	)
ggsave("~/Dropbox/MyResearch/WaterUseSmoothing/Graphs/by_nrd_pair/Low_Mid.pdf")

#===================================
#  Middle and Upper
#===================================
#--------------------------
# create spatial blocks and identify which wells are in each of them
#--------------------------
#--- select relevant nrds ---#
um_bd <- nrd_boundary[nrd_boundary@data$NRD_Name %in% c('Upper Republican','Middle Republican'),]
um_buf <- buffers[buffers@data$id=='Upp_Mid',]

#--------------------------
# Define the extent of 3 buffers
#--------------------------
um_bd_coords <- fortify(um_bd,region='NRD_Name') %>% data.table()

#--- upper ---#
bb1 <- um_buf@bbox
bb1[2,1] <- 317700 
bb1[2,2] <- max(um_bd_coords[id == 'Middle Republican',lat]) + 100 
bb1[1,1] <- bb1[1,1] + 28300
bb1[1,2] <- bb1[1,2] + 1000

b_poly <- as(extent(bb1),'SpatialPolygons')
proj4string(b_poly) <- um_buf@proj4string
plot(um_bd)
plot(um_buf,add=TRUE,border='red')
plot(b_poly,add=TRUE,border='blue')

#--- middle ---#
bb2 <- um_buf@bbox
bb2[2,1] <- 190800
bb2[2,2] <- 317700
bb2[1,1] <- bb2[1,1]
bb2[1,2] <- bb2[1,2] - 27700

#--- lower ---#
bb3 <- um_buf@bbox
bb3[2,1] <- min(um_bd_coords[id == 'Upper Republican',lat]) - 100 
bb3[2,2] <- 190800
bb3[1,1] <- bb3[1,1] + 3800
bb3[1,2] <- bb3[1,2] - 24000

#--- assign a spatial block to each well ---#
um_buf_h <- sp_block_lat(8,bb1,um_buf,'Upp_Mid','sp_block_um_u')
um_buf_m <- sp_block_lat(8,bb2,um_buf,'Upp_Mid','sp_block_um_m')
um_buf_l <- sp_block_lat(8,bb3,um_buf,'Upp_Mid','sp_block_um_l')

#--- sp_block_um ---#
rrb[,sp_block_um:=0]
rrb[,sp_block_um:=sp_block_um_u + sp_block_um_m + sp_block_um_l]

#--------------------------
# Mapping
#--------------------------
#--- restrict geographic focus ---#
bb_nrd <- um_buf@bbox
bb_nrd[1,1] <- bb_nrd[1,1] - 170000
bb_nrd[1,2] <- bb_nrd[1,2] + 90000
bb_nrd[2,2] <- bb_nrd[2,2] + 70000

bd_new <- res_bd(bb_nrd,um_bd)

#--- centroids ---#
centroids <- gCentroid(bd_new,byid=TRUE)
nrd_name <- coordinates(centroids) %>% data.table()
rownames(nrd_name) <-	c('Upper Republican','Middle Republican')

#--- mapping ---#
bd_new_f <- fortify(bd_new,region='NRD_Name')
um_buf_h_f <- fortify(um_buf_h,region='sp_id')
um_buf_m_f <- fortify(um_buf_m,region='sp_id')
um_buf_l_f <- fortify(um_buf_l,region='sp_id')

g <- ggplot(data=bd_new_f,aes(x=long,y=lat)) +
	geom_polygon(aes(group=group),fill='white',colour="black",size=0.2) +
	geom_polygon(data=um_buf_h_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_polygon(data=um_buf_m_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_polygon(data=um_buf_l_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_point(data=rrb[NRD %in% c('Upper Republican','Middle Republican') &
		long >= bb_nrd[1,1] & long <= bb_nrd[1,2] & lat <= bb_nrd[2,2]],
		aes(x=long,y=lat),colour='red',size=0.4) +
	geom_point(data=rrb[Upp_Mid==1 & sp_block_um != 0,],aes(x=long,y=lat),colour='blue',size=0.4) +
	geom_text(data=nrd_name,aes(x=x,y=y,label=rownames(nrd_name)),size=4) +
	coord_equal(ratio=1) +
  labs(x="", y="") + #labels
  	theme(
  		axis.ticks.y = element_blank(),
  		axis.text.y = element_blank(), # get rid of x ticks/text
  		axis.ticks.x = element_blank(),
  		axis.text.x = element_blank(),
  		panel.background = element_rect(fill='white'),
      legend.title = element_blank()
  	)
ggsave("~/Dropbox/MyResearch/WaterUseSmoothing/Graphs/by_nrd_pair/Upp_Mid.pdf")

#===================================
#  Middle and Gosper (Tri-Basin)
#===================================
#--------------------------
# create spatial blocks and identify which wells are in each of them
#--------------------------
#--- select relevant nrds ---#
mg_bd <- nrd_boundary[nrd_boundary@data$NRD_Name %in% c('GOS_Tri','Middle Republican'),]
mg_buf <- buffers[buffers@data$id=='Mid_GOS',]

#--------------------------
# Define the extent of 3 buffers
#-------------------------- 
#--- upper ---#
bb1 <- mg_buf@bbox
bb1[2,1] <- 220500 
bb1[2,2] <- max(rrb[Mid_GOS==1 & county=='GOSPER',lat]) + 100 
bb1[1,1] <- bb1[1,1] + 31300

#--- lower ---#
bb2 <- mg_buf@bbox
bb2[2,1] <- 201800
bb2[2,2] <- 240500
bb2[1,1] <- 1613655
bb2[1,2] <- 1625655

#--- assign a spatial block to each well ---#
mg_buf_u <- sp_block_lat(6,bb1,mg_buf,'Mid_GOS','sp_block_mg_u')
mg_buf_m <- sp_block_lat(1,bb2,mg_buf,'Mid_GOS','sp_block_mg_m')

#--- sp_block_mg ---#
rrb[,sp_block_mg:=0]
rrb[,sp_block_mg:=sp_block_mg_u + sp_block_mg_m]

#--------------------------
# Mapping
#--------------------------
#--- restrict geographic focus ---#
bb_nrd <- mg_buf@bbox
bb_nrd[1,1] <- bb_nrd[1,1] - 60000
bb_nrd[1,2] <- bb_nrd[1,2] + 70000
bb_nrd[2,2] <- bb_nrd[2,2] + 70000
bb_nrd[2,1] <- bb_nrd[2,1] - 20000

bd_new <- res_bd(bb_nrd,mg_bd)

#--- centroids ---#
centroids <- gCentroid(bd_new,byid=TRUE)
nrd_name <- coordinates(centroids) %>% data.table()
rownames(nrd_name) <-	c('Middle Republican','Gosper')

#--- mapping ---#
bd_new_f <- fortify(bd_new,region='NRD_Name')
mg_buf_u_f <- fortify(mg_buf_u,region='sp_id')
mg_buf_m_f <- fortify(mg_buf_m,region='sp_id')

g <- ggplot(data=bd_new_f,aes(x=long,y=lat)) +
	geom_polygon(aes(group=group),fill='white',colour="black",size=0.2) +
	geom_polygon(data=mg_buf_u_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_polygon(data=mg_buf_m_f,aes(x=long,y=lat,group=group),fill='red',colour="black",alpha=0.4,size=0.3) +
	geom_point(data=rrb[NRD %in% c('GOSPER_Tri','Middle Republican') &
		long >= bb_nrd[1,1] & long <= bb_nrd[1,2] & lat <= bb_nrd[2,2] & lat >= bb_nrd[2,1]],
		aes(x=long,y=lat),colour='red',size=0.4) +
	geom_point(data=rrb[Mid_GOS==1 & sp_block_mg != 0,],aes(x=long,y=lat),colour='blue',size=0.4) +
	geom_text(data=nrd_name,aes(x=x,y=y,label=rownames(nrd_name)),size=4) +
	coord_equal(ratio=1) +
  labs(x="", y="") + #labels
  	theme(
  		axis.ticks.y = element_blank(),
  		axis.text.y = element_blank(), # get rid of x ticks/text
  		axis.ticks.x = element_blank(),
  		axis.text.x = element_blank(),
  		panel.background = element_rect(fill='white'),
      legend.title = element_blank()
  	)
ggsave("~/Dropbox/MyResearch/WaterUseSmoothing/Graphs/by_nrd_pair/Mid_Gos.pdf")


#--- final manipulation ---#
rrb <- rrb[within_any!=0,]
save(rrb,file='rrb_sp_block.RData')








