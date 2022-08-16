######################################
# Manage the townshiop data
######################################

#===================================
# 0. Preliminary Operations
#===================================
setwd('~/Dropbox/CollaborativeResearch/AllocationImpacts')
source('~/Dropbox/R_libraries_load/library.R')

#===================================
# 1. Manage geographic information
#===================================
#--------------------------
# NE counties
#--------------------------
NE_county <- readOGR(dsn = './Data/Geographic/County', 'county_bound')
# plot(NE_county)

#--------------------------
# NRDs
#--------------------------
#--- import NRD shape file ---#
NRD <- readOGR(dsn = './Data/Geographic/NRDUTM', 'NRDUTM')
# plot(NRD)

NRD@data$NRD_Name <- as.character(NRD@data$NRD_Name)
NRD@data[NRD@data$NRD_Name=='Little Bue','NRD_Name'] ='Little Blue'

#--- select NRDs ---#
nrd_list <- c('Middle Republican', 'Upper Republican', 'Lower Republican','Tri-Basin')
NRD <- NRD[NRD@data$NRD_Name %in% nrd_list,]

NRD_td <- tidy(NRD,region='NRD_Name') %>% data.table()

#===================================
# NE township
#===================================
NE_township <- readOGR(dsn = './Data/Geographic/plss_NE', 'TownshipUTM')

#--- townships in the four NRDs ---#
RRB_twsp <- NE_township[NRD,]
RRB_twsp@data <- RRB_twsp@data %>% 
	data.table()  
setnames(RRB_twsp@data,names(RRB_twsp@data),tolower(names(RRB_twsp@data)))
setnames(RRB_twsp@data,'objectid','id')
RRB_twsp_dt <- RRB_twsp@data %>% setkey('id')

#--- The township in Phase 3 ---#
# Gosper County: township 5, range 22, all sections.
RRB_twsp@data[,Phase3:=FALSE]
RRB_twsp@data[twnshp=='05' & rng=='22',Phase3:=TRUE]

#--- directions ---#
RRB_twsp@data[,twsp_SN:=substr(township,1,1)]
RRB_twsp@data[,rng_WE:=substr(range,1,1)]

#--------------------------
# Which township in which NRD?
#--------------------------
nrd_list <- c('Middle Republican', 'Upper Republican', 'Lower Republican','Tri-Basin')
nrd_len <- length(nrd_list)
RRB_twsp@data[,NRD:='']

for (i in 1:nrd_len){
	NRD_temp <- NRD[NRD@data$NRD_Name %in% nrd_list[i],]
	# plot(RRB_twsp[NRD_temp,])

	clipped_twsp <- gIntersection(
		RRB_twsp,NRD_temp,byid=TRUE,drop_lower_td = TRUE,
		id=getSpPPolygonsIDSlots(RRB_twsp))

	#--- select townships by area ---#
	twsp_in <- clipped_twsp[gArea(clipped_twsp,byid=TRUE)>=10e5,]
	# plot(twsp_in)

	in_ids <- getSpPPolygonsIDSlots(twsp_in) 

	RRB_twsp@data[getSpPPolygonsIDSlots(RRB_twsp) %in% in_ids,NRD:=nrd_list[i]]
	# plot(RRB_twsp[getSpPPolygonsIDSlots(RRB_twsp) %in% in_ids,])
}

RRB_twsp@data[NRD=='',NRD:=NA]

#--- save the township data ---#
saveRDS(RRB_twsp,'./Data/Geographic/RRB_twsp.rds')

#--------------------------
# Create visualization-ready data
#--------------------------
RRB_twsp_dt <- RRB_twsp@data %>% 
	setkey('id')

#--- tidy ---#
RRB_twsp_td <- tidy(RRB_twsp,region='id') %>% 
	data.table() 
RRB_twsp_td[,id:=as.numeric(id)]
setkey(RRB_twsp_td,id)

#--- merge with the original data ---#	
RRB_twsp_td <- RRB_twsp_dt[RRB_twsp_td]

#--- save the fortified data ---#
saveRDS(RRB_twsp_td,'./Data/Geographic/RRB_twsp_td.rds')

#===================================
# NE sections
#===================================
#--- subset the NE section data to RRB section data ---#
# NE_sections <- readOGR(dsn = './Data/Geographic/plss_NE', 'SectionsUTM')
# RRB_sec <- NE_sections[NRD,]
# saveRDS(RRB_sec,'./Data/Geographic/RRB_sec_raw.rds')

RRB_sec <- readRDS('./Data/Geographic/RRB_sec_raw.rds')
RRB_sec@data <- RRB_sec@data %>% data.table()
setnames(RRB_sec@data,names(RRB_sec@data),tolower(names(RRB_sec@data)))
RRB_sec@data[,twsp_SN:='N']
RRB_sec@data[,rng_WE:='W']
RRB_sec@data[,twnshp:=as.character(twnshp)]
RRB_sec@data[,rng:=as.character(rng)]
RRB_sec@data[,id:=1:nrow(RRB_sec@data)]
setkey(RRB_sec@data,twnshp,rng)

#--------------------------
# assign NRD status
#--------------------------
RRB_twsp <- readRDS('./Data/Geographic/RRB_twsp.rds')
RRB_twsp_dt <- RRB_twsp@data[,.(twnshp,rng,twsp_SN,rng_WE,NRD,Phase3)]  
RRB_twsp_dt[,twnshp:=as.character(twnshp)]  
RRB_twsp_dt[,rng:=as.character(rng)]  
setkey(RRB_twsp_dt,twnshp,rng)

#--- merge with the township data ---#
RRB_sec@data <- RRB_twsp_dt[RRB_sec@data] %>% setkey(id)

#--------------------------
# save
#--------------------------
saveRDS(RRB_sec,'./Data/Geographic/RRB_sec.rds')

#--------------------------
# create visualization-ready data
#--------------------------
RRB_sec_dt <- RRB_sec@data %>% 
	mutate(id=getSpPPolygonsIDSlots(RRB_sec)) %>% 
  data.table() %>% 
  setkey(id)

#--- tidy ---#
RRB_sec_td <- tidy(RRB_sec) %>% 
	data.table() 
setkey(RRB_sec_td,id)

#--- merge with the original data ---#	
RRB_sec_td <- RRB_sec_dt[RRB_sec_td]

#--- save the fortified data ---#
saveRDS(RRB_sec_td,'./Data/Geographic/RRB_sec_td.rds')

