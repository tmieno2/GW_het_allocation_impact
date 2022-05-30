######################################
# Identify wells within RRB NRD buffers of given radius in miles
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

#--- Set working directory ---#
setwd('~/Dropbox/CollaborativeResearch/AllocationImpacts')

#===================================
# Load datasets
#===================================
#--------------------------
# NE NRD boundary 
#--------------------------
#--- Nebraska NRD spatial boundary data ---#
NRD <- readOGR(dsn = './Data/Geographic/NRDUTM', 'NRDUTM')

#--- Convert NRD to UTM Zone 14 ---#
NRD <- spTransform(NRD, CRS("+proj=utm +zone=14 +ellps=WGS84"))

#--- Misc ---#
NRD@data$NRD_Name <- as.character(NRD@data$NRD_Name)
NRD@data[NRD@data$NRD_Name=='Little Bue','NRD_Name'] ='Little Blue'

#--- Extract only the NRDs of interest ---#
nrd_list <- c('Middle Republican', 'Upper Republican', 'Lower Republican','Tri-Basin')
NRD <- NRD[NRD@data$NRD_Name %in% nrd_list,]

#--------------------------
# Wells data
#--------------------------
#--- Load the water use data ---#
data <- readRDS('./Data/merged_data.rds')

#--- Make wells data spatial ---#
rrb_unique <- data[,.(wellid,longdd,latdd)] %>% setkey('wellid') %>% unique()
rrb_spatial <- SpatialPointsDataFrame(
  coords=rrb_unique[,list(longdd,latdd)], 
  data=rrb_unique[,list(wellid)]
  )
proj4string(rrb_spatial) <- CRS("+proj=longlat +datum=WGS84")

#--- convert to UTM ---#
rrb_spatial <- spTransform(rrb_spatial, NRD@proj4string)


#===================================
# Create buffers around the NRD borders
#===================================
#--------------------------
# Define the function to create buffers around the NRD borders
#--------------------------
NRD_buf_gen <- function(buffer_miles){

  #--- miles to meters ---# 
  buffer <- 1609.344*buffer_miles

  #--- create buffers by nrd ---#
  buffer_id <- paste(buffer_miles,'mi',sep='')

  #--- create buffers buy NRD ---#
  buf_nrd <- gBuffer(NRD,width=buffer,byid=TRUE) 

  #--- loop over each combination of NRDS ---#
  buf_spdf <- list()

  for (i in 1:nrd_comb_len){
    buf_id <- paste(c(substring(nrd_comb_ls[[i]], 1, 3)),collapse='_')

    #--- find the intersection of the buffers ---#
    nrds_comb <- nrd_comb_ls[[i]]
    buf_1 <- buf_nrd[buf_nrd@data$NRD_Name==nrds_comb[1],]
    buf_2 <- buf_nrd[buf_nrd@data$NRD_Name==nrds_comb[2],]
    buf_int <- gIntersection(buf_1, buf_2) 

    #--- cut out the part of intersection that does not belong to neither of the nrds ---#
    nrd_base <- NRD[NRD@data$NRD_Name %in% nrds_comb,]
    buf <- gIntersection(buf_int, gUnaryUnion(nrd_base)) 

    #--- make the buffer SPDF ---#
    buf_data <- SpatialPolygonsDataFrame(buf, 
        data=data.table(
          nrd_1 = nrds_comb[1],
          nrd_2=nrds_comb[2], 
          buf_miles=buffer_miles,
          id=buf_id
          )
        )
    buf_data@polygons[[1]]@ID = buf_id
    # plot(buf,col = '#FF000080')
    # plot(NRD,add=TRUE)
    
    #--- save the spdf ---# 
    buf_spdf[[i]] <- buf_data
  }

  #--- combine all the SPDFs and return ---# 
  buffers_temp <- do.call(bind,buf_spdf)
  return(buffers_temp)
}

#--------------------------
# Create buffers
#--------------------------
#--- Set list of RRB NRD combinations for buffers ---#
nrd_comb_ls <- vector('list', 4) 
nrd_comb_ls[[1]] <- c('Upper Republican','Middle Republican')
nrd_comb_ls[[2]] <- c('Middle Republican','Tri-Basin')
nrd_comb_ls[[3]] <- c('Lower Republican','Middle Republican')
nrd_comb_ls[[4]] <- c('Lower Republican','Tri-Basin')
nrd_comb_len <- length(nrd_comb_ls) 

#--- create buffers using the function ---#
buf_miels_ls <- seq(1,10)
buffers <- mclapply(buf_miels_ls, function(x) NRD_buf_gen(x),mc.cores=6)
buffers_all <- do.call(bind,buffers)

#--- save ---#
saveRDS(buffers_all,'./Data/Geographic/NRD_buffers/buffers_NRDS.rds')

# plot(buffers_all[buffers_all$id=='Low_Tri',])

#===================================
# Identify which wells are inside the buffers
#===================================
#--- read the buffers ---#
buffers_all <- readRDS('./Data/Geographic/NRD_buffers/buffers_NRDS.rds')

nrd_comb_ls <- buffers_all@data[,id] %>% unique()
buf_nrd_ls <- expand.grid(buf_miels_ls,nrd_comb_ls)
buf_nrd_len <- nrow(buf_nrd_ls)

buf_in_out <- function(i){

  #--- get the buffer to work with ---# 
  buf_miles_temp <- buf_nrd_ls[i,1]
  buf_id_temp <- buf_nrd_ls[i,2]

  buf_temp_index <- buffers_all@data[,
    buf_miles== buf_miles_temp & 
    id==buf_id_temp  
    ]
  buf_temp <- buffers_all[buf_temp_index,]

  #--- identify all the wells inside the buffer ---#
  within_id <- cbind(rrb_unique[,.(wellid)],over(rrb_spatial,buf_temp))
  within_id[,in_buffer:=0]
  within_id[!is.na(nrd_1),in_buffer:=1]
  within_id <- within_id[,.(wellid,in_buffer)]
  within_id[,buffer_id:=paste(buf_id_temp,'_',buf_miles_temp,'mi',sep='')]

  return(within_id)
}

buf_id <- mclapply(1:buf_nrd_len,buf_in_out,mc.cores=6) %>% 
  rbindlist() %>% 
  dcast(wellid~buffer_id,value.var='in_buffer') %>% 
  data.table()

#--- save ---#
saveRDS(buf_id,'./Data/Geographic/NRD_buffers/buffer_identified.rds')






