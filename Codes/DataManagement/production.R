######################################
# Manage the production data
######################################

#===================================
# 0. Preliminary Operations
#===================================
setwd('~/Dropbox/CollaborativeResearch/AllocationImpacts')
source('~/Dropbox/R_libraries_load/library.R')

#===================================
# Production data processing
#===================================
# #--- import the csv file ---#
prod <- read_csv('./Data/Production/production.txt') %>% 
  data.table()
saveRDS(prod,'./Data/Production/production.rds')

#--- import the data ---#
prod <- readRDS('./Data/Production/production.rds') 

#--------------------------
# recover section, township, and range
#--------------------------
prod[,section:=substr(landlocation,2,3)] 
prod[,twsp:=substr(landlocation,6,7)] 
prod[,twsp_SN:=substr(landlocation,8,8)] 
prod[,rng:=substr(landlocation,11,12)] 
prod[,rng_EW:=substr(landlocation,13,13)] 

# prod[,section] %>% unique()
# prod[,twsp] %>% unique()
# prod[,rng] %>% unique()

#--- get rid of those who do not have complete location information ---#
prod <- prod[
	!is.na(section) & as.numeric(section) <= 36 &
	!is.na(twsp) & !is.na(twsp_SN) & as.numeric(twsp) <= 35 & 
	!is.na(rng) & !is.na(rng_EW) & as.numeric(rng) <= 46,]

#--- save the data ---#
saveRDS(prod,'./Data/Production/prod_NE.rds')	

#===================================
# Merge with section data to assign NRD 
#===================================
#--- import production data ---#
prod_NE <- readRDS('./Data/Production/prod_NE.rds')[state==31,]

setkey(prod_NE,twsp,rng)

#--- import section data ---#
RRB_twsp <- readRDS('./Data/Geographic/RRB_twsp.rds')
RRB_twsp_dt <- RRB_twsp@data[,.(twnshp,rng,twsp_SN,rng_WE,NRD,Phase3)]

RRB_twsp_dt[,twsp:=as.character(twnshp)]
RRB_twsp_dt[,rng:=as.character(rng)]

setkey(RRB_twsp_dt,twsp,rng)

#--- merge with the township data ---#
prod_NE <- RRB_twsp_dt[prod_NE]

#--- get only the RRB part ---#
prod_RRB <- prod_NE[!is.na(NRD),]

#--- define few variables ---#
prod_RRB[,plss:=paste(twnshp,twsp_SN,rng,rng_WE,section,sep='')]
prod_RRB[,obs_id:=1:nrow(prod_RRB)]

#===================================
# Identify which sections have overlapped area with buffers
#===================================
#--- read the sections data ---#
RRB_sec <- readRDS('./Data/Geographic/RRB_sec.rds')

#--- read the buffers ---#
buffers_all <- readRDS('./Data/Geographic/NRD_buffers/buffers_NRDS.rds') %>% 
  spTransform(proj4string(RRB_sec))

buf_miels_ls <- seq(1,10)
nrd_comb_ls <- buffers_all@data[,id] %>% unique()
buf_nrd_ls <- expand.grid(buf_miels_ls,nrd_comb_ls)
buf_nrd_len <- nrow(buf_nrd_ls)

#--------------------------
# define the function 
#--------------------------
buf_overlap_sec <- function(i){
  #--- get the buffer to work with ---# 
  buf_miles_temp <- buf_nrd_ls[i,1]
  buf_id_temp <- buf_nrd_ls[i,2]

  buf_temp_index <- buffers_all@data[,
    buf_miles== buf_miles_temp & 
    id==buf_id_temp  
    ]

  buf_temp <- buffers_all[buf_temp_index,]
  # plot(buf_temp)

  #--- find the sections that overlaps with the buffer ---#
  plss_ls <- RRB_sec[buf_temp,]@data[,paste(twnshp,twsp_SN,rng,rng_WE,section_,sep='')] %>% 
    unique()
  # plot(RRB_sec[buf_temp,])

  #--- create the dummy ---#
  data_temp <- prod_RRB[,.(obs_id,NRD,in_out=(plss %in% plss_ls),buf_id=paste(buf_id_temp,'_',buf_miles_temp,'mi',sep=''))] 

  return(data_temp)
}

#--------------------------
# Run the function to create in-out dummies
#--------------------------
buf_dummies <- mclapply(1:buf_nrd_len,buf_overlap_sec ,mc.cores=6) %>% 
  rbindlist() %>% 
  dcast(obs_id~buf_id,value.var='in_out') %>% 
  data.table() %>% 
  setkey(obs_id)

#--- merge with the production data ---#
setkey(prod_RRB,obs_id)
prod_RRB <- buf_dummies[prod_RRB]

#===================================
# Define some new variables
#===================================
#--- practice ---#
prod_RRB[,practice_txt:='']
prod_RRB[practice==2,practice_txt:='irrigated']
prod_RRB[practice==3,practice_txt:='dry']
prod_RRB[practice==4,practice_txt:='continuous']
prod_RRB[practice==5,practice_txt:='sum_fallow']

#--- crop code ---#
prod_RRB[,crop:='']
prod_RRB[cropcode==11,crop:='Wheat']
prod_RRB[cropcode==41,crop:='Corn']
prod_RRB[cropcode==51,crop:='Sorghum']
prod_RRB[cropcode==81,crop:='Soybeans']

#--- year-plss factors ---#
prod_RRB[,year_plss:=paste(cropyear,plss,sep='')]

#---  ---#
prod_RRB[,id_ci:=paste(idnumber,'_',landlocation,'_',crop,'_',practice_txt,sep='')]

#--- duplicates ---#
prod_RRB[,count:=.N,by=.(idnumber,landlocation,crop,practice_txt,cropyear)]
prod_RRB <- prod_RRB[count==1,]

#===================================
# Merge with prism data
#===================================
setkey(prod_RRB,plss,cropyear)
prism_plss_final <- readRDS('./Data/plss_prism_final.rds') %>% 
  setkey(plss,year)

#--- merge ---#
# prism data only available for some sections that are 
# located around the NRD borders
prod_final <- prism_plss_final[prod_RRB]

# prod_final[!is.na(precip_in),] %>% nrow()

#===================================
# Save
#===================================
# Sections that exist in the dataset are within the certain range of the borders
# The distance of the buffer is indicated by 'buf_miles' 
# The buffer id (two NRDs that share the border) is indicated by 'buf_id' 

saveRDS(prod_final,'./Data/Production/prod_RRB.rds')
save.dta13(prod_final,'./Data/Production/prod_RRB.dta')


