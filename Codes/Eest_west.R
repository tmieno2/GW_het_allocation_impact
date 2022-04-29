
well_all_sf <-
  here("Shared/Data/WaterAnalysis/comp_reg_dt.rds") %>%
  readRDS() %>%
  .[,.(wellid, longdd, latdd)] %>%
  # --- the geographic coordinate in the well data is NAD83 (espg=4269)--- #
  unique(.,by="wellid") %>%
  st_as_sf(., coords = c("longdd","latdd"), crs = 4269)


# --- NRD boundary --- #
nrd_boud <- 
 here("Shared/Data/WaterAnalysis/NRD_bd/BND_NaturalResourceDistricts_DNR.shp") %>%
 st_read() %>%
 filter(NRD_Name %in% c("Lower Republican", "Tri-Basin")) %>%
 st_transform(4269)



#--- highway 183 ---#
nrd_boud_c <- 
  nrd_boud %>%
   st_transform(32614)


well_all_sf_c <- 
  well_all_sf %>%
  st_transform(32614)


hw183_buf <- st_read('./Shared/Data/Geographic/ne_roads/tl_2015_31_prisecroads.shp') %>%
  filter(FULLNAME=="US Hwy 183") %>%
  st_transform(32614) %>%
  # st_transform(st_crs(nrd_boud)) %>%
  .[nrd_boud_c, , op = st_intersects] %>%
  st_buffer(dist=0.0001)

ggplot() +
  geom_sf(data=nrd_boud_c) +
  geom_sf(data=hw183, fill="red")

#===================================
# Identify which side each well is in
#===================================

#--------------------------
# split the LR NRD into two 
#--------------------------
split_LR <- rgeos::gDifference(as(nrd_boud_c, "Spatial"),as(hw183_buf, "Spatial")) %>% 
  disaggregate() %>% # disaggregate the polygon into two
  st_as_sfc 

ggplot()+geom_sf(data=split_LR)

#++++++++++++++++
# which in the east side
#++++++++++++++++
in_east_ls <- well_all_sf_c[split_LR[1],] %>% 
  data.table() %>% 
  .[,geometry:=NULL] %>% 
  .[,in_east:=1] %>%   
  .[,in_west:=0]    

#++++++++++++++++
# which in the west side
#++++++++++++++++
in_west_ls <- well_all_sf_c[split_LR[2],] %>% 
  data.table() %>% 
  .[,geometry:=NULL] %>% 
  .[,in_east:=0] %>%   
  .[,in_west:=1]    

#++++++++++++++++
# combine
#++++++++++++++++
east_or_west <- rbind(in_east_ls,in_west_ls)  

saveRDS(east_or_west, here("Shared/Data/WaterAnalysis/east_or_west.rds"))










