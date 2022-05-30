# ==========================================================================
# Make 10 miles buffer data
# ==========================================================================

# /*===== library =====*/
library(here)
source(here("GitControlled/Codes/0_1_ls_packages.R"))
library(rgeos); library(sp)

# === NRD boundary === #
nrd_bound <- 
    here("Shared/Data/WaterAnalysis/NRD_bd/BND_NaturalResourceDistricts_DNR.shp") %>%
    st_read() %>%
    filter(NRD_Name %in% c("Lower Republican", "Tri-Basin")) %>%
    st_transform(32614) # WGS UTM 14 (Cartesian 2D CS covering NE)

# === Find the common boundary === #
bd_interiors <- st_intersection(nrd_bound) %>%
    filter(n.overlaps == 2)

# --- Check: interior boundary --- #
ggplot() +
    geom_sf(data=nrd_bound) +
    geom_sf(data=bd_interiors, color="red")


# /*===========================================*/
#'=  Make 10 miles buffers around the boundary =
# /*===========================================*/
buffer_miles <- 10 # in miles
buffer_meter <- 1609.344*buffer_miles # in meters

bd_buffer <- 
    st_buffer(bd_interiors, dist = buffer_meter
    # ,endCapStyle = "FLAT"
    ) %>%
    st_geometry()

# --- Check: interior boundary --- #
ggplot() +
    geom_sf(data=nrd_bound) +
    geom_sf(data=st_transform(bd_interiors, 32614), color="red") +
    geom_sf(data=bd_buffer, fill="green", size=NA, alpha=0.6)

# === Cut the irrelevant part (edge) of the buffer === #
bbox_buffer <- 
    bd_buffer %>%
    st_bbox()

bbox_target <- 
    bd_interiors %>%
    st_bbox()

# ymin
bbox_target[2] <- bbox_buffer[["ymin"]]
# ymax
bbox_target[4] <- bbox_buffer[["ymax"]]
# change to sf object
bbox_target_sf <- st_as_sfc(bbox_target)

ggplot() +
    geom_sf(data=nrd_bound) +
    geom_sf(data=st_transform(bd_interiors, 32614), color="red") +
    geom_sf(data=bd_buffer, fill="green", size=NA, alpha=0.6)+
    geom_sf(data=bbox_target_sf, color="blue", fill=NA)

# select a portion of the polygon that fits within bbox_target_sf 
bd_buffer_10 <- st_intersection(bd_buffer,bbox_target_sf)

ggplot() +
    geom_sf(data=nrd_bound) +
    geom_sf(data=st_transform(bd_interiors, 32614), color="red") +
    geom_sf(data=bd_buffer_10, fill="green", size=NA, alpha=0.6)+
    geom_sf(data=bbox_target_sf, color="blue", fill=NA)

saveRDS(bd_buffer_10, here("Shared/Data/WaterAnalysis/bd_buffer_10.rds"))


# /*===========================================*/
#'=  Make 5 miles buffers around the boundary =
# /*===========================================*/

buffer_miles <- 5 # in miles
buffer_meter <- 1609.344*buffer_miles # in meters

bd_buffer <- 
    st_buffer(bd_interiors, dist = buffer_meter, endCapStyle = "SQUARE"
    ) %>%
    st_geometry()

# --- Check: interior boundary --- #
ggplot() +
    geom_sf(data=nrd_bound) +
    geom_sf(data=st_transform(bd_interiors, 32614), color="red") +
    geom_sf(data=bd_buffer, fill="green", size=NA, alpha=0.6)

# === Cut the irrelevant part (edge) of the buffer === #
bbox_buffer <- 
    bd_buffer %>%
    st_bbox()

bbox_target <- 
    bd_interiors %>%
    st_bbox()

# ymin
bbox_target[2] <- bbox_buffer[["ymin"]]
# ymax
bbox_target[4] <- bbox_buffer[["ymax"]]
# change to sf object
bbox_target_sf <- st_as_sfc(bbox_target)

ggplot() +
    geom_sf(data=nrd_bound) +
    geom_sf(data=st_transform(bd_interiors, 32614), color="red") +
    geom_sf(data=bd_buffer, fill="green", size=NA, alpha=0.6)+
    geom_sf(data=bbox_target_sf, color="blue", fill=NA)

# select a portion of the polygon that fits within bbox_target_sf 
bd_buffer_5 <- st_intersection(bd_buffer,bbox_target_sf)

ggplot() +
    geom_sf(data=nrd_bound) +
    geom_sf(data=st_transform(bd_interiors, 32614), color="red") +
    geom_sf(data=bd_buffer_5, fill="green", size=NA, alpha=0.6)+
    geom_sf(data=bbox_target_sf, color="blue", fill=NA)

saveRDS(bd_buffer_5, here("Shared/Data/WaterAnalysis/bd_buffer_5.rds"))



