# R: Raster Data Handling (Chap4: Raster Data Handling, Chap5: Spatial Interactions of Vector and Raster Dataset)


# ==========================================================================
# Brief Descriptions
# ==========================================================================

+ Mainly two packages: *"raster"* and *"terra"*

## raster package
+ *RasterLayer*: is the raster data class defined by the raster package
+ stack (or read) multiple raster layers:
	* `raster::stack()`  creates *RasterStack* class
	* `raster::brick()` creates *RasterBrick* class

## terra package
+ *SpatRaster* is the only class of raster data in terra package
	* everything is called *SpatRaster* regardress it is single-layer or multiple-layer ones
	* to make multiple layer SpatRaster object, simply use `c()`

+ *SpatVector* is the terra's class for vector data
	* terra functions do not support *sf* as of now
	* convert *sf* object to *SpatRaster* object using `terra::vect()` 


# ==========================================================================
# Functions
# ==========================================================================

## Basics:
+ `terra::rast()`:
	* read raster data of many common formats
	* convert a RasterLayer to a SpatRaster
	* create a SpatRaster

+ `terra::writeRaster()`: save a *SpatRaster* object
+ `terra::crs()`: get CRS
+ `X[[i]]`: access specific layers in a multi-layer raster object
+ `terra::values()`: access the values stored in a *SpatRaster* object
+ `as(, "Raster")`: 


## cropping (use terra::crop())
+ `terra::crop(SpatRaster, sf)`: crop a raster layer to the specific area of interest 


## Extracting to Polygon (use exactextractr::exact_extract())
+ `terra::extract(raster, polygon)`
+ `terra::extract(raster, polygon, fun= mean)`: you can apply some basic summary functions (i.e. mean, min, maxm sum) within the function: 
+ `terra::extract(raster, polygon, exact=TRUE)`: to find weighted summary later
	* to get weighted summary can be obtained in the following way	

```{r}
tmax_by_county_from_stack %>% 
  group_by(ID) %>% 
  summarize(
    tmax_0701 = sum(fraction * PRISM_tmax_stable_4kmD2_20180701_bil) / sum(fraction),
    tmax_0702 = sum(fraction * PRISM_tmax_stable_4kmD2_20180702_bil) / sum(fraction)
  )
```

### Alternatively
+ `exactextractr::exact_extract(raster, polygons)` can be used intead of `terra::extract()`. It is much faster than `terra::extract()`
	* for raster data: both `SpatRaster` and `raster` are acceptable
	* for polygon data: only `sf` is accepted




## Extracting to Points (use terra::extract(raster, points))
+ `terra::extract(raster, points)`: finds which raster cell each of the points is located within and assigns the value of the cell to the point
	* **NOTE** : Before that, you need to change a *sf* object to a *SpatVect* class object using `vect()` function
	* `terra:extract()` works for the combinations of *SpatRaster - SpatVect* or *Raster - sf/sp*


+ Extracting values from a multi-layer *SpatRaster*: 

```{r}
#--- create a multi-layer SpatRaster ---#
prism_tmax_stack <- c(prism_tmax_0701_KS_sr, prism_tmax_0702_KS_sr)

#--- extract tmax values ---#
tmax_from_prism_stack <- terra::extract(prism_tmax_stack, vect(KS_wells))
```


## visualization
	* quick visualization is using `plot()`
+ convert a raster object to a data.frame 
+ or, `tmap` can work directory with raster objects 
	* in this case, you need to convert a *SpatRaster* to a `raster` at first	

```{r}
tmap_leaflet(
	tm_shape(as(IA_cdl_2015_sr, "Raster")) + # what sf to use for creating a map 
	tm_raster() # what type of geometry to put on the map 
) 
```



## Caveat
+ converting a raster object to a data.frame() is very slow
+ for visualization using `ggplot()` the conversion to data.frame is necessary










