# Questions
+ About `get_SDA_property`, what is method?? (Weighted Average?)
+ Is it okay if I work with Kelvin?

# Missing data for folder
## data


# girdMET
"pr":  precipitation (mm)
<!-- "rmin":  min relative humidity (%) -->
<!-- "rmax":  max relative humidity (%) -->
<!-- "srad":  radiation (W/M^2) -->
"tmmn":  Minimum Near-Surface Air Temperature (Kelvin)
"tmmx":  Maximum Near-Surface Air Temperature (Kelvin)
"pet" : Reference grass evaportranspiration (mm)

+ gridMET data is also available in the Google Earth Engine Data Catalog, which can be accessed with the R library *rgee*

+ the data is stored as *netCDFs* format
	* *ncdf4* package is a specific tool to handle netCDF4 objects


# GDD (Growing Degree Days) calculation
+ see:
	* https://ndawn.ndsu.nodak.edu/help-corn-growing-degree-days.html
	* https://mrcc.purdue.edu/gismaps/info/gddinfo.htm
+ GDD is a measure of heat accumulation used to predict plant and animal development rates. 




## What to do with girdMET data
**objective**: 
Get new 
+ `precip_in`(in-season total precipitation)
+ `tmin_in`(in-season mean)
+ `tmax_in`(in-season mean)
+ `gdd_in` for each well
+ `slope`

+ Definition of in-season: from April to September

+ According to the paper: "In the water use regression, we identify which PRISM gridcell each well is in and assign the well the corresponding PRISM gridcell values of the weather variables"




# SSURGO data
+ "The SSURGO database contains information about soil as collected by the National Cooperative Soil Survey over the course of a century."
+ helpful website:
	* https://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html
	* https://palderman.github.io/DataSciAg/exercises/R-SSURGO-data-download/

+ Soil components are typically composed of multiple horizons(layers). *Component attributes* must be aggregated to a map unit level for map visualization. *Horizon attributes* must be aggregated to the component level, before components are aggregated to the map unit level. *Horizon attributes* may be aggregated for the entire soil profile or for a specific depth range. One may only be interested in the value of a horizon attribute for the surface layer.
	
## R package: **"soilDB"**
### `SDA_spatialQuery()`: 
	+ https://www.rdocumentation.org/packages/soilDB/versions/2.6.2/topics/SDA_spatialQuery
	+ Query SDA (SSURGO / STATSGO) records via spatial intersection with supplied geometries. Input can be *SpatialPoints*, SpatialLines, or SpatialPolygons objects with a valid CRS. Map unit keys, overlapping polygons, or the spatial intersection of `geom` + SSURGO / STATSGO polygons can be returned. 
	 
	
### `get_SDA_property()`:
	+ Get map unit properties from Soil Data Access
	+ see: https://search.r-project.org/CRAN/refmans/soilDB/html/get_SDA_property.html
+ `property`
	* see  
		- https://sdmdataaccess.sc.egov.usda.gov/documents/TablesAndColumnsReport.pdf
		- https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/geo/?cid=nrcs142p2_053631
+ `method`: 
	* see
		- https://github.com/ncss-tech/ssurgoOnDemand
	* *"Dominant Component"* returns the attribute value associated with the component with the highest percent composition in the map unit. If more than one component shares the highest percent composition, the value of the first named component is returned.
	* *"Weighted Average"* computes a weighted average value for all components in the map unit. Percent composition is the weighting factor. The result returned by this aggregation method represents a weighted average value of the corresponding attribute throughout the map unit.






+ `mukey`: mapunit key, unique nationally
+ `muname`: mapunit name
+ `_r` : RV, a value or row (set of values) is representative for the component.

### target variables
+ sand_pct: `Total Sand - Rep Value` : `sandtotal_r`
+ clay_pct: `Total Clay - Rep Value` : `claytotal_r`
+ silt_pct: `Total Silt - Rep Value` : `silttotal_r`
+ kv: `Saturated Hydraulic Conductivity  - Rep Value` : `ksat_r`
	* saturated vertical hydraulic conductivity 
+ awc: `Available Water Capacity - Rep Value` : `awc_r`


+ `Electrical Conductivity - Rep Value` : `ec_r`


## Question:
+ Total sand(0.05mm to 2.0mm in equivalent diameter) or Fine sand (0.10 to 0.25mm in equivalent diameter)??
+ Total clay or Fine clay??
+ Total silt or Fine silt??















