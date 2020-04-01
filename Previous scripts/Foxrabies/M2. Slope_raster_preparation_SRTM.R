##'**MRES PROJECT**
##'
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'
##'**MICAELA DE LA PUENTE L.**        
##'          
##'**This script create the elevation raster** 
####################################################

## Call the packages
## Call the packages
library(ggplot2)
library(ggmap)
library(mapproj)
library(Rcpp)
library(foreign)
library(maptools)
library(sp)
library(rgdal)
library(raster)
#####################################################

countries_zero <-readOGR("data/Boundaries data/welevel0.shp","welevel0")
fli.raster <- raster("output/new_raster2_fli.tif")

#######     Get DATA ONLINE     #########

#http://www.gis-blog.com/r-raster-data-acquisition/#
#http://www.chemie.fu-berlin.de/diverse/doc/ISO_3166.html
#http://www.guru-gis.net/get-administrative-topographic-and-climatic-data#-directly-with-r/#


####   ELEVATION - SRTM  ####

# https://www.iso.org/obp/ui/#search
## Download the rasters
aus_elev <- getData('alt', country="AUT", path = "data/Elevation")
bel_elev <- getData('alt', country="BEL", path = "data/Elevation")
fra_elev <- getData('alt', country="FRA", path = "data/Elevation")
ger_elev <- getData('alt', country="DEU", path = "data/Elevation")
lux_elev <- getData('alt', country="LUX", path = "data/Elevation")
swi_elev <- getData('alt', country="CHE", path = "data/Elevation")
net_elev <- getData('alt', country="NLD", path = "data/Elevation")
ita_elev <- getData('alt', country="ITA", path = "data/Elevation")
slok_elev <- getData('alt', country="SVK", path = "data/Elevation")
cze_elev <- getData('alt', country="CZE", path = "data/Elevation")
dnk_elev <- getData('alt', country="DNK", path = "data/Elevation")
pol_elev <- getData('alt', country="POL", path = "data/Elevation")
esp_elev <- getData('alt', country="ESP", path = "data/Elevation")
hun_elev <- getData('alt', country="HUN", path = "data/Elevation")
svn_elev <- getData('alt', country="SVN", path = "data/Elevation")


## Merge the rasters
mosaic <- mosaic(aus_elev, bel_elev, fra_elev, ger_elev, lux_elev, 
                 swi_elev, net_elev, ita_elev, slok_elev, cze_elev, 
                 dnk_elev, pol_elev, esp_elev, hun_elev, svn_elev, fun=mean)

plot(mosaic)
mosaic # coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 

# Change the projection
proj4string(mosaic) <- CRS("+init=epsg:5639")
mosaic
# values  : -179, 4536  (min, max)   # negative are water
#extent : -5.2, 17.3, 41.3, 55.2 (xmin, xmax, ymin, ymax)#right coords.
# coord. ref. : +init=epsg:5639 +proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0#=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
#data source : C:\Users\Mica\AppData\Local\Temp\R_raster_Mica\r_tmp_2016-02-27_140349_3#676_5824

# full
# extent      : -9.4, 24.2, 35.2, 57.8  (xmin, xmax, ymin, ymax)

# Check if values remain
hasValues(mosaic) # [1] TRUE
inMemory(mosaic) # [1] FALSE  # don't know what it is

# Plot the raster layer
plot(mosaic, colNA="blue", main="RasterLayer of elevation from strm")


### Calculate slope values
slope.raster <- terrain(mosaic, opt = "TRI", neighbors=8, unit='degrees')
plot(slope.raster)
plot(countries_zero, add=TRUE)

freq(slope.raster)
# TRI (Terrain Ruggedness Index) is the mean of the absolute differences 
# between the value of a cell and the value of its 8 surrounding cells
# http://finzi.psych.upenn.edu/library/raster/html/terrain.html


### LOWER RESOLUTION
## Call the raster
slope.raster
#dimensions  : 2712, 4032, 10934784  (nrow, ncol, ncell)
#resolution  : 0.008333333, 0.008333333  (x, y)
#extent      : -9.4, 24.2, 35.2, 57.8  (xmin, xmax, ymin, ymax)
#coord. ref. : +init=epsg:5639 +proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
#values      :  0, 944.25  (min, max)  (min, max)
# Check if values remain

# Check land use xtent
e <- extent(fli.raster) # same extent of elevation

# Crop the extent
slope.raster1 <- crop(slope.raster, e)
# elevation1 # with only we countries
# dimensions  : 1656, 2680, 4438080  (nrow, ncol, ncell)
#resolution  : 0.008333333, 0.008333333  (x, yprp
#extent      : -5.166667, 17.16667, 41.28333, 55.08333  (xmin, xmax, ymin, ymax)
# values      : 0, 944.25  (min, max)

plot(slope.raster1)
plot(countries_zero, add=TRUE)
# it is ok so far

### Reduce esolution of land use raster
slope.low <- projectRaster(slope.raster1, fli.raster, method='bilinear')
slope.low
# dimensions  : 207, 335, 69345  (nrow, ncol, ncell)
# resolution  : 0.06666667, 0.06666667  (x, y)
# extent      : -5.166667, 17.16667, 41.28333, 55.08333  (xmin, xmax, ymin, ymax)
# values      :  0, 634.1893  (min, max)

writeRaster(slope.low, filename="output/slope_we.tif", 
            format="GTiff", overwrite=TRUE)
# With also border countries

# Try if it works
slope.low <- raster("output/slope_we.tif")
freq(slope.low) # [395,]    NA 26285


#### CREATE a RASTER FOR WE ONLY
slope_raster1 <- rasterize(countries_zero, slope.low, mask=TRUE) 
slope_raster1 # 0, 634.1893  (min, max)

## Plot the new raster
plot(slope_raster1, legend=FALSE)
length(slope_raster1) #[1] 69345 # number of cells
# Check the frequency of each value
freq(slope_raster1, value=NA) # [1]NA 39327, with boundaries also

#### Mask them to get a better shape
slope_raster2 <- mask(slope_raster1, slope.low)
## Check if values are the same that previous raster
freq(slope_raster2) # NA values sum is the same.
plot(slope_raster2) 

## save it
writeRaster(slope_raster2, filename="output/slope_we2.tif", 
            format="GTiff", overwrite=TRUE)
# We will use this one.

# Try if it works
par(mar = c(2,4,2,0.5))
slope_raster <- raster("output/slope_we2.tif")
