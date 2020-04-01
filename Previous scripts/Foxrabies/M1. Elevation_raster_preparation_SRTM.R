##'**MRES PROJECT**
##'
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'
##'**MICAELA DE LA PUENTE L.**        
##'          
##'**This script create the elevation raster** 
####################################################

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


## Merge the rasters
mosaic <- mosaic(aus_elev, bel_elev, fra_elev, ger_elev, lux_elev, 
                 swi_elev, net_elev, ita_elev, slok_elev, cze_elev, 
                 dnk_elev, pol_elev, esp_elev, fun=mean)

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

writeRaster(mosaic, filename="output/elevationwe_new1.tif", format="GTiff", 
            overwrite=TRUE)

# With boundaries countries as well.


### LOWER RESOLUTION
## Call the raster
elevation <- raster("output/elevationwe_new1.tif")
#class       : RasterLayer 
#dimensions  : 2712, 4032, 10934784  (nrow, ncol, ncell)
#resolution  : 0.008333333, 0.008333333  (x, y)
#extent      : -9.4, 24.2, 35.2, 57.8  (xmin, xmax, ymin, ymax)
#values      : -179, 4536  (min, max)

# Check if values remain
hasValues(elevation) # [1] TRUE
inMemory(elevation) # [1] FALSE  # don't know what it is

# Plot the raster layer
plot(elevation, main="Raster of elevation in WE")

# Check land use xtent
e <- extent(fli.raster) # same extent of elevation

# Crop the extent
elevation1 <- crop(elevation, e)
# elevation1 # with only we countries
# dimensions  : 1656, 2680, 4438080  (nrow, ncol, ncell)
#resolution  : 0.008333333, 0.008333333  (x, yprp#extent      : -5.166667, 17.16667, 41.28333, 55.08333  (xmin, xmax, ymin, ymax)
# values      : -179, 4536  (min, max)
## Decrease resolutions

plot(elevation1)
plot(countries_zero, add=TRUE)
# it is ok so far

### Reduce resolution of land use raster
elev.low <- projectRaster(elevation1, fli.raster, method='bilinear')
elev.low
# dimensions  : 207, 335, 69345  (nrow, ncol, ncell)
# resolution  : 0.06666667, 0.06666667  (x, y)
# extent      : -5.166667, 17.16667, 41.28333, 55.08333  (xmin, xmax, ymin, ymax)
# values      : -6.000006, 4028.496  (min, max)

writeRaster(elev.low, filename="output/elevationwe_new2.tif", 
                       format="GTiff", overwrite=TRUE)
# With also border countries

# Try if it works
elev.low <- raster("output/elevationwe_new2.tif")
freq(elev.low)  # NA 26667


#### CREATE a RASTER FOR WE ONLY
elev_raster1 <- rasterize(countries_zero, elev.low, mask=TRUE) 
elev_raster1 # -6.000006, 3843.253

## Plot the new raster
plot(elev_raster1, legend=FALSE)
length(elev_raster1) #[1] 69345 # number of cells
# Check the frequency of each value
freq(elev_raster1, value=NA) # [1]NA 39178, with boundaries also

#### Mask them to get a better shape
elev_raster2 <- mask(elev_raster1, elev.low)
## Check if values are the same that previous raster
freq(elev_raster2) # NA values sum is the same.
# There are 322 less NA cells than in the land use raster. Probably are water bodies that don't have landuse.
## Plot the new raster
plot(elev_raster2) 

## save it
writeRaster(elev_raster2, filename="output/elevationwe_new3.tif", 
            format="GTiff", overwrite=TRUE)
# We will use this one.

# Try if it works
elev_raster <- raster("output/elevationwe_new3.tif")
plot(elev_raster, ylab="Latitude")
