##'**MRES PROJECT**
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'**MICAELA DE LA PUENTE L.**        
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
elev.fli <- raster("C:/Users/Micaela/Dropbox/Fox Rabies_MD/FLI/Data/output/altitude_raster.tif")
fli.raster <- raster("output/new_raster2_fli.tif")

####   ELEVATION - FLI  ####

altitude <-readOGR("C:/Users/Micaela/Dropbox/Fox Rabies_MD/FLI/Data/Raster/Europe_altitude/Europe_altitude.shp", 
                   "Europe_altitude")

# Explore the dataset
class(altitude) # "SpatialPolygonsDataFrame"

## Transform it to a raster
summary(altitude)
#Object of class SpatialPolygonsDataFrame
#Coordinates:
#  min  max
#x -10.7 51.0
#y  34.5 71.2
#Is projected: FALSE 
#proj4string :
#  [+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0]
#Data attributes:
#  HOEHE          Shape_Leng    Shape_Area  
#Min.   : -30.0   Min.   :0.4   Min.   :0.01  
#1st Qu.:   0.0   1st Qu.:0.4   1st Qu.:0.01  
#Median :  65.0   Median :0.4   Median :0.01  
#Mean   : 193.4   Mean   :0.4   Mean   :0.01  
#3rd Qu.: 194.0   3rd Qu.:0.4   3rd Qu.:0.01  
#Max.   :3772.0   Max.   :0.4   Max.   :0.01  

# Create the raster
raster.alt <- raster(extent(altitude)) # use the same extent
proj4string(raster.alt) <- CRS("+init=epsg:5639")

# for the raster    
res(raster.alt)=c(0.066667,0.066667) # sets the resolution of the raster to 10 m

# Add the attribute
raster.alt2 <- rasterize(altitude, field="HOEHE", raster.alt) # converts to a raster
# assigning the attribute of interest
raster.alt2
#dimensions  : 550, 925, 508750  (nrow, ncol, ncell)
#resolution  : 0.066667, 0.066667  (x, y)
#extent      : -10.7, 50.96698, 34.53315, 71.2  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
#values      : -30, 3772  (min, max)
proj4string(raster.alt2) <- CRS("+init=epsg:5639")
plot(raster.alt2)

# Check land use xtent
e <- extent(fli.raster) # same extent of elevation
# Crop the extent
elev.fli1 <- crop(raster.alt2, e)
# dimensions  : 207, 335, 69345  (nrow, ncol, ncell)
# resolution  : 0.066667, 0.066667  (x, y)
# extent      : -5.166639, 17.16681, 41.26652, 55.06659  (xmin, xmax, ymin, ymax)
# values      : values      : -5, 3165  (min, max)
## Decrease resolutions

# writeRaster(elev.fli1, filename="output/elevationwe_fli1.tif", 
#             format="GTiff", overwrite=TRUE)
# With also border countries

# Try if it works
elev.fli1 <- raster("output/elevationwe_fli1.tif")
freq(elev.fli1)  # no nas


#### CREATE a RASTER FOR WE ONLY
elev.fli2 <- rasterize(countries_zero, elev.fli1, mask=TRUE) 
elev.fli2 # -5, 3165

## Plot the new raster
plot(elev.fli2, legend=FALSE)
length(elev.fli2) #[1] 69345 # number of cells
# Check the frequency of each value
freq(elev.fli2, value=NA) # [1]NA 39070, with boundaries also

#### Mask them to get a better shape
elev.fli3 <- mask(elev.fli2, elev.fli1)
## Check if values are the same that previous raster
freq(elev.fli3) # NA values sum is the same.
# There are 322 less NA cells than in the land use raster. Probably are water bodies that don't have landuse.
## Plot the new raster
plot(elev.fli3) 

## save it
# writeRaster(elev.fli3, filename="output/elevationwe_fli2.tif", 
#             format="GTiff", overwrite=TRUE)
# We will use this one.

# Try if it works
elev_raster <- raster("output/elevationwe_fli2.tif")
plot(elev_raster)
