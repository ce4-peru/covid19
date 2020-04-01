##'**MRES PROJECT**
##'
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'
##'**MICAELA DE LA PUENTE L.**        
##'          
##'**This script shows find the HR by rasters** 
####################################################

## Call the pacakages
library(ggplot2)
library(ggmap)
library(mapproj)
library(Rcpp)
library(foreign)
library(maptools)
library(sp)
library(rgdal)
library(raster)


#########   WORK WITH NASA RASTERS ##############

## Import the TIF file
nasa.raster <- raster("data/Land_use_rasters/a2000_eu.tif")
nasa.raster
# dimensions  : 756, 793, 599508  (nrow, ncol, ncell)
# resolution  : 0.08333333, 0.08333333  (x, y)
# extent      : -25, 41.08333, 27, 90  (xmin, xmax, ymin, ymax)

fli.raster <-raster("output/new_raster2_FLI.tif") # Modified in QGIS
fli.raster
# dimensions  : 207, 335, 69345  (nrow, ncol, ncell)
# resolution  : 0.06666667, 0.06666667  (x, y)
# extent      : -5.166667, 17.16667, 41.28333, 55.08333  (xmin, xmax, ymin, ymax)

countries_zero <-readOGR("data/Boundaries data/welevel0.shp",
                         "welevel0")


## Check projection
crs(nasa.raster) #  +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
crs(fli.raster)
# CRS arguments:
# +proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000
# +ellps=GRS80 +units=m +no_defs 
crs(nasa.raster) <- crs(countries_zero)
nasa.raster

# Plot the raster
plot(nasa.raster, main="Land use raster_tif")
plot(countries_zero, add=TRUE)


## Change extent and resolution of FLI raster

# Set extent and increase resolution of NASA raster
ext <- extent(countries_zero)
nasa.raster1 <- crop(nasa.raster, ext)
# extent: -5.166667, 17.16667, 41.33333, 55.08333 (xmin, xmax, ymin, ymax)

# Copy extent and resolution of NASA land use raster
new_raster <- projectRaster(nasa.raster1, fli.raster, method="ngb")

## Check the extent
new_raster
# dimensions  : 207, 335, 69345  (nrow, ncol, ncell)
# resolution  : 0.06666667, 0.06666667  (x, y)
# extent      : -5.166667, 17.16667, 41.28333, 55.08333  (xmin, xmax, ymin, ymax)
# values      : 11, 62  (min, max)
# OK


# Plot
plot(new_raster)
plot(countries_zero, add=TRUE)

land.raster <- new_raster

freq(land.raster)
# [1,]    11  2703
#[2,]    12   817
#[3,]    22  1376
#[4,]    23 10014
#[5,]    24   395
#[6,]    31  1568
#[7,]    32 21668
#[8,]    33  1979
#[9,]    34    94
#[10,]    41  1175
#[11,]    42  1026
#[12,]    43   405
#[13,]    51  3785
#[14,]    52  1720
#[15,]    53   116
#[16,]    54   282
#[17,]    61   150
#[18,]    62     2
#[19,]    NA 20070

# values
# 11 - Urban
# 12 - Mixed settlements
# 21 - Rice villages * we dont have this
# 22 - Irrigated villages
# 23 - Rainfed villages
# 24 - Pastoral villages
# 31 - Residential irrigated croplands
# 32 - Residential rainfed croplands
# 33 - Populated croplands
# 34 - Remote croplands *
# 41 - Residential rangelands
# 42 - Populated rangelands
# 43 - Remote rangelands
# 51 - Residential woodlands
# 52 - Populated woodlands
# 53 - Remote woodlands
# 54 - Inhabited trees and barren lands
# 61 - Wild woodlands
# 62 - Wild treeless and barren lands 


## Reclassify values
m <- c(10, 12, 1,   # 11 and 12 - dense settlement
       21, 24, 2,   # 22, 23 and 24
       30, 34, 3,   # 31, 32 and 33
       40, 44, 4,   # 41, 42 and 43
       50, 53, 5,   # 51, 52 and 53
       53.5, 60, 6, # 
       60.5, 61.5, 5, 
       61.6, 62.5, 6)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
land.raster1 <- reclassify(land.raster, rclmat)

freq(land.raster1)
#[1,]     1  3520
#[2,]     2 11785
#[3,]     3 25309
#[4,]     4  2606
#[5,]     5  5771
#[6,]     6   284
#[7,]    NA 20070

## Create the legend labs
lab.legend <- c("Dense \nsettlements", "Villages", "Croplands", "Rangelands", 
                "Forested lands", "Treeless and \nbarren lands")

## Create the legend colors
col.legend <- c("mediumorchid4", "cyan2", "yellow2", "chocolate1",
                "forestgreen", "bisque3")


### Plot assigning colours directly
plot(land.raster1, col=c("mediumorchid4", "cyan2", "yellow2", "chocolate1", 
                         "forestgreen", "bisque3"))

### Plot using breaks
breakpoints <- c(0, 1, 2, 3, 4, 5, 6) # create breaks to assign colors
colors <- c("mediumorchid4", "cyan2","yellow2",
         "chocolate1", "forestgreen", "bisque3") # create a vector with colors for land use
# http://gis.stackexchange.com/questions/17339/raster-legend-in-r-how-to-colour-specific-values

## Plot the land use raster
plot(land.raster1, breaks=breakpoints, col=colors, legend=FALSE)
par(oma = c(1, 1, 1, 4))
legend("right", legend=lab.legend, fill=col.legend, 
       x.intersp = .1, y.intersp = .4, cex = 0.75, 
       xpd = TRUE, inset = c(-0.45,0), bty = "n")


####   Create the new raster   ####
writeRaster(land.raster1, filename="output/new_raster1_nasa.tif", 
            format="GTiff", overwrite=TRUE)

# Try if it works
new_raster1_nasa <- raster("output/new_raster1_nasa.tif")
freq(new_raster1_nasa) # [7,]    NA 20070



#### CREATE a RASTER OF LAND USE FOR WE ONLY
land.raster <- new_raster1_nasa
land_raster1 <- rasterize(countries_zero,land.raster, mask=TRUE) 
## Check values
land_raster1
# values      : 1, 6  (min, max)

## Plot the new raster
plot(land_raster1, legend=FALSE)
length(land_raster1) #[1] 69345 # number of cells
# Check the frequency of each value
freq(land_raster1) 
#[7,]  NA 39282

#### Mask them to get a better shape
land_raster2 <- mask(land_raster1, land.raster)
## Check if values are the same that previous raster
freq(land_raster2) # NA values sum is the same.
## Plot the new raster
plot(land_raster2, breaks=breakpoints, col=colors, legend=FALSE)
par(oma = c(1, 1, 1, 4))
legend("right", legend=lab.legend, fill=col.legend, 
       x.intersp = .1, y.intersp = .4, cex = 0.75, 
       xpd = TRUE, inset = c(-0.45,0), bty = "n") 

writeRaster(land_raster2, filename="output/new_raster2_nasa2000.tif", 
            format="GTiff", overwrite=TRUE)
# Try if it works
new_raster2_nasa2000 <- raster("output/new_raster2_nasa2000.tif")
