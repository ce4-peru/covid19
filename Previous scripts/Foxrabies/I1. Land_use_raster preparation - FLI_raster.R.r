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

#####################################################################################

## Import the TIF file
nasa.raster <- raster("data/Land_use_rasters/a2000_eu.tif")
nasa.raster
# dimensions  : 756, 793, 599508  (nrow, ncol, ncell)
# resolution  : 0.08333333, 0.08333333  (x, y)
# extent      : -25, 41.08333, 27, 90  (xmin, xmax, ymin, ymax)

fli.raster <-raster("C:/Users/Micaela/Dropbox/Fox Rabies_MD/FLI/Data/Raster/Corine_land/land_raster_fli.tif") # Modified in QGIS
# dimensions  : 33745, 87960, 2968210200  (nrow, ncol, ncell)
# resolution  : 0.001396235, 0.001396229  (x, y)
# extent      : -49.90671, 72.90614, 25.54471, 72.66045  (xmin, xmax, ymin, ymax)

countries_zero <-readOGR("data/Boundaries data/welevel0.shp",
                         "welevel0")


## Check projection
crs(nasa.raster)
crs(fli.raster)
# CRS arguments:
# +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
# Change projection
crs(nasa.raster) <- crs(countries_zero)
crs(fli.raster) <- crs(countries_zero)
fli.raster
# values      : 1, 44  (min, max)


# Plot the raster
plot(fli.raster, main="Land use raster_tif")
plot(countries_zero, add=TRUE)

## Reclassify values -  agreed with Katie and Laurie
#m <- c(0.9, 1.4, 1,   # continous urban (dense settlement)
#       1.5, 11.4, 2,  # Discontinous urban and the rest of artificials
#       11.5, 22.4, 3, # agricultural lands
#       22.5, 25.4, 4, # highly forested
#       25.5, 29.4, 5, # Moderate forested.
#       29.5, 34.4, 6, # barren lands
#       34.5, 39.4, 7, # marshlands
#       39.5, 44.4, NA) # water bodies

m <- c(0.9, 11.4, 1,   # artificial areas
       11.5, 22.4, 3, # agricultural lands
       22.5, 25.4, 4, # highly forested
       25.5, 29.4, 5, # Moderate forested.
       29.5, 34.4, 6, # barren lands
       34.5, 39.4, 7, # marshlands
       39.5, 44.4, NA) # water bodies

rclmat <- matrix(m, ncol=3, byrow=TRUE)
fli.raster2 <- reclassify(fli.raster, rclmat)

# Set extent and increase resolution of NASA raster
ext <- extent(countries_zero)
nasa.raster1 <- crop(nasa.raster, ext)
# extent: -5.166667, 17.16667, 41.33333, 55.08333 (xmin, xmax, ymin, ymax)
freq(nasa.raster1)

# Set extent and increase resolution of FLI raster
nasa.raster2 <- disaggregate(nasa.raster1, fact = 5, fun = modal)
nasa.raster3 <- aggregate(nasa.raster2, fact = 4, fun = modal)
nasa.raster3
# dimensions  : 207, 335, 69345  (nrow, ncol, ncell)
# resolution  : 0.06666667, 0.06666667  (x, y)

# Copy extent and resolution of NASA land use raster
new_raster <- projectRaster(fli.raster2, nasa.raster3, method="ngb")

## Check the extent
new_raster
# class       : RasterLayer 
# dimensions  :  207, 335, 69345  (nrow, ncol, ncell)
# resolution  : 0.06666667, 0.06666667  (x, y)
# extent      : -5.166667, 17.16667, 41.28333, 55.08333
# coord. ref. : +proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
# values      : 1, 7  (min, max)

# Plot
plot(new_raster)
plot(countries_zero, add=TRUE)

freq(new_raster)
#[1,]     1  3125 # all artificial
#[2,]     3 21361 # agricultural
#[3,]     4  8244 # pastures
#[4,]     5 14892 # forest
#[5,]     6   997 # barren
#[6,]    NA 20276

# if separated continous - agreed w/ KS and LB
#[1,]     1    64 # continous urban
#[2,]     2  3061 # discontinous urban
#[3,]     3 27694 # agricultural lands
#[4,]     4 13449 # highly forested
#[5,]     5  3354 # moderately forested
#[6,]     6   997 # barren lands
#[7,]     7   454 # marshlands
#[8,]    NA 20272

# if continous within artificial areas
#[1,]     1 3125  # artificial lands
#[3,]     3 27694 # agricultural lands
#[4,]     4 13449 # highly forested
#[5,]     5  3354 # moderately forested
#[6,]     6   997 # barren lands
#[7,]     7   454 # marshlands
#[8,]    NA 20272
# this is the current one

## Create the legend labs
#lab.legend <- c("Urban areas", "Croplands", "Rangelands", 
#                "Forested lands", "Treeless and \nbarren lands")

## Create the legend colors
#col.legend <- c("mediumorchid4", "yellow2", "chocolate1",
#                "forestgreen", "bisque3")

# with continous and discontinuous separated
lab.legend <- c("Continuous Urban", "Discontinuous urban", 
                "Agricultural", "Highly forested", 
                "Moderately forested ", "Barren lands",
                "Marshlands")

## Create the legend colors
col.legend <- c("mediumorchid4", "turquoise2", "yellow2", #"chocolate1",
                "forestgreen", "limegreen", "bisque3", "coral1")

# with continuous within artificial
lab.legend <- c("Urban", 
                "Agricultural", "Dense forest", 
                "Moderate forest ", "Barren",
                "Wetlands")

## Create the legend colors
col.legend <- c("mediumorchid4", "yellow2", #"chocolate1",
                "forestgreen", "limegreen", "bisque3", "coral1")


### Plot assigning colours directly
# separated
plot(new_raster, col=c("mediumorchid4", "turquoise2", "yellow2", 
                       #"chocolate1", 
                       "forestgreen", "limegreen", "bisque3", "coral1"))

# together
plot(new_raster, col=c("mediumorchid4", "yellow2", 
                       "forestgreen", "limegreen", "bisque3", "coral1"))

### Plot using breaks
breakpoints <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5) # create breaks to assign colors
colors <- c("mediumorchid4", "turquoise2", "yellow2", #"chocolate1",
                "forestgreen", "limegreen", "bisque3", "coral1") # create a vector with colors for land use
# http://gis.stackexchange.com/questions/17339/raster-legend-in-r-how-to-colour-specific-values
breakpoints <- c(0.5, 1.5, 3.5, 4.5, 5.5, 6.5, 7.5) # create breaks to assign colors
colors <- c("mediumorchid4", "yellow2", #"chocolate1",
            "forestgreen", "limegreen", "bisque3", "coral1") # create a vector with colors for land use


## Plot the land use raster
plot(new_raster, breaks=breakpoints, col=colors, legend=FALSE)
par(oma = c(1, 1, 1, 10))
legend("right", legend=lab.legend, fill=col.legend, 
       x.intersp = .1, y.intersp = .4, cex = 0.75, 
       xpd = TRUE, inset = c(-0.45,0), bty = "n")


####   Create the new raster   ####
# writeRaster(new_raster, filename="output/new_raster1_FLI_v3.tif", 
#             format="GTiff", overwrite=TRUE)

# Try if it works
new_raster1_FLI <- raster("output/new_raster1_FLI.tif")
new_raster1_FLI_v2 <- raster("output/new_raster1_FLI_v2.tif")
new_raster1_FLI_v3 <- raster("output/new_raster1_FLI_v3.tif")
freq(new_raster1_FLI) # [6,]    NA 20726



#### CREATE a RASTER OF LAND USE FOR WE ONLY
new_raster1_FLI <- new_raster1_FLI_v3
land_raster1 <- rasterize(countries_zero,new_raster1_FLI, mask=TRUE) 
## Check values
land_raster1
# dimensions  : 207, 335, 69345  (nrow, ncol, ncell)
# resolution  : 0.06666667, 0.06666667  (x, y)
# extent      : -5.166667, 17.16667, 41.28333, 55.08333  (xmin, xmax, ymin, ymax)
# values      : 1, 6  (min, max)

## Plot the new raster
plot(land_raster1, legend=FALSE)
length(land_raster1) #[1] 69345 # number of cells
# Check the frequency of each value
freq(land_raster1) 
#[6,]    NA 39580

#### Mask them to get a better shape
land_raster2 <- mask(land_raster1, new_raster1_FLI)
## Check if values are the same that previous raster
freq(land_raster2) # NA values sum is the same. PREVIOUS. MY SEPARATION.
#[1,]     1  2090
#[2,]     3 12962
#[3,]     4  5306
#[4,]     5  8793
#[5,]     6   614
#[6,]    NA 39580

# TOGETHER 
#     value count
#[1,]     1  2090
#[2,]     3 17314
#[3,]     4  8350
#[4,]     5  1397
#[5,]     6   614
#[6,]     7   122
#[7,]    NA 39458

## Plot the new raster
plot(land_raster2, breaks=breakpoints, col=colors, legend=FALSE)
par(oma = c(1, 1, 1, 4), mar=c(2,3,4,3))
legend("right", legend=lab.legend, fill=col.legend, 
       x.intersp = .1, y.intersp = .4, cex = 0.75, 
       xpd = TRUE, inset = c(-0.45,0), bty = "n") 

# We will use this one.
# writeRaster(land_raster2, filename="output/new_raster2_FLI.tif", 
#             format="GTiff", overwrite=TRUE)
# writeRaster(land_raster2, filename="output/new_raster2_FLI_v3.tif", 
#             format="GTiff", overwrite=TRUE)

# Try if it works
land.raster2 <- raster("output/new_raster2_FLI.tif")
land.raster2_v3 <- raster("output/new_raster2_FLI_v3.tif")

par(oma = c(1, 1, 1, 2), mar=c(2,4,2,6))
plot(land.raster2_v3,  breaks=breakpoints, col=colors, 
     legend=FALSE, ylab="Latitude")
legend("right", legend=lab.legend, fill=col.legend, 
      x.intersp = .3, y.intersp = 1, cex = 0.9, 
      xpd = TRUE, inset = c(-0.30,0), bty = "n")
