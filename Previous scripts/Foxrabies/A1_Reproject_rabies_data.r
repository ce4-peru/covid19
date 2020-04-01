##'**MRES PROJECT**
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'**MICAELA DE LA PUENTE L.**
##'**This script shows reprojection of the data**


### TRANSFORM IT TO A DATA FRAME ###

######################################################
# FIRST SET WORKING DIRECTORY IN RSTUDIO
# i.e. Session > Set Working Directory > To Source File Location
rm(list=ls())

######################################################
library(foreign)
library(maptools)
library(sp)
library(rgdal)
library(rworldmap)
require(rgeos)
######################################################

## Transform it in R
rabies.data.single <-readOGR("data/Incidence/singlepart/Rabigramm_positive_single.shp", 
                             "Rabigramm_positive_single") # file created in QGIS by Katie.
rabies.data.single.df <- as.data.frame(rabies.data.single)
str(rabies.data.single.df)
# 'data.frame':	248066 obs. of  11 variables:
# coords.x1 and coords.x2 and the individual coords per rabies case.
# x and y and the groupal coords for the reports.

### REPROJECT THE DATA ####

## Look for possible projections
EPSG <- make_EPSG() # object with all possible projections
EPSG[grep("Europe", EPSG$note), ] # search for projections assiciated with a word.
# we keep "+proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# CODE EPSG=5639, ISN2004 / LCC Europe

## Set longitude and latitude
longitude <- rabies.data.single.df$coords.x1
max(longitude); min(longitude)
#[1] 7221107
#[1] 2508469

latitude <- rabies.data.single.df$coords.x2
max(latitude); min(latitude)
#[1] 5076308
#[1] 1088140

## Define the coordinate systems
latlon_CRS <- CRS("+proj=longlat +ellps=WGS84")

## Call latlon_CRS
latlon_CRS
# Output: CRS arguments: +proj=longlat +ellps=WGS84

## Create spatial points
d.rabies <- SpatialPoints(cbind(x=rabies.data.single.df$coords.x1,
                                y=rabies.data.single.df$coords.x2))
d.rabies # features    : 248066 

## Plot the spatial points
plot(d.rabies)

## Create an object with the new projection for the spatial points
rabies_CRS <- CRS("+proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

## Convert the spatial points to a new object with the new projection
proj4string(d.rabies) <- rabies_CRS
d.rabies.new <- spTransform(d.rabies, latlon_CRS)
d.rabies.new

### Plot the new spatial points object over a low resolution map to confirm location
newmap <- getMap(resolution = "low")
plot(newmap)
points(d.rabies.new, col="red", cex=0.5)
# The location looks correct.

### CREATE A SPATIAL POINTS DATA FRAME OBJECT ###
rabies.data.spdf <- SpatialPointsDataFrame(d.rabies.new, rabies.data.single.df)
rabies.data <- as.data.frame(rabies.data.spdf)
str(rabies.data)
# 248066 obs. of  13 variables, including x.1 and y.1 (the new coordinates).


######################################################
# WHEN EXPORTING CREATE AND SAVE TO OUTPUT DIRECTORY - WILL HELP LATER!:
######################################################

### EXPORT DATA FRAMES (csv and shp) OBJECTS ###
#write.csv(rabies.data, file = "output/rabies.data.projected.csv")
#writeOGR(rabies.data.spdf, ".", "output/rabies.data.projected", driver="ESRI Shapefile")

## Confirm they work
#rabies <-readOGR("rabies.data.projected_.shp","rabies.data.projected_")
#rabies.data.single.df <- as.data.frame(rabies)
