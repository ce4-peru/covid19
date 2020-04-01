##'**MRES PROJECT**
##'
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'
##'**MICAELA DE LA PUENTE L.**        
##'          
##'**This script creates GADM raster for the countries in WE** 
##############################################################

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

## Import the land use and elevation rasters
land.raster <- raster("output/new_raster2_nasa2000.tif")
plot(land.raster)

## Call shapefiles of countries GADMs
gadm_level0 <-readOGR("data/Boundaries data/welevel0.shp", "welevel0")
gadm_level1 <-readOGR("output/welevel1_spdf.shp", "welevel1_spdf")
gadm_level2 <-readOGR("output/welevel2_spdf.shp", "welevel2_spdf")
gadm_level3 <-readOGR("output/welevel3_spdf.shp", "welevel3_spdf")
gadm_level4 <-readOGR("output/welevel4_spdf.shp", "welevel4_spdf")


#############    RASTER OF WESTERN EUROPE  #################

## Create the raster of administrative units level 0 - COUNTRY LEVEL

## Plot the raster and shp
plot(land.raster, col=c("mediumorchid4", "cyan2", "yellow2", "chocolate1", 
                        "forestgreen", "bisque3"))
plot(gadm_level0, add=TRUE)

## Rasterize
raster_gadm_level0 <- rasterize(gadm_level0, land.raster, 
                             field=gadm_level0$NAME_0) # country name
raster1_gadm_level0 <- mask(raster_gadm_level0, 
                             land.raster)

raster1_gadm_level0
# dimensions  : 830, 1350, 1120500 
# values      : 1, 7  (min, max)

freq(raster1_gadm_level0)
#      value count
#[1,]     1  36086
#[2,]     2  14029
#[3,]     3 230375
#[4,]     4 164363
#[5,]     5   1153
#[6,]     6  17340
#[7,]     7  17618
#[8,]    NA 639536
# Alphabetic order

plot(raster1_gadm_level0)

## Select just some
breaks1 <- c(0,1,2,3,4,5,6,7)
colors <- c("black","red","black","black","black","black","black") # just Blgium in red
plot(raster1_gadm_level0,breaks=breaks1,col=colors)

# Save GADM raster
writeRaster(raster1_gadm_level0, filename="output/gadm_level0_raster.tif", 
            format="GTiff", overwrite=TRUE)



## Create the raster of administrative units level 1 - RGION LEVEL

## Plot the raster and shp
plot(land.raster, col=c("mediumorchid4", "cyan2", "yellow2", "chocolate1", 
                        "forestgreen", "bisque3"))
plot(gadm_level1, add=TRUE)


## Rasterize
raster_gadm_level1 <- rasterize(gadm_level1, land.raster, 
                                field=gadm_level1$NAME_1) # country name
raster1_gadm_level1 <- mask(raster_gadm_level1, 
                            land.raster)

raster1_gadm_level1
# dimensions  : 830, 1350, 1120500 
# values      : 1, 93  (min, max)

freq(raster1_gadm_level1)
# [90,]    NA 639536

plot(raster1_gadm_level1)

# Save GADM raster
writeRaster(raster1_gadm_level1, filename="output/gadm_level1_raster.tif", format="GTiff", overwrite=TRUE)



## Create the raster of administrative units level 2 - SUBRGION LEVEL

## Plot the raster and shp
plot(land.raster, col=c("mediumorchid4", "cyan2", "yellow2", "chocolate1", 
                        "forestgreen", "bisque3"))
plot(gadm_level2, add=TRUE)


## Rasterize
raster_gadm_level2 <- rasterize(gadm_level2, land.raster, 
                                field=gadm_level2$NAME_2) # country name
raster1_gadm_level2 <- mask(raster_gadm_level2, 
                            land.raster)

raster1_gadm_level2
# dimensions  : 830, 1350, 1120500 
# values      : 1, 912 (min, max)

freq(raster1_gadm_level2)
# [911,]    NA 640289
plot(raster1_gadm_level2)

# Save GADM raster
writeRaster(raster1_gadm_level2, filename="output/gadm_level2_raster.tif", format="GTiff", overwrite=TRUE)


## Create the raster of administrative units level 3 - DISTRICT LEVEL

## Plot the raster and shp
plot(land.raster, col=c("mediumorchid4", "cyan2", "yellow2", "chocolate1", 
                        "forestgreen", "bisque3"))
plot(gadm_level3, add=TRUE)


## Rasterize
raster_gadm_level3 <- rasterize(gadm_level3, land.raster, 
                                field=gadm_level3$NAME) # country name
raster1_gadm_level3 <- mask(raster_gadm_level3, 
                            land.raster)

raster1_gadm_level3
# dimensions  : 830, 1350, 1120500 
# values      : 1, 6204  (min, max)

sum(is.na(as.data.frame(raster1_gadm_level3)))
# [1] 640330

plot(raster1_gadm_level3)

# Save GADM raster
# writeRaster(raster1_gadm_level3, filename="output/gadm_level3_raster.tif", format="GTiff", overwrite=TRUE)



## Create the raster of administrative units level 4 - MUNICIPALITY LEVEL

## Plot the raster and shp
plot(land.raster, col=c("mediumorchid4", "cyan2", "yellow2", "chocolate1", 
                        "forestgreen", "bisque3"))
plot(gadm_level4, add=TRUE)


## Rasterize
raster_gadm_level4 <- rasterize(gadm_level4, land.raster, 
                                field=gadm_level4$NAME) # country name
raster1_gadm_level4 <- mask(raster_gadm_level4, 
                            land.raster)

raster1_gadm_level4
# dimensions  : 830, 1350, 1120500 
# values      : 1, 20227  (min, max)

sum(is.na(as.data.frame(raster1_gadm_level4)))
# NA [1] 640574

plot(raster1_gadm_level4)

# Save GADM raster
# writeRaster(raster1_gadm_level4, filename="output/gadm_level4_raster.tif", 
#             format="GTiff", overwrite=TRUE)