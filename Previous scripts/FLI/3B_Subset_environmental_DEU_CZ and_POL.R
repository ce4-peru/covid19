##'**LAURIE'S PROJECT**
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'**MICAELA DE LA PUENTE L.**
##'**This script shows subset of rabies data**

######################################################
# FIRST SET WORKING DIRECTORY IN RSTUDIO
# i.e. Session > Set Working Directory > To Source File Location
rm(list=ls())

#####################
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

####################
# Call the level 0 shapefile to use for plotting
countries <-readOGR("output/nuts_l0_subset_three.shp", 
                 "nuts_l0_subset_three") 
plot(countries)


### URBAN AREAS - SPATIAL POLYGONS DATAFRAME ###

urban <-readOGR("Variables_Europe/Urban_Area.shp", "Urban_Area")

# Select urban areas inside Germany, Czech Republic and Poland only
urban1 <- intersect(urban, countries) # takes some time
str(urban1@data)
head(urban1@data)
urban2 <- urban1
urban2@data <- urban2@data[, c(1:6)]
str(urban2@data)


png("output/urban_areas_three.png", width=6, height=4, units="in", res=300)
plot(urban2, col="red", border="red", main="Urban areas in Germany, CZ and POL",
     axes=TRUE)
plot(countries, add=TRUE)
dev.off()

#writeOGR(urban2, dsn=".", layer="output/urban_areas_three", driver="ESRI Shapefile")



## LARGE LAKES
large_lakes <-readOGR("Variables_Europe/Lakes_large.shp", "Lakes_large")

#subset lakes inside Germany, Poland and Czech Republic only
#compute intersection
large_lakes1 <- intersect(large_lakes, countries) # takes some 
str(large_lakes1@data)
head(large_lakes1@data)
large_lakes1@data <- large_lakes1@data[, c(1:6)]
str(large_lakes1@data)


png("output/lakes_large_three.png", width=6, height=4, units="in", res=300)
plot(large_lakes1, col="blue", border="blue2", 
     main="Large lakes in Germany, CZ and POL", axes=TRUE)
plot(countries, add=TRUE)
dev.off()
#writeOGR(large_lakes1, dsn=".", layer="output/lakes_large_three", driver="ESRI Shapefile")



### 
## BIG RIVERS
# Call shapefile of big rivers, created previously
rivers_large <-readOGR("output/rivers_large.shp", "rivers_large")

#subset
#compute intersection
rivers_large1 <- intersect(rivers_large, countries) # takes some 
str(rivers_large1@data)
head(rivers_large1@data)
rivers_large1@data <- rivers_large1@data[, c(1:4)]
str(rivers_large1@data)


png("output/rivers_large_three.png", width=6, height=4, units="in", res=300)
plot(countries, main="Large rivers in Germany, CZ and POL", axes=TRUE)
plot(rivers_large1, col="blue", add=TRUE)
dev.off()
#writeOGR(rivers_large1, dsn=".", layer="output/rivers_large_three", 
#         driver="ESRI Shapefile")



### Elevation ###

altitude <-raster("output/altitude_raster.tif")
class(altitude) # [1] "RasterLayer"
plot(altitude)

# Crop only Germany, Poland and czech Republic altitudes
altitude_raster <- rasterize(countries,altitude, mask=TRUE) 
plot(altitude_raster)

# Keep only occupied space
altitude_raster1 <- altitude_raster
ext <- extent(countries)
altitude_raster1 <- crop(altitude_raster1, ext)

png("output/altitude_raster_three.png", width=6, height=4, units="in", res=300)
plot(altitude_raster1)
dev.off()

writeRaster(altitude_raster1, filename="output/altitude_raster_three.tif", 
            format="GTiff", overwrite=TRUE)
