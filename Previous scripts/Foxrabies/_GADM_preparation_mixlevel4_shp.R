##'**MRES PROJECT**
##'
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'
##'**MICAELA DE LA PUENTE L.**
##'
##'**This script gets the countries boundaries shapefiles at level 3**
######################################################################

## Call the pacakages
library(ggplot2)
library(ggmap)
library(sp)
library(rgdal)
library(raster)


############ CREATE A SINGLE LEVEL 3 SHP  ###############

## Upload level3 shp of all WE countries

#austria3<-readOGR("austria3.shp","austria3")
austria2<-readOGR("data/Boundaries data/austria2.shp","austria2")
france3<-readOGR("data/Boundaries data/france3.shp","france3")
belgium3<-readOGR("data/Boundaries data/belgium3.shp","belgium3")
#luxembourg3<-readOGR("luxembourg3.shp","luxembourg3")
luxembourg2<-readOGR("data/Boundaries data/luxembourg2.shp","luxembourg2")
#switzerland3<-readOGR("switzerland3.shp","switzerland3")
switzerland2<-readOGR("data/Boundaries data/switzerland2.shp","switzerland2")
netherlands2<-readOGR("data/Boundaries data/netherlands2.shp","netherlands2")
germany4<-readOGR("output/Germany/germany4_shp.shp","germany4_shp")

## Observe the column names
#names(austria3@data)
names(austria2@data)
names(belgium3@data)
names(france3@data)
#names(luxembourg3@data)
names(luxembourg2@data)
names(netherlands2@data)
#names(switzerland3@data)
names(switzerland2@data)
names(germany4@data)

# Austria, Luxembourg and Switzerland have same number of columns. Belgium, France have the same number and Netherlands differs to all the others. Germany is OK already.


## Standarize the number of columns
belgium3_tobind <- belgium3[,c(9:12,4)]
france3_tobind <- france3[,c(9:12,4)]

austria2_tobind <- austria2[,c(7:10,4)]
luxembourg2_tobind <- luxembourg2[,c(7:10,4)]
switzerland2_tobind <- switzerland2[,c(7:10,4)]

netherlands2_tobind <- netherlands2[,c(6,7,12,13,3)]

germany4_tobind <- germany4


#### Standarize the name of columns in all countries shapefiles

names(austria2_tobind) <- names(belgium3_tobind) <- names(france3_tobind) <- names(netherlands2_tobind) <- names(luxembourg2_tobind) <- names(switzerland2_tobind) <- c("ID", "NAME", "TYPE", "ENGTYPE", "COUNTRY")


#### Create the rows names for the single shapefile
row.names(austria2_tobind) <- paste("austria2_tobind", row.names(austria2_tobind), sep="_")
row.names(belgium3_tobind) <- paste("belgium3_tobind", row.names(belgium3_tobind), sep="_")
row.names(france3_tobind) <- paste("france3_tobind", row.names(france3_tobind), sep="_")
row.names(luxembourg2_tobind) <- paste("luxembourg2_tobind", row.names(luxembourg2_tobind), sep="_")
row.names(netherlands2_tobind) <- paste("netherlands2_tobind", row.names(netherlands2_tobind), sep="_")
row.names(switzerland2_tobind) <- paste("switzerland2_tobind", row.names(switzerland2_tobind), sep="_")
row.names(germany4_tobind) <- paste("germany4_tobind", row.names(germany4_tobind), sep="_")


## Merge in a single spdf of Level 3
countries3_spdf_v3 <- rbind(france3_tobind, germany4_tobind, belgium3_tobind, switzerland2_tobind, luxembourg2_tobind, austria2_tobind, netherlands2_tobind)

## Plot to see if it worked
plot(countries3_spdf_v3)

#### Create and export the shapefiles ####
dir.create("tempdir")
writeOGR(obj=countries3_spdf_v3, dsn="output", layer="countrieslevel3_spdf_v3", driver="ESRI Shapefile", overwrite_layer = TRUE)



#######  CREATE SOME MAPS  #########

## Read level 3 SHP
level3_spdf_v3 <-readOGR("output/countrieslevel3_spdf_v3.shp","countrieslevel3_spdf_v3")

## Plot assigning a color to each country
#pdf("GADMlevel3_color.pdf", onefile = TRUE, width = 13, height = 7)
plot(level3_spdf_v3)
plot(austria2, add=TRUE, col="red")
plot(netherlands2, add=TRUE, col="orange")
plot(switzerland2, add=TRUE, col="grey")
plot(luxembourg2, add=TRUE, col="green")
plot(germany4, add=TRUE, col="yellow")
plot(france3, add=TRUE, col="blue")
plot(belgium3, add=TRUE, col="purple")
#dev.off()

# Colors for countries are: Austria (red), Belgium(purple), France (blue), Germany, Luxembourg(), Switzerland(grey), The Netherlands(orange).
