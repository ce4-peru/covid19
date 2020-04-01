##'**MRES PROJECT**
##'
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'
##'**MICAELA DE LA PUENTE L.**
##'
##'**This script gets other countries boundaries shapefiles**
#############################################################

## Call the pacakages
library(ggplot2)
library(ggmap)
library(sp)
library(rgdal)
library(raster)


####### GLOBAL ADMINISTRATIVE AREAS - GADM ######


#### Download GADM level 1 (regions, provinces, states)

## Create a path to download
tdone <- tempdir() # save the directory

## Call the GADM spdf
france1 <- getData('GADM', country=c('FRA'), level=1, path=tdone) # 18 REGIONS
austria1 <- getData('GADM', country=c('AUT'), level=1, path=tdone) # 9 STATES
germany1 <- getData('GADM', country=c('DEU'), level=1, path=tdone)
switzerland1 <- getData('GADM', country=c('CHE'), level=1, path=tdone)
belgium1 <- getData('GADM', country=c('BEL'), level=1, path=tdone)
netherlands1 <- getData('GADM', country=c('NLD'), level=1, path=tdone)
luxembourg1 <- getData('GADM', country=c('LUX'), level=1, path=tdone)

## Set row names before merging
row.names(austria1) <- paste("austria1", row.names(austria1), sep="_")
row.names(germany1) <- paste("germany1", row.names(germany1), sep="_")
row.names(france1) <- paste("france1", row.names(france1), sep="_")
row.names(belgium1) <- paste("belgium1", row.names(belgium1), sep="_")
row.names(netherlands1) <- paste("netherlands1", row.names(netherlands1), sep="_")
row.names(switzerland1) <- paste("switzerland1", row.names(switzerland1), sep="_")
row.names(luxembourg1) <- paste("luxembourg1", row.names(luxembourg1), sep="_")

## Merge the spdf
welevel1 <- rbind(france1, germany1, belgium1, switzerland1, luxembourg1, netherlands1, austria1)
plot(welevel1)


#### Download GADM level 2 (deparments/districts)

## Create a path to download
tdtwo <- tempdir() # save the directory

## Call the GADM spdf (already downloaded)
france2 <- getData('GADM', country=c('FRA'), level=2, path=tdtwo) # 101 deps
austria2 <- getData('GADM', country=c('AUT'), level=2, path=tdtwo)
germany2 <- getData('GADM', country=c('DEU'), level=2, path=tdtwo)
switzerland2 <- getData('GADM', country=c('CHE'), level=2, path=tdtwo)
belgium2 <- getData('GADM', country=c('BEL'), level=2, path=tdtwo)
netherlands2 <- getData('GADM', country=c('NLD'), level=2, path=tdtwo)
luxembourg2 <- getData('GADM', country=c('LUX'), level=2, path=tdtwo)

## Set the rows names
row.names(austria2) <- paste("austria2", row.names(austria2), sep="_")
row.names(germany2) <- paste("germany2", row.names(germany2), sep="_")
row.names(france2) <- paste("france2", row.names(france2), sep="_")
row.names(belgium2) <- paste("belgium2", row.names(belgium2), sep="_")
row.names(netherlands2) <- paste("netherlands2", 
                                 row.names(netherlands2), sep="_")
row.names(switzerland2) <- paste("switzerland2", 
                                 row.names(switzerland2), sep="_")
row.names(luxembourg2) <- paste("luxembourg2", 
                                row.names(luxembourg2), sep="_")

## Merge the spdf
welevel2 <- rbind(france2, germany2, belgium2, switzerland2, luxembourg2, netherlands2, austria2)
plot(welevel2)



#### Download GADM level 3 (arrondissements, districts, municipalities and Communes)

## * Difference with Level 3 shapefiles of previous script if that this one does not includes Austria nor The Netherlands.

## Create a path to download
tdthree <- tempdir() # save the directory

## Call the GADM spdf
france3 <- getData('GADM', country=c('FRA'), level=3, path=tdthree) # 335 arrondissements
germany3 <- getData('GADM', country=c('DEU'), level=3, path=tdthree)
switzerland3 <- getData('GADM', country=c('CHE'), level=3, path=tdthree)
belgium3 <- getData('GADM', country=c('BEL'), level=3, path=tdthree)
luxembourg3 <- getData('GADM', country=c('LUX'), level=3, path=tdthree)

# Downloaded manually
austria3 <-readOGR("data/Boundaries data/AUT_adm3.shp", "AUT_adm3")
netherlands2 <-readOGR("data/Boundaries data/NLD_adm2.shp","NLD_adm2")

# Observe the column names
names(austria3@data)
# [1] "ID_0"   "ISO"  "NAME_0"    "ID_1"      "NAME_1"    "ID_2"    
# [7] "NAME_2" "ID_3" "NAME_3"    "CCN_3"     "CCA_3"     "TYPE_3"   
# [13] "ENGTYPE_3" "NL_NAME_3" "VARNAME_3"
names(belgium3@data)
# [1] "OBJECTID"  "ID_0"    "ISO"   "NAME_0"  "ID_1"    "NAME_1"   
# [7] "ID_2"      "NAME_2"  "ID_3"  "NAME_3"  "TYPE_3"  "ENGTYPE_3"
# [13] "NL_NAME_3" "VARNAME_3"
names(france3@data) # same as Belgium
names(germany3@data) # same as Belgium
names(luxembourg3@data)  # same as Belgium
names(netherlands2@data)
# [1] "OBJECTID" "ID_0"    "ISO"     "NAME_0"    "ID_1"      "NAME_1"   
# [7] "ID_2"    "NAME_2"  "TYPE_2"  "ENGTYPE_2" "NL_NAME_2" "VARNAME_2"
names(switzerland3@data)  # same as Belgium


## Keep same columns
belgium3_tobind <- belgium3[,c(2,4:12)]
france3_tobind <- france3[,c(2,4:12)]
germany3_tobind <- germany3[,c(2,4:12)]
luxembourg3_tobind <- luxembourg3[,c(2,4:12)]
switzerland3_tobind <- switzerland3[,c(2,4:12)]
austria3_tobind <- austria3[,c(1,3:9,12,13)]

netherlands2_tobind <-netherlands2[,c(2,4:10)]
netherlands2_tobind$ID_3 <- netherlands2_tobind$ID_2  
netherlands2_tobind$NAME_3 <- netherlands2_tobind$NAME_2
names(netherlands2_tobind)
netherlands2_tobind <-netherlands2_tobind[,c(1:6,9,10,7,8)]

#### Standarize the name of columns in all countries shapefiles
names(austria3_tobind) <-names(belgium3_tobind) <-names(france3_tobind) <-names(germany3_tobind) <-names(netherlands2_tobind) <-names(luxembourg3_tobind) <-names(switzerland3_tobind) <- c("ID_0", "NAME_0", "ID_1", "NAME_1", "ID_2", "NAME_2", "ID", "NAME", "TYPE", "ENGTYPE")

## Set the row names
row.names(germany3_tobind) <- paste("germany3_tobind", row.names(germany3_tobind), 
                                    sep="_")
row.names(france3_tobind) <- paste("france3_tobind", row.names(france3_tobind), 
                                   sep="_")
row.names(belgium3_tobind) <- paste("belgium3_tobind", row.names(belgium3_tobind), 
                                    sep="_")
row.names(switzerland3_tobind) <- paste("switzerland3_tobind", 
                                        row.names(switzerland3_tobind), sep="_")
row.names(luxembourg3_tobind) <- paste("luxembourg3_tobind", 
                                       row.names(luxembourg3_tobind), sep="_")
row.names(austria3_tobind) <- paste("austria3_tobind", 
                                       row.names(austria3_tobind), sep="_")
row.names(netherlands2_tobind) <- paste("netherlands2_tobind", 
                                    row.names(netherlands2_tobind), sep="_")


## Merge the spdf
welevel3 <- rbind(france3_tobind, germany3_tobind, belgium3_tobind, switzerland3_tobind, luxembourg3_tobind, austria3_tobind, netherlands2_tobind)
# Omit that Netherlands in level 2 for this purpose.
plot(welevel3)


#### Download GADM level 4 (municipalities)

## Create a path to download
tdfour <- tempdir() # save the directory

## Call the GADM spdf (already downloaded)
france4 <- getData('GADM', country=c('FRA'), level=4, path=tdfour)
belgium4 <- getData('GADM', country=c('BEL'), level=4, path=tdfour)
luxembourg4 <- getData('GADM', country=c('LUX'), level=4, path=tdfour)
# The Netherlands, Germany, Switzerland and Austria were not available at this level.

# Add manually
germany4 <-readOGR("data/Boundaries data/DEU_adm4.shp", "DEU_adm4")

# Already here
austria3
netherlands2
switzerland3

# Observe the column names
names(austria3@data)
# [1] "ID_0"   "ISO"  "NAME_0"    "ID_1"      "NAME_1"    "ID_2"    
# [7] "NAME_2" "ID_3" "NAME_3"    "CCN_3"     "CCA_3"     "TYPE_3"   
# [13] "ENGTYPE_3" "NL_NAME_3" "VARNAME_3"
names(belgium4@data)
# [1] "OBJECTID"  "ID_0"    "ISO"   "NAME_0"  "ID_1"    "NAME_1"     "ID_2"       
# [8] "NAME_2"    "ID_3"   "NAME_3"  "ID_4"   "NAME_4"  "VARNAME_4"  "TYPE_4"   
# [15]"ENGTYPE_4"
names(france4@data) # same as Belgium
names(luxembourg4@data)  # same as Belgium
names(germany4@data)
#[1] "ID_0"   "ISO"      "NAME_0"   "ID_1"     "NAME_1"      "ID_2"    "NAME_2"   
#[8] "ID_3"   "NAME_3"   "ID_4"     "NAME_4"   "VARNAME_4"   "CCN_4"   "CCA_4" 
#[15] "TYPE_4"    "ENGTYPE_4"
names(netherlands2@data)
# [1] "OBJECTID"   "ID_0"    "ISO"     "NAME_0"    "ID_1"      "NAME_1"   
# [7] "ID_2"       "NAME_2"  "TYPE_2"  "ENGTYPE_2" "NL_NAME_2" "VARNAME_2"
names(switzerland3@data)  # same as Belgium
#[1] "OBJECTID"  "ID_0"  "ISO"     "NAME_0"   "ID_1"      "NAME_1"    "ID_2"     
#[8] "NAME_2"    "ID_3"  "NAME_3"  "TYPE_3"   "ENGTYPE_3" "NL_NAME_3" "VARNAME_3"

## Slct rows
austria3_tobind <-austria3[,c(1,3:5,8,9,12:13)]
belgium4_tobind <-belgium4[,c(2,4:6,11,12,14,15)]
france4_tobind <-france4[,c(2,4:6,11,12,14,15)]
luxembourg4_tobind <-luxembourg4[,c(2,4:6,11,12,14,15)]
germany4_tobind <-germany4[,c(1,3:5,10,11,15:16)]
switzerland3_tobind <-switzerland3[,c(2,4:6,9:12)]
netherlands2_tobind <-netherlands2[,c(2,4:10)]

#### Standarize the name of columns in all countries shapefiles
names(austria3_tobind) <-names(belgium4_tobind) <-names(france4_tobind) <-names(germany4_tobind) <-names(netherlands2_tobind) <-names(luxembourg4_tobind) <-names(switzerland3_tobind) <- c("ID_0", "NAME_0", "ID_1", "NAME_1", "ID", "NAME", "TYPE", "ENGTYPE")

## Name rows
row.names(germany4_tobind) <- paste("germany4_tobind", row.names(germany4_tobind), 
                                    sep="_")
row.names(france4_tobind) <- paste("france4_tobind", row.names(france4_tobind), 
                                   sep="_")
row.names(belgium4_tobind) <- paste("belgium4_tobind", row.names(belgium4_tobind), 
                                    sep="_")
row.names(switzerland3_tobind) <- paste("switzerland3_tobind", 
                                        row.names(switzerland3_tobind), sep="_")
row.names(luxembourg4_tobind) <- paste("luxembourg4_tobind", 
                                       row.names(luxembourg4_tobind), sep="_")
row.names(austria3_tobind) <- paste("austria3_tobind", 
                                    row.names(austria3_tobind), sep="_")
row.names(netherlands2_tobind) <- paste("netherlands2_tobind", 
                                        row.names(netherlands2_tobind), sep="_")

## Merge the spdf
welevel4 <- rbind(france4_tobind, germany4_tobind, belgium4_tobind, switzerland3_tobind, luxembourg4_tobind, austria3_tobind, netherlands2_tobind)
# Omit that Netherlands in level 2 for this purpose.
plot(welevel4)


## check Info over countries
names(welevel1)
#[1] "OBJECTID"  "ID_0"   "ISO"   "NAME_0"    "ID_1"   "NAME_1"    "TYPE_1"
#[8] "ENGTYPE_1" "NL_NAME_1" "VARNAME_1"

names(welevel2)
#[1] "OBJECTID"  "ID_0"   "ISO"   "NAME_0"    "ID_1"    "NAME_1"    "ID_2"
#[8] "NAME_2"    "TYPE_2"    "ENGTYPE_2" "NL_NAME_2" "VARNAME_2"

names(welevel3)
# [1] "ID_0"    "NAME_0"  "ID_1"    "NAME_1"  "ID_2"    "NAME_2"  "ID"      
# [8] "NAME"    "TYPE"    "ENGTYPE"

names(welevel4)
# [1] "ID_0"   "NAME_0"  "ID_1"   "NAME_1"  "ID"   "NAME"    "TYPE"  "ENGTYPE"


## Check coords.
print(welevel1)
print(welevel2)
print(welevel3)
print(welevel4)
# Coords is +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0

## Reproject the spdf
proj4string(welevel1) <- CRS("+init=epsg:5639")
proj4string(welevel2) <- CRS("+init=epsg:5639")
proj4string(welevel3) <- CRS("+init=epsg:5639")
proj4string(welevel4) <- CRS("+init=epsg:5639")

## Plot to see if the reprojection worked
plot(welevel1) # shape changes
plot(welevel2) # shape changes
plot(welevel3) # shape changes
plot(welevel4) # shape changes

## Create and export the shapefiles
#writeOGR(obj=welevel1, dsn="output", layer="welevel1_spdf",
#         driver="ESRI Shapefile", overwrite = TRUE)
#writeOGR(obj=welevel2, dsn="output", layer="welevel2_spdf",
#         driver="ESRI Shapefile", overwrite = TRUE)
#writeOGR(obj=welevel3, dsn="output", layer="welevel3_spdf",
#         driver="ESRI Shapefile", overwrite = TRUE)
#Warning 1: One or several characters couldn't be converted correctly from UTF-8 to ISO-8859-1. This warning will not be emitted anymore.
#writeOGR(obj=welevel4, dsn="output", layer="welevel4_spdf",
#         driver="ESRI Shapefile", overwrite = TRUE)
# Warning 1: One or several characters couldn't be converted correctly from UTF-8 to ISO-8859-1. This warning will not be emitted anymore.

# To re-write change tempdir and name of layer


## Upload level 1, 2 an 4 created shp.
welevel0 <-readOGR("data/Boundaries data/welevel0.shp", "welevel0")
welevel1 <-readOGR("output/welevel1_spdf.shp", "welevel1_spdf")
welevel2 <-readOGR("output/welevel2_spdf.shp", "welevel2_spdf")
welevel3 <-readOGR("output/welevel3_spdf.shp", "welevel3_spdf")
welevel4 <-readOGR("output/welevel4_spdf.shp", "welevel4_spdf")
