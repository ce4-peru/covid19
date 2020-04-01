##'**MRES PROJECT**
##'
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'
##'**MICAELA DE LA PUENTE L.**
##'
##'**Create the countries boundaries shapefile (level 0 GADM)**
#########################################################################

## Call the packages
library(ggplot2)
library(ggmap)
library(sp)
library(rgdal)
library(raster)


#######   Get ONLINE DATA OF GLOBAL ADMINISTRATIVE AREAS - GADM  #########
#http://www.gis-blog.com/r-raster-data-acquisition/#
#http://www.chemie.fu-berlin.de/diverse/doc/ISO_3166.html
#http://www.guru-gis.net/get-administrative-topographic-and-climatic-data#-directly-with-r/#
# http://www.gadm.org/    # version is 2.8 (November 2015)

#######


#### Download borders of countries (level 0)

tdcero <- tempdir() # save the directory

## Call the GADM spdf
france0 <- getData('GADM', country=c('FRA'), level=0, path=tdcero)
austria0 <- getData('GADM', country=c('AUT'), level=0, path=tdcero)
germany0 <- getData('GADM', country=c('DEU'), level=0, path=tdcero)
switzerland0 <- getData('GADM', country=c('CHE'), level=0, path=tdcero)
belgium0 <- getData('GADM', country=c('BEL'), level=0, path=tdcero)
netherlands0 <- getData('GADM', country=c('NLD'), level=0, path=tdcero)
luxembourg0 <- getData('GADM', country=c('LUX'), level=0, path=tdcero)

# Some other for paper
uk0 <- getData('GADM', country=c('GBR'), level=0, path=tdcero)
italy0 <- getData('GADM', country=c('ITA'), level=0, path=tdcero)
slovenia0 <- getData('GADM', country=c('SVN'), level=0, path=tdcero)
hungary0 <- getData('GADM', country=c('HUN'), level=0, path=tdcero)
slovakia0 <- getData('GADM', country=c('SVK'), level=0, path=tdcero)
czechrep0 <- getData('GADM', country=c('CZE'), level=0, path=tdcero)
poland0 <- getData('GADM', country=c('POL'), level=0, path=tdcero)
denmark0 <- getData('GADM', country=c('DNK'), level=0, path=tdcero)


## Create row names
row.names(austria0) <- paste("austria0", row.names(austria0), sep="_")
row.names(germany0) <- paste("germany0", row.names(germany0), sep="_")
row.names(france0) <- paste("france0", row.names(france0), sep="_")
row.names(belgium0) <- paste("belgium0", row.names(belgium0), sep="_")
row.names(netherlands0) <- paste("netherlands0", row.names(netherlands0), sep="_")
row.names(switzerland0) <- paste("switzerland0", row.names(switzerland0), sep="_")
row.names(luxembourg0) <- paste("luxembourg0", row.names(luxembourg0), sep="_")


# Some other for paper
row.names(uk0) <- paste("uk0", row.names(uk0), sep="_")
row.names(italy0) <- paste("italy0", row.names(italy0), sep="_")
row.names(slovenia0) <- paste("slovenia0", row.names(slovenia0), sep="_")
row.names(hungary0) <- paste("hungary0", row.names(hungary0), sep="_")
row.names(slovakia0) <- paste("slovakia0", row.names(slovakia0), sep="_")
row.names(czechrep0) <- paste("czechrep0", row.names(czechrep0), sep="_")
row.names(poland0) <- paste("poland0", row.names(poland0), sep="_")
row.names(denmark0) <- paste("denmark0", row.names(denmark0), sep="_")


## Merge the shapefile in a single level 0 shp for all WE.
countries0_spdf <- rbind(france0, germany0, belgium0, switzerland0,
                         luxembourg0, netherlands0, austria0)
plot(countries0_spdf)

# Check information of countries
names(countries0_spdf)
#  [1] "OBJECTID"      "ID_0"          "ISO"           "NAME_0"        "OBJECTID_1"
# [6] "ISO3"          "NAME_ENGLISH"  "NAME_ISO"      "NAME_FAO"      "NAME_LOCAL"
# [11] "NAME_OBSOLETE" "NAME_VARIANTS" "NAME_NONLATIN" "NAME_FRENCH"   "NAME_SPANISH"
# [16] "NAME_RUSSIAN"  "NAME_ARABIC"   "NAME_CHINESE"  "WASPARTOF"     "CONTAINS"
# [21] "SOVEREIGN"     "ISO2"          "WWW"           "FIPS"          "ISON"
# [26] "VALIDFR"       "VALIDTO"       "POP2000"       "SQKM"          "POPSQKM"
# [31] "UNREGION1"     "UNREGION2"     "DEVELOPING"    "CIS"           "Transition"
# [36] "OECD"          "WBREGION"      "WBINCOME"      "WBDEBT"        "WBOTHER"
# [41] "CEEAC"         "CEMAC"         "CEPLG"         "COMESA"        "EAC"
# [46] "ECOWAS"        "IGAD"          "IOC"           "MRU"           "SACU"
# [51] "UEMOA"         "UMA"           "PALOP"         "PARTA"         "CACM"
# [56] "EurAsEC"       "Agadir"        "SAARC"         "ASEAN"         "NAFTA"
# [61] "GCC"           "CSN"           "CARICOM"       "EU"            "CAN"
# [66] "ACP"           "Landlocked"    "AOSIS"         "SIDS"          "Islands"
# [71] "LDC"

## Check the extent of coordinates
print(countries0_spdf)
# class       : SpatialPolygonsDataFrame
# features    : 7
# extent      : -5.143751, 17.16236, 41.33375, 55.05653  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0

## Reproject the spdf
proj4string(countries0_spdf) <- CRS("+init=epsg:5639") # not for paper
# Warning message:
#  In `proj4string<-`(`*tmp*`, value = <S4 object of class "CRS">) :
#  A new CRS was assigned to an object with an existing CRS:
#  +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
#  without reprojecting.
#  For reprojection, use function spTransform in package rgdal

## Plot again to check if the reprojection worked
plot(countries0_spdf) # shape of WE changed.



## DO it again for the paper
countries0_spdf1 <- rbind(france0, germany0, belgium0, switzerland0,
                         luxembourg0, netherlands0, austria0, czechrep0,
                         denmark0, italy0, hungary0, poland0, slovakia0,
                         slovenia0, uk0)
plot(countries0_spdf1)

## Check the extent of coordinates
print(countries0_spdf1)
# class       : SpatialPolygonsDataFrame
# features    : 7
# features    : 15
# extent      : -13.69139, 24.15328, 35.49292, 61.52708  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0

## Reproject the spdf
proj4string(countries0_spdf1) <- CRS("+init=epsg:5639")

## Plot again to check if the reprojection worked
plot(countries0_spdf1) # shape of WE changed.



##### CREATE AND EXPORT THE SHAPEFILE ####

## Create a directory
dir.create("tempdir")

## Export the shp
#writeOGR(obj=countries0_spdf, dsn="tempdir", layer="welevel0",
#         driver="ESRI Shapefile", overwrite=TRUE)

# writeOGR(obj=countries0_spdf1, dsn="tempdir", layer="welevel0_bond",
#          driver="ESRI Shapefile", overwrite=TRUE)
# Copy and paste them in Data/Boundaries data

## Upload the shapefile to check
setwd("output/Data/Boundaries data")
welevel0 <-readOGR("Data/Boundaries data/welevel0.shp",
                   "welevel0")
plot(welevel0)


## last one
countries0_spdf <- rbind(france0, germany0, belgium0, switzerland0,
                         luxembourg0, netherlands0, austria0)
c1 <- gUnaryUnion(countries0_spdf)
#pro <- projection(uk0)
#c1_new <- spTransform(c1, pro)
c1_new <- c1
attr <- data.frame(attr1 = 1, row.names = 1)
c1_new_spdf <- SpatialPolygonsDataFrame(c1_new, attr)
plot(c1_new_spdf)
# clean columns
uk0_ <- uk0[,c(1)]
italy0_ <- italy0[,c(1)]
slovenia0_ <- slovenia0[,c(1)]
slovakia0_ <- slovakia0[,c(1)]
poland0_ <- poland0[,c(1)]
czechrep0_ <- czechrep0[,c(1)]
denmark0_ <- denmark0[,c(1)]
hungary0_ <- hungary0[,c(1)]

#Some other for paper
row.names(c1_new_spdf) <- paste("c1_new_spdf", row.names(c1_new_spdf), sep="_")
row.names(uk0_) <- paste("uk0", row.names(uk0_), sep="_")
row.names(italy0_) <- paste("italy0_", row.names(italy0_), sep="_")
row.names(slovenia0_) <- paste("slovenia0_", row.names(slovenia0_), sep="_")
row.names(hungary0_) <- paste("hungary0_", row.names(hungary0_), sep="_")
row.names(slovakia0_) <- paste("slovakia0_", row.names(slovakia0_), sep="_")
row.names(czechrep0_) <- paste("czechrep0_", row.names(czechrep0_), sep="_")
row.names(poland0_) <- paste("poland0_", row.names(poland0_), sep="_")
row.names(denmark0_) <- paste("denmark0_", row.names(denmark0_), sep="_")

colnames(c1_new_spdf@data) <- colnames(uk0_@data) <- colnames(italy0_@data) <-
  colnames(slovenia0_@data) <- colnames(hungary0_@data) <-
  colnames(slovakia0_@data) <- colnames(slovakia0_@data) <-
  colnames(czechrep0_@data) <- colnames(poland0_@data) <-
  colnames(denmark0_@data) <- "NAME"

paper_pol <- rbind(c1_new_spdf, czechrep0_,
                          denmark0_, italy0_, hungary0_, poland0_, slovakia0_,
                          slovenia0_, uk0_)
plot(paper_pol)

# sAVE THE polygon
# WE map with border countries
proj4string(paper_pol) <- CRS("+init=epsg:5639")
writeOGR(obj=paper_pol, dsn="tempdir", layer="we_with_borders",
         driver="ESRI Shapefile", overwrite=TRUE)
# copy from tempdir to boundaries data
