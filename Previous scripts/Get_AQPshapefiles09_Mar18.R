# This code explore formato 2 of the chiris project
# Call packages
library(ggplot2)
library(data.table)
library(raster)
#library(car)
library(dplyr)
library(plyr)
#install.packages("tmap")
library(tmap)
library(rgdal)

setwd("C:/Users/Micaela/Documents/covid19PE-LIEZ")

### CALL SHAPEFILES ###
## DISTRICT AND BEYOND...
tdcero <- tempdir() # save the directory
peru0 <- getData('GADM', country=c('PER'), level=0, path=tdcero)
peru1 <- getData('GADM', country=c('PER'), level=1, path=tdcero)
peru2 <- getData('GADM', country=c('PER'), level=2, path=tdcero)
peru3 <- getData('GADM', country=c('PER'), level=3, path=tdcero)
#peru4 <- getData('GADM', country=c('PER'), level=4, path=tdcero)
# no level 4

# Plot shapefiles
plot(peru0)
plot(peru1)
plot(peru2)
plot(peru3)

# get df
peru1_df <- peru1@data
peru2_df <- peru2@data
peru3_df <- peru3@data

## check variables in gadm2 - provinces
names(peru2@data)
# [1] "OBJECTID"  "ID_0"      "ISO"       "NAME_0"    "ID_1"      "NAME_1"   
# [7] "ID_2"      "NAME_2"    "HASC_2"    "CCN_2"     "CCA_2"     "TYPE_2"   
# [13] "ENGTYPE_2" "NL_NAME_2" "VARNAME_2"
head(peru2@data, 3)
# OBJECTID ID_0 ISO NAME_0 ID_1   NAME_1 ID_2      NAME_2   HASC_2 CCN_2 CCA_2
# 1        1  178 PER   Peru    1 Amazonas    1       Bagua PE.AM.BG    NA      
# 2        2  178 PER   Peru    1 Amazonas    2     Bongará PE.AM.BN    NA      
# 3        3  178 PER   Peru    1 Amazonas    3 Chachapoyas PE.AM.CP    NA      
# TYPE_2 ENGTYPE_2 NL_NAME_2 VARNAME_2
# 1 Provincia  Province                    
# 2 Provincia  Province             Bongart
# 3 Provincia  Province  

# Export

## Create a directory
dir.create("tempdir")

# writeOGR(obj=peru0, dsn="tempdir", layer="peru_0",
#          driver="ESRI Shapefile", overwrite=TRUE)
# writeOGR(obj=peru1, dsn="tempdir", layer="peru_1",
#          driver="ESRI Shapefile", overwrite=TRUE)
# writeOGR(obj=peru2, dsn="tempdir", layer="peru_2",
#          driver="ESRI Shapefile", overwrite=TRUE)
# writeOGR(obj=peru3, dsn="tempdir", layer="peru_3",
#          driver="ESRI Shapefile", overwrite=TRUE)

# Keep Lima only. It has Lima with its XX provinces
LIM_prov <-  peru2[peru2$ID_1 %in% c(15,16),]
LIM_prov_df <- LIM_prov@data
plot(LIM_prov) # OK
writeOGR(LIM_prov, dsn=".", layer="data/LIM_region_provinces", 
         driver="ESRI Shapefile")

## check variables in gadm3 - district levels
names(peru3@data)
head(peru3@data, 3)
# OBJECTID ID_0 ISO NAME_0 ID_1   NAME_1 ID_2 NAME_2 ID_3   NAME_3 CCN_3 CCA_3
# 1        1  178 PER   Peru    1 Amazonas    1  Bagua    1 Aramango    NA      
# 2        2  178 PER   Peru    1 Amazonas    1  Bagua    2 Copallin    NA      
# 3        3  178 PER   Peru    1 Amazonas    1  Bagua    3 El Parco    NA      
# TYPE_3 ENGTYPE_3 NL_NAME_3 VARNAME_3
# 1 Distrito  District                    
# 2 Distrito  District                    
# 3 Distrito  District   

# Keep ONLY districts in Lima region
LIM_districts <-  peru3[peru3$ID_1 %in% c(15,16),]
LIM_districts_df <- LIM_districts@data
plot(LIM_districts) # OK
# writeOGR(LIM_districts, dsn=".", layer="data/LIM_region_district", 
#          driver="ESRI Shapefile")

# Keep ONLY districts in Lima province
LIM_prov_districts <-  peru3[peru3$ID_1 == 15,]
LIM_prov_districts_df <- LIM_prov_districts@data
plot(LIM_prov_districts) # OK
# writeOGR(LIM_prov_districts, dsn=".", layer="data/LIM_province_district", 
#          driver="ESRI Shapefile")

# Keep ONLY Lima province, border
LIM_LIM_prov <-  peru2[peru2$ID_1 == 15,]
LIM_LIM_prov_df <- LIM_prov@data
plot(LIM_LIM_prov) # OK
# writeOGR(LIM_LIM_prov, dsn=".", layer="data/LIM_LIM_prov", 
#          driver="ESRI Shapefile")


# Plotear
plot(LIM_prov_districts, col="ivory2")
plot(LIM_prov, add=TRUE, lwd=2)
#plot(peru0, add=TRUE, lwd=3)
box()

# Plot with districts to check
qtm(LIM_prov_districts, fill="white") + 
  tm_text("NAME_3")
# OK

## Explore more the shapefile that we need
names(AQP_AQP_districts_df)
# [1] "OBJECTID"  "ID_0"      "ISO"       "NAME_0"    "ID_1"      "NAME_1"   
# [7] "ID_2"      "NAME_2"    "ID_3"      "NAME_3"    "CCN_3"     "CCA_3"    
# [13] "TYPE_3"    "ENGTYPE_3" "NL_NAME_3" "VARNAME_3" "fNAME_3"  
str(AQP_AQP_districts_df) 
# $ ID_3     : int  330 331 332 333 334 335 336 337 338 339 ...
# $ NAME_3   : chr  "Alto Selva Alegre" "Arequipa" "Cayma" "Cerro Colorado" ...

unique(sort(AQP_AQP_districts_df$ID_3))
# [1] 330 331 332 333 334 335 336 337 338 339 340 341 342 343 344 345 346 347 348 349
# [21] 350 351 352 353 354 355 356 357 358 359
levels(as.factor(AQP_AQP_districts_df$NAME_3))

# Recode names
AQP_AQP_districts$NAME_3 <- recode(AQP_AQP_districts$NAME_3,  
                                   `Alto Selva Alegre` = "A.S.A.", 
                                   `Cayma` = "CAYMA", 
                                   `Cerro Colorado` = "C. COLORADO", 
                                   `Jacobo Hunter` = "HUNTER", 
                                   `Jose Luis Bustamante Y Rivero` = "J. L. B Y R", 
                                   `Mariano Melgar` = "M. MELGAR", 
                                   `Miraflores` = "MIRAFLORES", 
                                   `Paucarpata` = "PAUCARPATA",
                                   `Sachaca` = "SACHACA", 
                                   `Socabaya` = "SOCABAYA",
                                   `Uchumayo` = "UCHUMAYO",
                                   `Yura` = "YURA",
                                   `Arequipa` = "AREQUIPA",
                                   `Characato` = "CHARACATO",
                                   `Chiguata` = "CHIGUATA",
                                   `La Joya` = "LA JOYA",
                                   `Laguna Loriscota` = "L. LORISCOTA",
                                   `Mollebaya` = "MOLLEBAYA",
                                   `Pocsi` = "POCSI",
                                   `Polobaya` = "POLOBAYA",
                                   `Quequeña` = "QUEQUEÑA",
                                   `Sabandia` = "SABANDIA",
                                   `San Juan de Siguas` = "S.J. Siguas",
                                   `San Juan de Tarucani` = "S.J. Tarucani",
                                   `Santa Isabel de Siguas` = "S.I. de Siguas",
                                   `Santa Rita de Siguas` = "S.R. de Siguas",
                                   `Tiabaya` = "TIABAYA",
                                   `Vitor` = "VITOR",
                                   `Yanahuara` = "YANAHUARA",
                                   `Yarabamba` = "YARABAMBA")
AQP_AQP_districts_df <- AQP_AQP_districts@data

# Plot again
qtm(AQP_AQP_districts, fill="white") + 
  tm_text("NAME_3")
# OK


### Create shapefiles of districts that we like

districts.i <- c("A.S.A.", "CAYMA", "C. COLORADO", "CHARACATO", "HUNTER",
                 "J. L. B Y R", "LA JOYA", "M. MELGAR", "MIRAFLORES",
                 "MOLLEBAYA", "PAUCARPATA", "SACHACA", "UCHUMAYO",
                 "TIABAYA", "SOCABAYA")

# for(i in districts.i){
#   print(i)
#   AQP_AQP_districts.i <- AQP_AQP_districts[AQP_AQP_districts$NAME_3 == i,]
#   AQP_AQP_districts.i_df <- AQP_AQP_districts.i@data
#   plot(AQP_AQP_districts.i)
#   writeOGR(AQP_AQP_districts.i, dsn=".", 
#            layer=paste("output/Casos_INS/Scripts/outputs/", i, "_district", sep=""), 
#            driver="ESRI Shapefile")
#   }
#   