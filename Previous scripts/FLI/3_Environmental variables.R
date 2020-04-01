##'**LAURIE'S PROJECT**
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'**MICAELA DE LA PUENTE L.**
##'**This script shows subset of rabies data**

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
library(raster)
######################################################

# Plot map of level 0 to use as background
europe <-readOGR("Administrative Shape Database (Rabigramm)/Europe_Nuts0_WGS84.shp", 
                    "Europe_Nuts0_WGS84") 
#plot(europe)


### URBAN AREAS - SPATIAL POLYGONS DATAFRAME ###

# Call the shapefile
urban <-readOGR("Variables_Europe/Urban_Area.shp", "Urban_Area")
class(urban) #[1] "SpatialPolygonsDataFrame"

## Check the data attributes
names(urban)
#[1] "Name"       "Name_ASCII" "Shape_STAr" "Shape_STLe"

head(urban@data)
#          Name  Name_ASCII   Shape_STAr Shape_STLe
#0     Elbasan     Elbasan 0.0003628602 0.07707617
#1        Fier        Fier 0.0004916029 0.09937361
#2 Gjirokaster Gjirokaster 0.0002636111 0.06742202
#3       Korce       Korce 0.0004570599 0.08833639
#4     Shkoder     Shkoder 0.0011654430 0.13204342
#5      Tirane      Tirane 0.0011047932 0.14970587

# Chech number of urban areas
length(levels(unique(urban$Name))) # [1] 12751 levels

## Check projection
str(urban)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

str(urban@data)
# 'data.frame':	13516 obs. of  4 variables:
#$ Name      : Factor w/ 12751 levels "'s-Gravenhage (The Hague)",..: 2948 3267 3702 5585 10110 11085 11865 316 360 564 ...
#$ Name_ASCII: Factor w/ 12743 levels "'s-Gravenhage (The Hague)",..: 2900 3221 3645 5537 10070 11063 11852 255 303 509 ...
#$ Shape_STAr: num  0.000363 0.000492 0.000264 0.000457 0.001165 ...
#$ Shape_STLe: num  0.0771 0.0994 0.0674 0.0883 0.132 ...

# Only appear cities but not the countries where this cities belong to.

png("output/urban_areas.png", 
    width=9, height=6, units="in", res=300)
plot(urban, col="red", border="red", main="Urban areas in Europe", axes=TRUE)
plot(europe, add=TRUE)
dev.off()



### CITIES - SPATIAL POINTS DATAFRAME ###

## BIG CITIES
city_large <-readOGR("Variables_Europe/City_large.shp", "City_large")

## Check the data attributes
names(city_large)
#[1] "Name"       "Name_ASCII" "Population" "Status"

head(city_large,3)
#    coordinates            Name              Name_ASCII         Population
#1  (6.13208, 49.61136)     Luxembourg        Luxembourg         50.000 - 100.000
#2  (1.545109, 42.51068)    Andorra la Vella  Andorra la Vella   50.000 - 100.000
#3  (-6.783205, 62.01052)   Tórshavn         Torshavn           <NA>
#   Status
#1  National and provincial capital
#2                National capital
#3  National and provincial capital

# Number of big cities
levels(unique(city_large$Name)) # 748 levels

## Check projection
str(city_large)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

str(city_large@data)
# 'data.frame':	749 obs. of  6 variables:
#$ Name      : Factor w/ 748 levels "'s-Gravenhage",..: 370 29 644 694 734 51 658 433 407 226 ...
#$ Name_ASCII: Factor w/ 748 levels "'s-Gravenhage",..: 367 24 667 692 733 46 656 427 400 223 ...
#$ Population: Factor w/ 4 levels ">100.000","10.000 - 20.000",..: 4 4 NA NA NA NA NA 2 3 1 ...
#$ Status    : Factor w/ 3 levels "National and provincial capital"

# Check division of the shapefile
levels(city_large@data$Population)
#[1] ">100.000"   "10.000 - 20.000"  "20.000 - 50.000"  "50.000 - 100.

# Only appear cities but not the countries where this cities belong to.

png("output/city_large.png", 
    width=6, height=6, units="in", res=300)
plot(city_large, pch=21, cex=0.4, col="red", main="Large cities in Europe",
     axes=TRUE)
plot(europe, add=TRUE)
dev.off()


## MEDIUM CITIES
city_medium <-readOGR("Variables_Europe/City_medium.shp", "City_medium")

## Check the data attributes
names(city_medium)
#[1] "Name"       "Name_ASCII" "Population" "Status"

head(city_medium)
#            coordinates       Name Name_ASCII       Population             Status
#1 (20.78519, 40.61988)    Korçë      Korce 50.000 - 100.000 Provincial capital
#2 (19.51443, 42.06891)   Shkodër    Shkoder 50.000 - 100.000 Provincial capital
#3 (19.49049, 40.47297)     Vlorë      Vlore 50.000 - 100.000 Provincial capital
#4  (14.3034, 46.62555) Klagenfurt Klagenfurt 50.000 - 100.000 Provincial capital
#5 (14.03102, 48.15818)       Wels       Wels 50.000 - 100.000               <NA>
#6 (13.84448, 46.61718)    Villach    Villach 50.000 - 100.000               <NA>
  
# Number of medium cities
levels(unique(city_medium$Name)) # 1109 levels

## Check projection
str(city_medium)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

str(city_medium@data)
# data.frame':	1116 obs. of  6 variables:
#$ Name      : Factor w/ 1109 levels "�-demis","�-rnsköldsvik",..: 501 868 1050 487 1067 1040 1101 1102 491 575 ...
#$ Name_ASCII: Factor w/ 1108 levels "Aalen","Aalst",..: 491 861 1048 477 1065 1038 1099 1100 481 562 ...
#$ Population: Factor w/ 5 levels "10.000 - 20.000",..: 5 5 5 5 5 5 5 5 5 5 ...
#$ Status    : Factor w/ 1 level "Provincial capital": 1 1 1 1 NA NA NA NA NA NA ...

levels(city_medium@data$Population)
# [1] "10.000 - 20.000"  "10.000 - 50.000"  "20.000 - 50.000"  "5.000 - 10.000"  
# [5] "50.000 - 100.000"

png("output/city_medium.png", 
    width=7, height=6, units="in", res=300)
plot(city_medium, pch=21, cex=0.3, col="red", main="Medium cities in Europe",
     axes=TRUE)
plot(europe, add=TRUE)
dev.off()



## SMALL CITIES
city_small <-readOGR("Variables_Europe/City_small.shp", "City_small")

## Check the data attributes
names(city_small)
#[1] "Name"       "Name_ASCII" "Population" "Status"

# Number of small cities
levels(unique(city_small$Name)) # 9843 levels

## Check projection
str(city_small)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

str(city_small@data)
# 'data.frame':	9912 obs. of  6 variables:
#$ Name      : Factor w/ 9843 levels "'s-Gravenzande",..: 792 1070 1075 1433 2088 2306 2806 3080 3081 3082 ...
#$ Name_ASCII: Factor w/ 9838 levels "'s-Gravenzande",..: 678 967 973 1345 2024 2239 2750 3004 3005 3006 ...
#$ Population: Factor w/ 4 levels "10.000 - 20.000",..: NA NA NA NA NA NA NA NA NA NA ...
#$ Status    : Factor w/ 0 levels: NA NA NA NA NA NA NA NA NA NA ...

levels(city_small@data$Population)
# [1] "10.000 - 20.000"  "10.000 - 50.000"  "20.000 - 50.000"  "5.000 - 10.000"  
# [5] "50.000 - 100.000"

png("output/city_small.png", width=7, height=6, units="in", res=300)
plot(city_small, pch=21, cex=0.2, col="red", main="Small cities", axes=TRUE)
plot(europe, add=TRUE)
dev.off()



#### LARGE LAKES
large_lakes <-readOGR("Variables_Europe/Lakes_large.shp", "Lakes_large")

## Check the data attributes
names(large_lakes)
#[1] "Name"       "Name_ASCII" "GIS_FLI_Se" "Perimeter"  "Shape_STAr" "Shape_STLe"

# Number of lakes
levels(unique(large_lakes$Name)) # 2070 levels

## Check projection
str(large_lakes)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

str(large_lakes@data)
# 'data.frame':	2217 obs. of  6 variables:
#$ Name      : Factor w/ 2070 levels "�- Kikkejaure",..: 98 1179 1189 1247 1327 1858 2032 2006 867 1047 ...
#$ Name_ASCII: Factor w/ 2070 levels "Aalisjarvi","Abbojavrre",..: 75 1149 1162 1221 1311 1855 2030 2027 843 1021 ...
#$ GIS_FLI_Se: num  1.00e+08 2.84e+07 3.05e+07 3.29e+08 2.21e+07 ...
#$ Perimeter : num  70662 37506 36707 206427 34610 ...
#$ Shape_STAr: num  0.00544 0.00157 0.00166 0.0179 0.00123 ...
#$ Shape_STLe: num  0.502 0.308 0.29 1.476 0.286 ...

png("output/lakes_large.png", width=7, height=6, units="in", res=300)
plot(large_lakes, col="blue", border="blue2", main="Large lakes in Europe", 
     axes=TRUE)
plot(europe, add=TRUE)
dev.off()




#### BIG RIVERS
rivers <-readOGR("Variables_Europe/Rivers.shp", "Rivers")
# Warning message:
#In readOGR("Variables_Europe/Rivers.shp", "Rivers") :
#  Dropping null geometries: 11075
# Use this one. I dont understand the warning quite well but plots ok.

# Check data
class(rivers) # [1] "SpatialLinesDataFrame"
plot(rivers)

## Check the data attributes
names(rivers)
#[1] "Name" 
levels(unique(rivers$Name)) # 4047 levels

## Check projection
str(rivers)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

png("output/rivers.png", 
    width=7, height=6, units="in", res=300)
plot(rivers, col="blue", border="blue2", main="Rivers in Europe", axes=TRUE)
plot(europe, add=TRUE)
dev.off()


### Subset to keep only big rivers
rivers1 <- rivers
# Create a column with the length of the river
rivers1$Sl_length <- SpatialLinesLengths(rivers, longlat=TRUE) # distance in km
str(rivers1@data) # column Sl_length appears now 
range(rivers1@data$Sl_length) # [1] 3.685173e-03 1.857392e+03
# Classification. Big rivers over 100 or 1000 km
# https://books.google.co.uk/books?id=dEeK5LFWLecC&pg=PA16&lpg=PA16&dq=classification+rivers+by+size+km&source=bl&ots=B3GwGeDD_8&sig=-WZH9j5OBJKZzcqoGYDiJPwj5Jo&hl=en&sa=X&ved=0ahUKEwjIsZKElJrPAhViBMAKHY5sC9EQ6AEIHDAA#v=onepage&q=classification%20rivers%20by%20size%20km&f=false

# Subset only rivers over 100 km.
rivers2 <- subset(rivers1, Sl_length >= 100)
png("output/rivers_large_opt1.png", 
    width=7, height=6, units="in", res=300)
plot(europe, main="Large rivers in Europe", axes=TRUE)
plot(rivers2, add=TRUE, col="blue")
dev.off()
# this one look better
#writeOGR(rivers2, dsn=".", layer="output/rivers_large", driver="ESRI Shapefile")

# Subset only rivers over 500 km.
rivers3 <- subset(rivers1, Sl_length >= 500)
png("output/rivers_large_opt2.png", 
    width=7, height=6, units="in", res=300)
plot(europe, main="Large rivers in Europe", axes=TRUE)
plot(rivers3, add=TRUE, col="blue")
dev.off()

# Subset only rivers over 1000 km.
rivers4 <- subset(rivers1, Sl_length >= 1000)
png("output/rivers_large_opt3.png", 
    width=7, height=6, units="in", res=300)
plot(europe, main="Large rivers in Europe", axes=TRUE)
plot(rivers4, add=TRUE, col="blue")
dev.off()



### ELEVATION ###

altitude <-readOGR("Raster/Europe_altitude/Europe_altitude.shp", 
                   "Europe_altitude")

# Explore the dataset
class(altitude) # "SpatialPolygonsDataFrame"

head(altitude@data)
#   HOEHE Shape_Leng Shape_Area
# 0     0        0.4       0.01
# 1     0        0.4       0.01
# 2     0        0.4       0.01
# 3     0        0.4       0.01
# 4     0        0.4       0.01
# 5     0        0.4       0.01

#plot(altitude) # wierd


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
projection(raster.alt) <- proj4string(altitude) # uses the projection of the shapefile
# for the raster    
res(raster.alt)=c(0.001,0.001) #

# Add the attribute
raster.alt2 <- rasterize(altitude, field="HOEHE", raster.alt) # converts to a raster
# assigning the attribute of interest
raster.alt2
#class       : RasterLayer 
#dimensions  : 367, 617, 226439  (nrow, ncol, ncell)
#resolution  : 0.1, 0.1  (x, y)
#extent      : -10.7, 51, 34.5, 71.2  (xmin, xmax, ymin, ymax)
#coord. ref. : +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 
#data source : in memory
#names       : layer 
#values      : -30, 3772  (min, max)

# Save it
#writeRaster(raster.alt2, filename="output/altitude_raster", format="GTiff")

# Plot it
png("output/altitude.png", width=6, height=4, units="in", res=300)
plot(raster.alt2, main="Elevation in Europe") 
dev.off()
