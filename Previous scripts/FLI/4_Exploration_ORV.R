##'**LAURIE'S PROJECT**
##'**MICAELA DE LA PUENTE L.**
##'**This script explores ORV data**

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
library(car)
######################################################

europe_ORV <-readOGR("ORV/ORV_Europa_WGS84.shp","ORV_Europa_WGS84")
class(europe_ORV) # [1] "SpatialPolygonsDataFrame"
str(europe_ORV@data)
# 'data.frame':	1651 obs. of  16 variables:

## Check the data attributes
names(europe_ORV)
#  [1] "NAME"       "LAND"       "JAHR"       "JA_ZEIT"    "FIKT_DATE"  "VON_DATE"   "BIS_DATE"  
#[8] "AUSLAGEART" "KOED_DICHT" "IMPFSTOFF"  "FLAECHE"    "FLUG_LI_AB" "KOED_ANZ"   "DAV_FLUG"  
#[15] "DAV_HAND"   "BEMERKUNG" 

# "NAME": Name of the vaccination campaign
# "LAND": Country
# "JAHR": Year
# "JA_ZEIT": Time or Season - F(eder)=Spring, H(erbst)=Autumn, S(ommer)=Summer
# "FIKT_DATE": Mid date (fictional date)
# "VON_DATE": Start date
# "BIS_DATE": End date
# "AUSLAGEART": Type of delivery
# "KOED_DICHT": bait density
# "IMPFSTOFF": vaccine type
# "FLAECHE": area covered
# "FLUG_LI_AB": flight line distance
# "KOED_ANZ": ??
# "DAV_FLUG": area covered by flight
# "DAV_HAND": area covered by hand
# "BEMERKUNG": remarks
# "Shape_Leng": length of area covered
# "Shape_Area": Total area or circumference?

# Check years
sort(unique(europe_ORV@data$JAHR), decreasing = FALSE)
#[ [1] 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996
#[20] 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015
#[39] 2016
# 39 years of ORV campaings

# Check countries
levels(unique(europe_ORV@data$LAND)) # 31 levels
# [1] "ALBANIA"              "AUSTRIA"              "BELARUS"              "BELGIUM"             
#[5] "BOSNIA & HERZEGOVINA" "BULGARIA"             "CROATIA"              "CZECH_REPUBLIC"      
#[9] "ESTONIA"              "FINLAND"              "FRANCE"               "GERMANY"             
#[13] "GREECE"               "HUNGARY"              "ITALY"                "KOSOVO"              
#[17] "LATVIA"               "LITHUANIA"            "LUXEMBOURG"           "MACEDONIA"           
#[21] "MONTENEGRO"           "NETHERLANDS"          "POLAND"               "ROMANIA"             
#[25] "RUSSIA"               "SERBIA"               "SLOVAK REPUBLIC"      "SLOVENIA"            
#[29] "SWITZERLAND"          "TURKEY"               "UKRAINE"       
# 31 countries

## Check projection
str(europe_ORV)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## Plot the new spatial points object over a low resolution map to confirm location
newmap <- getMap(resolution = "low")
plot(newmap)
plot(europe_ORV, col="blue", add=TRUE)
# The location looks correct.


###########################################################
## Explore season variable (JA_ZEIT) - SEASON!
class(europe_ORV@data$JA_ZEIT) # [1] "factor"
str(europe_ORV@data$JA_ZEIT)
# Factor w/ 4 levels "F","H","S","W": 2 2 2 2 1 1 1 2 1 1 ...
# F(eder)=Spring (April), H(erbst)=Autumn (September), S(ommer)=Summer(June)
table(europe_ORV@data$JAHR, europe_ORV@data$JA_ZEIT) # Only one campaing in winter


## Create the new variable "VAC" from season and recode its values
europe_ORV@data$VAC <- europe_ORV@data$JA_ZEIT
europe_ORV@data$VAC <- recode(europe_ORV@data$VAC, "'F'=1; 'S'=2; 'H'=3; 'W'=4")

## Transform new variable "VACCINATION" as numeric
europe_ORV@data$VAC <- as.numeric(europe_ORV@data$VAC)
str(europe_ORV@data$VAC) # num [1:1651] 1 3 3 3 3 3 3 3 3 3 ...
length(unique(europe_ORV@data$VAC)) # [1] 4
sort((unique(europe_ORV@data$VAC)), decreasing = FALSE) # [1] 1 2 3 4
# "Spring","Summer","Autumn" "Winter"
### CREATE A LIST OF VACCINATIONS, named VAC_LIST
# Ordered by years and seasons, considering also seasons without campaigns.

# ## Create the new variable
## Recode the variable
europe_ORV@data$VAC_LIST <- (4*(europe_ORV@data$JAHR-1978)) + europe_ORV@data$VAC
str(europe_ORV@data$VAC_LIST) # num [1:1651] 145 147 127 127 35 39 43 47 51 55 ...
length(unique(europe_ORV@data$VAC_LIST)) # [1] 95
sort((unique(europe_ORV@data$VAC_LIST)), decreasing = FALSE)
#  [1]   3   5   7   9  11  13  15  17  19  21  23  25  27  29  31  33  35  37  39  41  43
#[22]  45  46  47  49  50  51  53  54  55  57  58  59  61  62  63  65  66  67  69  70  71
#[43]  73  74  75  77  78  79  81  82  83  85  86  87  89  90  91  93  94  95  97  98  99
#[64] 101 103 105 107 109 110 111 113 115 116 117 119 121 123 125 127 129 131 133 135 137
#[85] 138 139 141 142 143 145 147 149 151 153 155
# There are some numbers that not figure because there are not those quarter in those years.


## Create a DATAFRAME out of the SPATIAL POLYGON DATA FRAME
europe_ORV.df <- as.data.frame(europe_ORV) # This help to observe campaigns and distribution of new variables.

## Save the new spatial polygons data frame and standard dataframe!
#writeOGR(europe_ORV, dsn=".", layer="output/europe_ORV", driver="ESRI Shapefile")
# warnings about shape area being too long. They are save as full numbers withput decimals.
#write.csv(europe_ORV.df, file = "output/europe_ORV.csv")
