##'**MRES PROJECT**
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'**MICAELA DE LA PUENTE L.**
##'**This script set projection of fox ORV dataset**
#####################################################

## Installing packages
#install.packages("foreign",dependencies=TRUE)  ##To read DBF database file
#install.packages("maptools",dependencies=TRUE) ##To read GIS shape file

## Read the packages
library(foreign)
library(maptools)
library(sp)
library(rgdal)
library(rworldmap)
library(car)
#####################################################

##########  Bringing in the european vaccination data   ###############

######################################################
# FIRST SET WORKING DIRECTORY IN RSTUDIO
# i.e. Session > Set Working Directory > To Source File Location


## Read the dbf file
eu_geo<-read.dbf(file="data/ORV/Europa_complete.dbf", as.is = FALSE)
######################################################

names(eu_geo)
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

unique(eu_geo$LAND)
# [1] ALBANIA              AUSTRIA              BELARUS
# [4] BELGIUM              BOSNIA & HERZEGOVINA BULGARIA
# [7] CROATIA              CZECH_REPUBLIC       ESTONIA
# [10] FINLAND              FRANCE               GERMANY
# [13] GREECE               HUNGARY              ITALY
# [16] KOSOVO               LATVIA               LITHUANIA
# [19] LUXEMBOURG           MACEDONIA            MONTENEGRO
# [22] NETHERLANDS          POLAND               ROMANIA
# [25] RUSSIA               SERBIA               SLOVAK REPUBLIC
# [28] SLOVENIA             SWITZERLAND          TURKEY
# [31] UKRAINE


## Read the shapefile
eu_geo.spdf <-readOGR("Data/ORV/Europa_complete.shp","Europa_complete") # Reads ok

class(eu_geo.spdf)
# [1] "SpatialPolygonsDataFrame"
# attr(,"package")
# [1] "sp"

summary(eu_geo.spdf)
# Is projected: TRUE
# proj4string :
#  [+proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000
#   +y_0=2800000 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0]
# Projection is correct.

## Set projection
latlon_CRS <- CRS("+proj=longlat +ellps=WGS84")
eu_geo.spdf.new <- spTransform(eu_geo.spdf,latlon_CRS)
# This step is not necessary anymore.

orv_CRS <- CRS("+proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

proj4string(eu_geo.spdf.new) <- orv_CRS


## Plot the ORV data
newmap <- getMap(resolution = "low")
plot(newmap)
plot(eu_geo.spdf.new, add=TRUE, border="blue") # It plots ok. - Note, this takes sometime!


#######################################
#####    SUBSET ORV DATA FROM WE   ####

## Subset western europe data
levels(eu_geo.spdf.new$LAND)

ORV_we <- subset(eu_geo.spdf.new, LAND %in% c("BELGIUM", "LUXEMBOURG", "NETHERLANDS",
                                              "AUSTRIA", "FRANCE", "SWITZERLAND", "GERMANY"))
# This is the proper subset method for spdf. The other 
#(  ORV_we <- eu_geo.spdf.new[eu_geo.spdf.new$LAND==c("BELGIUM", "LUXEMBOURG", "NETHERLANDS",
#+ "AUSTRIA", "FRANCE", "SWITZERLAND", "GERMANY"),]  )
# does not subset complete information!!!!!!.

## Take years from 1983 to 2006
sort(unique(ORV_we@data$JAHR), decreasing = FALSE)
#[1] 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995
#[19] 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012
levels(ORV_we$LAND)

ORV_we <- ORV_we[which(ORV_we@data$JAHR < 2007),]
## Disappear previous values
levels(ORV_we$LAND)
ORV_we$LAND <- factor(ORV_we$LAND)
levels(ORV_we$LAND)
str(ORV_we@data) # 'data.frame':	736 obs. of  18 variables:

## Plot the data
plot(ORV_we, col = "steelblue") # it plots

# Plot in rworldmap
plot(newmap)
plot(ORV_we, add=TRUE, border="blue")  # it plots


#######   CREATE THE VACCINATION TIME STEPS FOR CAMPAINGS  #########

###########################################################
###########################################################
## Check lenght of years in ORV data
length(sort(unique(ORV_we@data$JAHR)))

## Explore season variable (JA_ZEIT) - SEASON!
class(ORV_we@data$JA_ZEIT) # [1] "factor"
str(ORV_we@data$JA_ZEIT)
# Factor w/ 4 levels "F","H","S","W": 2 2 2 2 1 1 1 2 1 1 ...
# F(eder)=Spring (April), H(erbst)=Autumn (September), S(ommer)=Summer(June)
table(ORV_we@data$JAHR, ORV_we@data$JA_ZEIT) # No campaings in winter


## Create the new variable "VAC" from season and recode its values
ORV_we@data$VAC <- ORV_we@data$JA_ZEIT
ORV_we@data$VAC <- recode(ORV_we@data$VAC, "'F'=1; 'S'=2; 'H'=3; 'W'=4")


## Transform new variable "VACCINATION" as numeric
ORV_we@data$VAC <- as.numeric(ORV_we@data$VAC)
str(ORV_we@data$VAC) # num [1:736] 3 3 1 1 1 1 2 3 3 3 ...
length(unique(ORV_we@data$VAC)) # [1] 3
sort((unique(ORV_we@data$VAC)), decreasing = FALSE) # [1] 1 2 3
# "Spring","Summer","Autumn". There is no winter

### CREATE A LIST OF VACCINATIONS, named VAC_LIST
# Ordered by years and seasons, considering also seasons without campaigns.

# ## Create the new variable
## Recode the variable
ORV_we@data$VAC_LIST <- (4*(ORV_we@data$JAHR-1978)) + ORV_we@data$VAC
str(ORV_we@data$VAC_LIST) # num [1:736] 35 39 43 47 51 55 59 63 67 71 ...
length(unique(ORV_we@data$VAC_LIST)) # [1] 72
sort((unique(ORV_we@data$VAC_LIST)), decreasing = FALSE)
# [1]   3   5   7   9  11  13  15  17  19  21  23  25  27  29  31  33  35  37  39  41  43  45
#[23]  46  47  49  50  51  53  54  55  57  58  59  61  62  63  65  66  67  69  70  71  73  74
#[45]  75  77  78  79  81  82  83  85  86  87  89  90  91  93  94  95  97  98  99 101 103 105
#[67] 107 109 110 111 113 115
# There are some numbers that not figure because there are not those quarter in those years.


## CREATE THE NEW VARIABLE SEMESTER
ORV_we@data$SEMESTER <- floor(ORV_we@data$VAC_LIST/2)
length(unique(ORV_we@data$SEMESTER)) # [1] 57
str(ORV_we@data$SEMESTER) # num [1:736] 17 19 21 23 25 27 29 31 33 35 ...
sort((unique(ORV_we@data$SEMESTER)), decreasing = FALSE)
# [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
# [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50
# [51] 51 52 53 54 55 56 57

## Create a DATAFRAME out of the SPATIAL POLYGON DATA FRAME
ORV_we.df <- as.data.frame(ORV_we@data) # This help to observe campaigns and distribution of new variables.

## Save the new spatial polygons data frame and standard dataframe!
writeOGR(ORV_we, dsn=".", layer="output/ORV_we_final1", driver="ESRI Shapefile")
# warnings about shape area being too long. They are save as full numbers withput decimals.
write.csv(ORV_we.df, file = "output/ORV_we.df_final1.csv")


##############################################
#####    CREATE KML FILES OF ORV DATASET  ####


## Create the KML file of the full ORV dataset
#ORV <- writeOGR(eu_geo.spdf.new, dsn=paste("output/ORV", ".kml", sep=""), layer="ID", driver="KML")
# Polygones read ok in google earth


## Create the KML file of just WE ORV dataset
#ORV1 <- writeOGR(ORV_we, dsn=paste("output/ORV.we", ".kml", sep=""), layer="ID", driver="KML")
#Points are in the correct region.


## Create KML files for WE by years
we.i <-sort((unique(ORV_we$JAHR)), decreasing = FALSE)

# Set the for loop function
for(i in seq_along(we.i)) {
  we <- ORV_we[which(ORV_we$JAHR==we.i[i]),]
   writeOGR(we, dsn=paste("output/ORV.we", we.i[i], ".kml", sep=""),
             layer="ID", driver="KML")
}


###############################################################################
# If we need to reduce one number
# foxrabies$QUARTER.LIST2 <- (foxrabies$QUARTER.LIST-1)
# str(foxrabies$QUARTER.LIST2)
# print(sort(unique(foxrabies$QUARTER.LIST2)))
