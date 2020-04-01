##'**MRES PROJECT**
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'**MICAELA DE LA PUENTE L.**
##'**This script shows subset of rabies data**

######################################################
# FIRST SET WORKING DIRECTORY IN RSTUDIO
# i.e. Session > Set Working Directory > To Source File Location
######################################################
## Call the script where we reproject the data.
rm(list=ls())
source("A1_Reproject_rabies_data.r")

## Subset just fox rabies and WE countries #######
# NOTE FOXRABIES.WE IS THE WORKING DATAFRAME!
str(rabies.data)
foxrabies.we <- subset(rabies.data, SPECIES == "fox" &
                        COUNTRY %in%  c("Austria", "Belgium",
                                        "France", "Germany",
                                        "Luxembourg", "The Netherlands",
                                        "Switzerland") &
                        IMPORTED == "F")

## Explore fox rabies dataset for WE dataset
names(foxrabies.we)
str(foxrabies.we)  # 33054 observations from 13 variables now.

# R remembers previous values but they are not there.
# NEED TO BE CAREFUL WITH THIS. IDEALLY EVERYTHING SHOULD RUN FROM BLANK!
head(foxrabies.we) # The previous row numbers of each observations remains.

## Make disappear other values from SPECIES, COUNTRY and IMPORTED variables
foxrabies.we$SPECIES <- factor(foxrabies.we$SPECIES); foxrabies.we$SPECIES
foxrabies.we$COUNTRY <- factor(foxrabies.we$COUNTRY); foxrabies.we$COUNTRY
foxrabies.we$IMPORTED <- factor(foxrabies.we$IMPORTED); foxrabies.we$IMPORTED
str(foxrabies.we) # factor values used for subset are the only ones now.

######################################################
##### CREATE A SPATIAL POINTS DATAFRAME
## Set longitude and latitude
longitude <- foxrabies.we$coords.x1
max(longitude); min(longitude)
#[1] 4514079
#[1] 3310554

latitude <- foxrabies.we$coords.x2
max(latitude); min(latitude)
#[1] 3078357
#[1] 2005598

## Define the coordinate systems
latlon_CRS <- CRS("+proj=longlat +ellps=WGS84")
latlon_CRS # Output: CRS arguments: +proj=longlat +ellps=WGS84

## Create spatial points
f.d.rabies <- SpatialPoints(cbind(x=foxrabies.we$coords.x1, y=foxrabies.we$coords.x2))

## Create an object with the new projection for the spatial points
f.rabies_CRS <- CRS("+proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

## Convert the spatial points to a new object with the new projection
proj4string(f.d.rabies) <- f.rabies_CRS
f.d.rabies.new <- spTransform(f.d.rabies,latlon_CRS)
f.d.rabies.new

## Plot the new spatial points object over a low resolution map to confirm location
newmap <- getMap(resolution = "low")
plot(newmap)
points(f.d.rabies.new, col="red", cex=0.5) # The location looks correct.

## Create the spdf
foxrabies.spdf <- SpatialPointsDataFrame(f.d.rabies.new, foxrabies.we)
foxrabies.spdf
names(foxrabies.spdf)
# [1] "YEAR"      "QUARTER"   "COUNTRY"   "REGION"    "IMPORTED"  "SPECIES"   "NUMBER"   
# [8] "x"         "y"         "coords.x1" "coords.x2" "x.1"       "y.1"      



### ADD GERMAN CASES FROM 1987 TO 1989 ###
germany_cases <-readOGR("output/germany_cases.shp", "germany_cases")
names(germany_cases)
# [1] "YEAR"    "QUARTER" "COUNTRY" "REGION"  "SPECIES" "NUMBER"
str(germany_cases) # 'data.frame':	5028 obs. of  6 variables:

# Use same CRS in both dataframes
str(foxrabies.spdf) # "+proj=longlat +ellps=WGS84"
g_rabies_CRS <- CRS("+proj=longlat +ellps=WGS84")
proj4string(germany_cases) <- g_rabies_CRS
str(germany_cases) # OK



### Keep the same columns sames in both datasets
foxrabies <- foxrabies.spdf[,c(1:4,6,7)] # eliminate unnecessary columns
names(foxrabies)
# [1] "YEAR"    "QUARTER" "COUNTRY" "REGION"  "SPECIES" "NUMBER" 

## Plot both datasets
plot(foxrabies, axes=TRUE)
plot(germany_cases, add=TRUE, col="red")

## Bind both datasets
full_cases <- rbind(foxrabies, germany_cases)
str(full_cases) # data.frame':	38082 obs. of  6 variables:

# Replace species values for fox only. It is in german for german dataset.
full_cases$SPECIES <- "fox"
str(full_cases@data) # ok now
full_cases$SPECIES <- as.factor(full_cases$SPECIES)

# Change names again
foxrabies <- full_cases
# Convert back to dataframe
foxrabies.we <- as.data.frame(foxrabies)
names(foxrabies.we)
# [1] "YEAR"    "QUARTER" "COUNTRY" "REGION"  "SPECIES" "NUMBER"  "x"       "y"     




############################################################################
#####   CREATE THE QUARTER AND SEMESTER TIME STEPS FOR FOX RABIES CASES   ####

## DEFINE YEAR AS NUMERIC
class(foxrabies.we$YEAR) # [1] "character"
foxrabies.we$YEAR <- as.numeric(foxrabies.we$YEAR)
sort((unique(foxrabies.we$YEAR)), decreasing = FALSE)
#[1] 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003
#[18] 2004 2005 2006

## DEFINE QUARTERS AS NUMERIC
class(foxrabies.we$QUARTER) # [1] [1] "character"
foxrabies.we$QUARTER <- as.numeric(foxrabies.we$QUARTER)
length(unique(foxrabies.we$QUARTER)) # [1] 4
sort((unique(foxrabies.we$QUARTER)), decreasing = FALSE) # [1] 1 2 3 4

##############################################################################
## CREATE THE NEW VARIABLE Qs (quarters by timesteps)
foxrabies.we$QUARTER.LIST <- (4*(foxrabies.we$YEAR-1987)) + foxrabies.we$QUARTER + 36
# Add 36 because of quarters in 1978:1986, for them to match in the plots
str(foxrabies.we$QUARTER.LIST) # num [1:38082] 106 77 77 75 75 75 74 74 74 74 ...
class(foxrabies.we$QUARTER.LIST) # [1] "numeric"
length(unique(foxrabies.we$QUARTER.LIST)) # [1] 77
sort((unique(foxrabies.we$QUARTER.LIST)), decreasing = FALSE)
#  [[1]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58
#[23]  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80
#[45]  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98 100 101 102 103
#[67] 104 105 106 107 108 109 110 111 112 113 115
## Some numbers not figure because there are not those quarters in those years.

## CREATE THE NEW VARIABLE SEMESTER
foxrabies.we$SEMESTER <- floor(foxrabies.we$QUARTER.LIST/2)
length(unique(foxrabies.we$SEMESTER)) # [1] 40
str(foxrabies.we$SEMESTER) #num [1:38082] 53 38 38 37 37 37 37 37 37 37 ...
sort((unique(foxrabies.we$SEMESTER)), decreasing = FALSE)
#  [1] 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46
#  [30] 47 48 49 50 51 52 53 54 55 56 57

######## ORDER THE DATASET ###########
## Replace all numbers in the "NUMBER" columns with "1".
foxrabies.we$NUMBER[foxrabies.we$NUMBER > 1] <- 1
# For all row to be 1 and not affect tapply function after.
#####   NO NEED FOR THIS STEP? IS THIS COLUMN USED?    ####

## Clean the dataset
names(foxrabies.we)
colnames(foxrabies.we)[9] <- "Qs" # quarter list
colnames(foxrabies.we)[10] <- "Ss" # semester list
names(foxrabies.we)
#  [1] "YEAR"    "QUARTER" "COUNTRY" "REGION"  "SPECIES" "NUMBER"  "x"       "y"       "Qs"     
# [10] "Ss" 
class(foxrabies.we) # [1] "data.frame"

######## UPDATE THE NEW SPATIAL POINTS DATAFRAME ##########
foxrabies.spdf <- foxrabies.we;
coordinates(foxrabies.spdf)<-~x+y
class(foxrabies.spdf) # [1] "SpatialPointsDataFrame"
proj4string(foxrabies.spdf) <- rabies_CRS


## Save the new spatial points data frame and standard dataframe & KMLs!
writeOGR(foxrabies.spdf, dsn=".", layer="output/foxrabies_final_full", 
         driver="ESRI Shapefile", overwrite_layer = TRUE)
write.csv(foxrabies.we, file = "output/foxrabies_final_full.csv")
writeOGR(foxrabies.spdf, dsn=paste("output/FoxRabies_full", ".kml", sep=""), 
         layer="ID", driver="KML")



# THIS PIECE OF CODE HAS NOT BEEN RUN YET #

#########  KML WESTERN EUROPE BY YEAR ############
we.i<-sort((unique(foxrabies.spdf$YEAR)), decreasing = FALSE) # COULD HAVE JUST USED YEAR!

# CREATES KML FOR EVERY YEAR?
for(i in seq_along(we.i)) {
  we <- foxrabies.spdf[which(foxrabies.spdf$YEAR==we.i[i]),]
  #writeOGR(we, dsn=paste("output/WesternEurope", we.i[i], ".kml", sep=""), layer="ID", driver="KML")
}


######### KML WESTERN EUROPE BY COUNTRIES ##############
co.i <- unique(foxrabies.spdf$COUNTRY)
levels(co.i) # print(co.i)

# CREATES KML FOR EVERY COUNTRY
for(i in levels(co.i)){
  print(paste("We will travel to", i))
  we.c <- foxrabies.spdf[which(foxrabies.spdf$COUNTRY==i),]
  #writeOGR(we.c, dsn=paste("output/", i, ".kml", sep=""), layer="ID", driver="KML")
}


####### KML FOR ALL COUNTRIES by year ##########
# OUTPUTS KML FOR EACH COUNTRY FOR EACH YEAR
for(i in levels(co.i)){
  we.c <- foxrabies.spdf[which(foxrabies.spdf$COUNTRY==i),]
  we.y <- sort((unique(we.c$YEAR)), decreasing = FALSE)

  for(j in seq_along(we.y)){
    # we <- foxrabies.clean[which(we.c$YEAR==we.y[j]),]
    we <- we.c[which(we.c$YEAR==we.y[j]),]
    print(paste("We will travel to", i, "in", we.y[j]))
   # writeOGR(we, dsn=paste("output/", i, we.y[j], ".kml",sep=""), layer="ID", driver="KML")
  }
}
########## BUT THIS ONLY EXISTS FOR COUNTRIES/YEARS WHEN THERE WERE CASES!