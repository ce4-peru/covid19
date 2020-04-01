##'**LAURIE'S PROJECT**
##'**FOX RABIES DYNAMIC**
##'**MICAELA DE LA PUENTE L.**
##'**This script explore the rabies dataset and subset fox rabies data **

######################################################
# FIRST SET WORKING DIRECTORY IN RSTUDIO
# i.e. Session > Set Working Directory > To Source File Location
rm(list=ls())

######################################################
library(foreign)
library(maptools)
library(maps)
library(mapdata)
library(mapproj)
library(sp)
library(rgdal)
library(rworldmap)
require(rgeos)
library(ggmap)
library(ggplot2)
library(Rcpp)
library(foreign)
library(devtools)
library(animation)
library(reshape2)


#####################################################
#####################################################
## Explore it in R
setwd("C:/Users/Micaela/Dropbox/Fox Rabies_MD/FLI/Data")
rabies.data.single <-readOGR("Cases/Rabies_WGS84_Singlepart.shp", 
                             "Rabies_WGS84_Singlepart") 
rabies.data.single.df <- as.data.frame(rabies.data.single)
str(rabies.data.single.df)
# 'data.frame':	254567 obs. of  8 variables:
# coords.x1 and coords.x2 and individual coords by rabies case.

## Check the data attributes
names(rabies.data.single)
# [1] "YEAR"     "QUARTER"  "COUNTRY"  "REGION"   "SPECIES"  
# "IMPORTED"
sort(unique(rabies.data.single@data$YEAR), decreasing = FALSE)
#[1] 1979 1982 1983 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 
#[16] 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 
#[31] 2012 2013 2014 2015 2016 # 35 years
levels(unique(rabies.data.single$COUNTRY)) # 39 levels

## Check projection
str(rabies.data.single)
#..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#..@ projargs: chr "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## Plot the new spatial points object over a low resolution map to confirm location
newmap <- getMap(resolution = "low")
plot(newmap)
points(rabies.data.single, col="red", cex=0.5)
# The location looks correct.


## Subset just fox rabies and non imported cases ##
# NOTE FOXRABIES.WE IS THE WORKING DATAFRAME!
levels(rabies.data.single@data$SPECIES)
# [1] "badger"           "bat"              "cat"              "cattle"          
#[5] "dog"              "equine"           "fallow deer"      "fox"             
#[9] "goat, sheep"      "human cases"      "marten"           "other carnivores"
#[13] "other domestic"   "other mustelides" "other wildLife"   "pig"             
#[17] "racoon"           "racoon dog"       "red deer"         "roe deer"        
#[21] "stray dDog"       "wildboar"         "wolf"        

unique(rabies.data.single@data$IMPORTED)
# [1] 0 1

# Subset
foxrabies <- subset(rabies.data.single, 
                    SPECIES == "fox" & IMPORTED == 0)

# R remembers previous values but they are not there.
## Make disappear other values from SPECIES
foxrabies$SPECIES <- factor(foxrabies$SPECIES);foxrabies$SPECIES # only one level now.
foxrabies$COUNTRY <- factor(foxrabies$COUNTRY);foxrabies$COUNTRY # 34 Levels
unique(foxrabies$COUNTRY)
#[1] Croatia              Hungary              Slovak Republic      Turkey              
#[5] Lithuania            Romania              Poland               Bulgaria            
#[9] Austria              Ukraine              Belarus              Russian Federation  
#[13] Slovenia             Bosnia - Hercegovina CS                   Albania             
#[17] Moldova              Belgium              Luxembourg           Switzerland         
#[21] Italy                Estonia              Latvia               Germany             
#[25] Czech Republic       France               Serbia               Montenegro          
#[29] Finland              The Netherlands      Macedonia            Greece              
#[33] Spain                Georgia      


## Explore fox rabies dataset for WE dataset
str(foxrabies)  # 142612 obs. of  6 variables now.
sort(unique(foxrabies@data$YEAR), decreasing = FALSE)
#[1] 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 
# 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016
## Only from 1987 ###
## There are 30 years of fox rabies data

# Transform the subset the a normal dataframe
foxrabies.df <- as.data.frame(foxrabies)


## Add Germany data from before
names(foxrabies)
#[1] "YEAR"     "QUARTER"  "COUNTRY"  "REGION"   "SPECIES"  "IMPORTED"

germany <-readOGR("output/Germany_cases.shp", 
                             "Germany_cases") 
names(germany)
#[1] "YEAR"    "QUARTER" "COUNTRY" "REGION"  "SPECIES" "NUMBER" 

# Make column names to match
foxrabies@data <- foxrabies@data[, c(1:5)]
germany@data <- germany@data[, c(1:5)]

# Use same CRS in both dataframes
str(foxrabies) # "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
rabies_CRS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(germany) <- rabies_CRS
str(germany) # OK

# Bind them
full_cases <- rbind(foxrabies, germany)
str(full_cases@data) # 147640 obs. of  5 variables:
#$ SPECIES: Factor w/ 2 levels "fox","Fuchs": 1 1 1 1 1 1 1 1 1 1 ...

# Replace species values for fox only. It is in german for german dataset.
full_cases$SPECIES <- "fox"
str(full_cases@data) # ok now
plot(full_cases)
str(full_cases)


####### ADD Qs and Ss ############
## CREATE THE NEW VARIABLE QUARTER.LIST (quarters by timesteps)
full_cases@data$QUARTER <- as.numeric(full_cases@data$QUARTER)
full_cases@data$YEAR <- as.numeric(full_cases@data$YEAR)

full_cases$Qs <-(4*(full_cases$YEAR-1987)) + full_cases$QUARTER + 36 #ORV starts on 1978
str(full_cases$Qs) # num [1:154011] 106 106 106 106 106 106 106 106 106 106 ...
class(full_cases$Qs) # [1] "numeric"
length(unique(full_cases$Qs)) # [1] [1] 118
sort((unique(full_cases$Qs)), decreasing = FALSE)
#   [1]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58
#[23]  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80
#[45]  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101 102
#[67] 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124
#[89] 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146
#[111] 147 148 149 150 151 152 153 154
## Some numbers not figure because there are not those quarters in those years.
#Turquey 2009 in Quarter 0 appears as 124 Qs

## CREATE THE NEW VARIABLE SEMESTER
full_cases$Ss <- floor(full_cases$Qs/2)
length(unique(full_cases$Ss)) # [1] 60
str(full_cases$Ss) #  num [1:154011] 53 53 53 53 53 53 53 53 53 53 ...
sort((unique(full_cases$Ss)), decreasing = FALSE)
# [1] 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
# [31] 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77

# Last look
str(full_cases) # OK

# Get a normal dataframe
full_cases.df <- as.data.frame(full_cases)

## Save the new spatial polygons data frame and standard dataframe!
#writeOGR(full_cases, dsn=".", layer="output/europe_cases", driver="ESRI Shapefile")
# warnings about shape area being too long. They are save as full numbers withput decimals.
#write.csv(full_cases.df, file = "output/europe_cases.csv")

########################################################################
#########      MAP IN GOOGLE MAPS WITH LONG-LAT DATA        ############

#### Map with full rabies cases

## Map of Europe focus on our study area
map <- get_map(location = c(lon = 25, lat = 55),     # select Europe
               zoom = 4,                # 5 to set a higher scale.
               maptype = "terrain",     # setting easy to visualise
               source = "google")       # the source to load the map from
ggmap(map) # ggmap function to load map of Europe - plots map as a layer over a base graphic

#pdf("output/Rabies_cases.pdf", onefile = TRUE)
## Map of our study are with rabies cases
ggmap(map) + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = full_cases.df, # Add points for sites using latitude and longitude.
             aes(x = coords.x1, y = coords.x2), # Set longitudes and latitudes on axes.
             alpha = 0.5, # Define the type of point to be used on the map.
             color = "red", # Set colour of the points
             size = 0.2,
             position = position_jitter(width = 0.1, height = 0.1))
#dev.off()


#### Maps with fox rabies cases by years in WE
## Select years
year.i<-sort((unique(full_cases.df$YEAR)), decreasing = FALSE)

## by year
for(i in seq_along(year.i)){
  
  eu.ggplot <- full_cases.df[which(full_cases.df$YEAR==year.i[i]),] 
  # Create the map with fox rabies data
  
  maps <- ggmap(map) + xlab("Longitude") + ylab("Latitude") +
    geom_point(data = eu.ggplot,
               aes(x = coords.x1, y = coords.x2),
               alpha = 0.5, color = "red", size = 0.5,
               position = position_jitter(width = 0.1, height = 0.1)) + 
    ggtitle(paste("Fox rabies in", year.i[i],sep=" ")) # add a title
  # Plot the map
  plot(maps)
  
  # Save plots in png files
 # ggsave(paste("output/Europe", year.i[i],  ".png",sep=""), 
#         dpi=300, width=6, height=5) # too heavy
}



########################################################################
#### Number of cases per year in WE ####

table(full_cases.df$YEAR) # every year had cases

## Sum the number of cases in each year
rabies.y <- hist(full_cases.df$YEAR, breaks=1986:2016, plot=FALSE)$counts

## Plot the graph
#png("output/rabies_years_scp.png", width=10, height=4, units="in", res=300)
plot.y <- plot(rabies.y, type="o", col="red", axes= FALSE,
               lwd=2, pch=19, ann=FALSE)
axis(1, at=1:30, lab=c(1987:2016)) ## Set axes and title
axis(2, at = seq(0, 15000, by = 3000), las=2)
box()
title(main="Rabies incidence in Europe", xlab="Years", 
      ylab="Number of infected foxes")
#dev.off()



#### Lineplots of number of cases by years per each country

table(full_cases.df$COUNTRY, full_cases.df$YEAR)

## Create the vector
country.i <- as.factor(full_cases.df$COUNTRY)
country.i

#pdf("output/rabies_countriesbyyears_scp.pdf", onefile = TRUE)
## Set the for loop
for(i in levels(country.i))
{ # Subset the data
  country <- full_cases.df[which(full_cases.df$COUNTRY==i),]
  
  # Sum the number of cases per year
  rabies.iy <- hist(country$YEAR, breaks=1986:2016, plot=FALSE)$counts
  
  # Plot the graphs
  plot.country <-plot(rabies.iy, col = "red", pch=19, type="o", 
                      ann=FALSE, axes=FALSE)
  axis(1, lab=c(1987:2016), at=1:30)
  axis(2)
  title(main=(paste(i, "by years")), xlab="Years", ylab="Number infected foxes")
  box()
}
#dev.off()



#### Barplot of number of cases by years per each country

#pdf("output/rabies_countriesbyyears.bp.pdf", width=12, height=4,
#  onefile = TRUE)
# Set the for loop function
for(i in levels(country.i))
{ country <- full_cases.df[which(full_cases.df$COUNTRY==i),]

# Sum the number of cases per year
rabies.iy <- hist(country$YEAR, breaks=1986:2016, plot=FALSE)$counts

# Plot the barplot
plot.country <-barplot(rabies.iy,
                       ylab ="New infected foxes Western Europe",
                       col = "grey",
                       main=(paste(i, "by years")))
text(plot.country, labels=c(1987:2016), par("usr")[3],
     adj = c(0.5,1.2), xpd = TRUE, cex=.5)
}
#dev.off()



#### Number of cases per quarter in WE - BARPLOT ####
table(full_cases.df$QUARTER)

## Sum the number of cases per quarter
rabies.q <- hist(full_cases.df$QUARTER, breaks=0:4, plot=FALSE)$counts

## Plot the graph
#png("output/rabies_quarters_bp.png", width=6, height=4, units="in", res=300)
bp <- barplot(rabies.q, axes=FALSE, axisnames = FALSE,
              main="Western Europe", ylim = c(0, 50000))
axis(2, ylim=c(0,20000), cex=1.2, las=2)
text(bp, par("usr")[3], labels = c("Quarter 1\n(Jan-Mar)", "Quarter 2\n(Apr-June)", "Quarter 3\n(Jul-Sep)", "Quarter 4\n(Oct-Dec)"), #srt = 45,
     adj = c(0.5,1.2), xpd = TRUE, cex=1.2)
mtext("Number of cases",side=2,line=4, cex=1.3)
# More cases in the first quarter
#dev.off()
