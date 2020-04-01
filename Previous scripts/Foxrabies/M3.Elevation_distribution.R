##'**MRES PROJECT**
##'
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'
##'**MICAELA DE LA PUENTE L.**        
##'          
##'**This script describe the distribution of elevation in WE** 
##############################################################

## Set working directory FOR RABIES CASES
setwd("C:/Users/Mica/Dropbox/Fox Rabies_MD/Scripts")
####################################################

## Call the packages
library(raster)
library(ggplot2)
library(ggmap)
library(mapproj)
library(Rcpp)
library(foreign)
library(maptools)
library(sp)
library(rgdal)
###################################################

## Call the elevation raster.
elev.fli <- raster("output/elevationwe_fli2.tif")
elev.strm <- raster("output/elevationwe_new3.tif")
# everything ok

## Call the countries shp
countries_zero <-readOGR("data/Boundaries data/welevel0.shp","welevel0")



###### STRM RASTER #########

# Plot to watch
plot(elev.fli)
plot(countries_zero, add=TRUE)

## Create a dataframe
#freq(elev.fli) # NA values sum is the same than raster1.
ele.count2.lr <- freq(elev.strm)
ele.count2.df.lr <- as.data.frame(ele.count2.lr)
ele.count2.df.lr <- ele.count2.df.lr[-c(2337), ] # delete NA row

# papr 
#ele.count2.df.lr <- ele.count2.df.lr[-c(2268), ] # delete NA row
# I DONT REMEMBER WHY THIS HAD THIS
ele.count2.df.lr$value <- ele.count2.df.lr$value

##### There are negative values #####


## Disaggregate the altitude values with more than one observation
ele.count2.df.lr.raw <- data.frame(value = rep(ele.count2.df.lr$value, 
                                           times=ele.count2.df.lr$count),
                               count = rep(1, times=sum(ele.count2.df.lr$count)))

## Plot the histogram
# seq2 <- seq(-200,4600, by=100)
# hist(ele.count2.df.lr.raw$value, breaks=seq2, 
#      ylim=c(0, 10000), 
#      xlim=c(0, 4000),
#      col="lightgreen",
#      xlab="Elevation", cex.lab=1.5, cex.axis=1.3) # this one

seq2 <- seq(-100,4000, by=100)
hist(ele.count2.df.lr.raw$value, breaks=seq2,
     ylim=c(0, 10000),
     xlim=c(0, 4000),
     col="lightgreen",
     xlab="Elevation", cex.lab=1.5, cex.axis=1.3) # this one

## Check the summary
summary(ele.count2.df.lr.raw$value)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -6.0    79.0   215.0   375.1   466.0  3843.0 

### I DONT REMEBER WHY WE HAD THESE
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  -181.0    78.0   213.0   374.1   467.0  4558.0 
# The highest point in WE territory is 4558.0. At least 75% of WE area is 
# under 470 m.a.s.l.

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -6.0    80.0   216.0   374.3   470.0  3669.0 

