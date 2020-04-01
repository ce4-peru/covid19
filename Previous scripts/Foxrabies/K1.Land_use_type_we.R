##'**MRES PROJECT**
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'**MICAELA DE LA PUENTE L.**        
##'**This script describe land use in we** 
##########################################

## Call the packages
library(foreign)
library(maptools)
library(sp)
library(rgdal)
library(rworldmap)
library(car)
library(adehabitatHR)
library(adehabitatLT)
library(adehabitatMA)
library(raster)
############################################################################

## Set working directory FOR RABIES CASES
#rm(list=ls())

## Call the fox rabies dataset in WE.
foxrabies.we <-read.csv("output/foxrabies_final_full.csv")
foxrabies <-readOGR("output/foxrabies_final_full.shp","foxrabies_final_full") 


## Call the land use raster
fli.raster <- raster("output/new_raster2_fli_v3.tif")
nasa.raster <- raster("output/new_raster2_nasa2000.tif")

## Call the countries shp
countries_zero <-readOGR("Data/Boundaries data/welevel0.shp","welevel0")


########       MIX WE SHP WITH LAND RASTER    ##################

##### NASA2000 ####
## Create legend
lab.legend.nasa <- c("Dense \nsettlements", "Villages", "Croplands", "Rangelands", 
                "Forested lands", "Treeless and \nbarren lands")
col.legend.nasa <- c("mediumorchid4", "cyan2", "yellow2", "chocolate1",
                "forestgreen", "bisque3")

## Create breakpoints
breakpoints.nasa <- c(0, 1, 2, 3, 4, 5, 6) # create breaks to assign colors
colors.nasa <- c("mediumorchid4", "cyan2","yellow2",
            "chocolate1", "forestgreen", "bisque3")


## Plot the land use raster and the countries boundaries
plot(nasa.raster, breaks=breakpoints.nasa, col=colors.nasa, legend=FALSE)
## Add a legend
legend <- legend("right", legend=lab.legend.nasa, 
                 fill=col.legend.nasa, 
                 x.intersp = .3, y.intersp = .6, cex = 0.75, 
                 xpd = TRUE, inset = c(-0.35,0), bty = "n")
plot(countries_zero, add=TRUE)
## A LITTLE MORE TO THE RIGHT

## Save frequencies of values of the shapefile in a dataframe
t.count <- freq(nasa.raster)
t.count1 <- as.data.frame(t.count)

### Factorize the values
t.count1$value <- factor(t.count1$value)
str(t.count1$value)
# Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 5 6 NA

## Recode the values
t.count1$value <- recode(t.count1$value, "'1'='Dense settlements'; '2'='Villages'; '3'='Croplands'; '4'='Rangelands';'5'='Forested lands';'6'='Treeless and barren lands';'NA'='NA'")
str(t.count1$value)
# Factor w/ 6 levels "Croplands","Dense settlements",..

## Sum rows with the same levels of land  use
sum.t.count1 <- tapply(t.count1$count, list(t.count1$value), sum, na.rm=TRUE)
sum.rab.t.count1 <- data.frame(sum.t.count1) # transform to a dataframe

## CREATE A PIE GRAPH
slices <- sum.rab.t.count1$sum.t.count1 # set pie structure
lbls <- c("Croplands", "Dense \nsettlements", 
          "Forestlands", "Rangelands",
          "Treeless and \nbarren lands", "Villages") # Labels of land use types
pct <- round(slices/sum(slices)*100, digits = 1) # Calcaulate percentages
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # add % to labels 

## Plot the pie
# pdf("output/pie3.landuse_2000.pdf", onefile = TRUE, width = 13, height = 9)
par(mfrow=c(1,1))
oldpar <- par()
par(cex=1.5)
pie(slices,labels = lbls, col=c("yellow2", "mediumorchid4", "forestgreen",
                                "chocolate1", "bisque3", "cyan2"),
                                main="Land use types in Western Europe")
# dev.off()


##### FLI ####
## Create legend
lab.legend.fli <- c("Artificial lands", 
                    "Agricultural", "Highly forested", 
                    "Moderately forested ", "Barren lands",
                    "Marshlands")
col.legend.fli <- c("mediumorchid4", "yellow2", 
                    "forestgreen", "limegreen", "bisque3", "coral1")

## Create breakpoints
breakpoints.fli <- c(0.5, 1.5, 3.5, 4.5, 5.5, 6.5, 7.5) # create breaks to assign colors
colors.fli <- c("mediumorchid4", "yellow2",
                "forestgreen", "limegreen", "bisque3", "coral1")


## Plot the land use raster and the countries boundaries
plot(fli.raster, breaks=breakpoints.fli, col=colors.fli, legend=FALSE)
## Add a legend
legend <- legend("right", legend=lab.legend.fli, 
                 fill=col.legend.fli, 
                 x.intersp = .3, y.intersp = .6, cex = 0.75, 
                 xpd = TRUE, inset = c(-0.35,0), bty = "n")
plot(countries_zero, add=TRUE)

## Save frequencies of values of the shapefile in a dataframe
t.count <- freq(fli.raster)
t.count1 <- as.data.frame(t.count)

### Factorize the values
t.count1$value <- factor(t.count1$value)
str(t.count1$value)
# Factor w/  6 levels "1","3","4","5",..: 1 2 3 4 5 6 NA

## Recode the values
t.count1$value <- recode(t.count1$value, "'1'='Artificial lands'; 
                         '3'='Agricultural'; '4'='Highly forested';'5'='Moderately forested';
                         '6'='Treeless and barren lands';'7'='Wetlands';'NA'='NA'")
str(t.count1$value)
# Factor w/ 6 levels "Agricultural",..: 2 1 3 4 5 6 NA

## Sum rows with the same levels of land  use
sum.t.count1 <- tapply(t.count1$count, list(t.count1$value), sum, na.rm=TRUE)
sum.rab.t.count1 <- data.frame(sum.t.count1) # transform to a dataframe

## CREATE A PIE GRAPH
slices <- sum.rab.t.count1$sum.t.count1 # set pie structure
lbls <- c("Agricultural", "Artificial lands", 
          "Highly forested", "Moderately forested",
          "Treeless lands", "Wetlands") # Labels of land use types
pct <- round(slices/sum(slices)*100, digits = 1) # Calcaulate percentages
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # add % to labels 

## Plot the pie
# pdf("output/pie3.landuse_fli_v3.pdf", onefile = TRUE, width = 13, height = 9)
par(mfrow=c(1,1))
oldpar <- par()
par(cex=1)
pie(slices,labels = lbls, col=c("yellow2", "mediumorchid4", 
                                "forestgreen", "limegreen", "bisque3", "coral1"), 
    main="Land use types in Western Europe")
# dev.off()

