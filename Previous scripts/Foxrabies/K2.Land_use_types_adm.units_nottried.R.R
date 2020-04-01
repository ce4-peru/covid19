##'**MRES PROJECT**
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'**MICAELA DE LA PUENTE L.**        
##'**This script describe land use in the gadms** 
#################################################

## Set working directory FOR RABIES CASES
rm(list=ls())

#############################################################################
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
############################################################################
# NOT PROVED CODE!!!

## Call the fox rabies dataset in WE.
foxrabies.we <-read.csv("output/foxrabies_final_full.csv")
foxrabies <-readOGR("output/foxrabies_final_full.shp","foxrabies_final_full") 

## Call the land use raster
land.raster <- raster("C:/Users/Mica/Dropbox/Fox Rabies_MD/Scripts/output/landuse_raster")
# THIS DOES NOT EXIST 

## Call the countries shp
setwd("D:/MRes/MRes Project/DATA/Boundaries data")
countries_zero <-readOGR("Data/Boundaries data/welevel0.shp","welevel0")
######################################################################

##### PLOT ADM. UNITS WITH LAND RASTERS AT THE BACK #####

## Create legend
lab.legend <- c("Dense \nsettlements", "Villages", "Croplands", "Rangelands", 
                "Forested lands", "Treeless and \nbarren lands")

col.legend <- c("mediumorchid4", "cyan2", "yellow2", "chocolate1",
                "forestgreen", "bisque3")

## Create breakpoints
breakpoints <- c(0, 1, 2, 3, 4, 5, 6) # create breaks to assign colors
colors <- c("mediumorchid4", "cyan2","yellow2",
            "chocolate1", "forestgreen", "bisque3")

#pdf("adm_units_landuse.pdf", onefile = TRUE, width = 13, height = 7)
plot(land.raster, breaks=breakpoints, col=colors, legend=FALSE)
## Add a legend
legend <- legend("right", legend=lab.legend, 
                 fill=col.legend, 
                 x.intersp = .3, y.intersp = .6, cex = 0.75, 
                 xpd = TRUE, inset = c(-0.35,0), bty = "n")
plot(countries_zero, add=TRUE)
#dev.off()
# Comment about territories: FRANCE IS MOSTLY CROPLANDS and rangelands at south. Germany has more croplands and Villages; Belgium, Villages and Rangelands; Neatherlands, villages and croplands; Austria, forest lands and villages; and Switzerland, Villages.

# Comment about borders: In Germany border are mostly villages and less rangelands.In France border are mostly croplands. In Belgium, rangelands and villages.


#####  MIX COUNTRIES SHP WITH LAND RASTER   #####

## Create a vector with countries
country.i <- unique(countries_zero$NAME_0)

## Create an empty dataframe for results
result.c <- data.frame(sum=character(), country=character(),
                       stringsAsFactors=FALSE)

#### Rasterize with Land.raster1
for (i in levels(country.i))
     {
  print(i)
  ## Subset to get only the i country shape
  subset.c <- countries_zero[which(countries_zero$NAME_0==i),] 
  hr_raster1 <- rasterize(subset.c, land.raster, mask=TRUE) 
  
  ## Plot the result
  plot(hr_raster1, breaks=breakpoints, col=colors, legend=FALSE)
  legend <- legend("right", legend=lab.legend, 
                 fill=col.legend, 
                 x.intersp = .2, y.intersp = .2, cex = 0.8, 
                 xpd = TRUE, inset = c(-0.35,0), bty = "n")
  
  ## Get the frequencies of values
  values <- freq(hr_raster1)
  values1 <- as.data.frame(values) # save values in a dataframe
  
  str(values1$value)
  values1$value <- factor(values1$value)
  str(values1$value)
  # Factor w/ 6 levels "1","2","3",..
  
  ## Recode the values
  values1$value <- recode(values1$value, "'1'='Dense settlements'; '2'='Villages'; '3'='Croplands'; '4'='Rangelands';'5'='Forested lands';'6'='Treeless and barren lands';'NA'='NA'")
  str(values1$value)
  # Factor w/ 6 levels "Croplands","Dense settlements",..
  
  ## Sum rows with same levels of land  use
  sum.values1 <- tapply(values1$count, list(values1$value), sum, na.rm=TRUE)
  values.df.i <- data.frame(sum.values1)
  values.df.i$country <- i
  result.c <- rbind(result.c, values.df.i) # bind all results together
}


#### Barplot of land use per country ####
## Set new vector of countries
country.i <- sort(unique(result.c$country), decreasing = FALSE)
print(country.i)

#pdf("countries_landuse.pdf", onefile = TRUE, width = 14, height = 7)
par(mfrow=c(1,1)) 
for (i in country.i) 
  {
  land.i <- result.c[result.c$country==i,]
  print(i)
  barplot <- barplot(land.i$sum.values1, main=i)
  }
#dev.off()


#### Pie of land use per country ####
## Set new vector of countries

#pdf("pie3.landuse_country.pdf", onefile = TRUE, width = 13, height = 9)
par(mfrow=c(1,1)) 
for (i in country.i) 
{ 
  land.i <- result.c[result.c$country==i,]
  slices <- land.i$sum.values1 # set pie structure
  lbls <- c("Croplands", "Dense \nsettlements", 
            "Forestlands", "Rangelands",
            "Treeless and \nbarren lands", "Villages") # Labels of land use types
  pct <- round(slices/sum(slices)*100) # Calcaulate percentages
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  
  ## Plot the pie
  par(mfrow=c(1,1))
  oldpar <- par()
  par(cex=1.5)
  pie(slices,labels = lbls, col=c("yellow3", "mediumorchid4", "chartreuse4",
                                  "orange2", "bisque4", "turquoise2"),
      main=paste("Land use types in", i))
}
#dev.off()
### BELGIUM, GERMANY, NETHERLANDS AND LUXEMBOURG ONLY HAVE 5 LEVELS, SO BARREN LANDS LOOKS INFLATED.