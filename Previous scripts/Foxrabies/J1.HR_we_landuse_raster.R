##'**MRES PROJECT**
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'**MICAELA DE LA PUENTE L.**
##'**This script shows find the HR in WE using the land use raster of we**
#############################################################################

## Call the packages
library(foreign)
library(maptools)
library(sp)
library(rgdal)
library(rworldmap)
library(car)
library(raster)
library(adehabitatHR)
library(adehabitatLT)
library(adehabitatMA)
library(raster)
############################################################################

## Set working directory FOR RABIES CASES
#rm(list=ls())

## Call the fox rabies dataset in WE & shp file
foxrabies.we <-read.csv("output/foxrabies_final_full.csv")
foxrabies <-readOGR("output/foxrabies_final_full.shp","foxrabies_final_full")

## Call the land use raster
land.raster <- raster("output/new_raster1_nasa.tif")
fli.raster <- raster("output/new_raster2_fli_v3.tif")

## Call the countries shp
countries_zero <-readOGR("Data/Boundaries data/welevel0.shp", "welevel0")


########       MIX COUNTRIES WITH LAND RASTER 1    ############
plot(land.raster)
plot(countries_zero, add=TRUE)

plot(fli.raster)
plot(countries_zero, add=TRUE)


#### Rasterize the land.raster1 with WE countries shp
hr_raster1 <- rasterize(countries_zero, land.raster, field=0) # onyl 0 or NA
hr_raster1
# dimensions  : 207, 335, 69345  (nrow, ncol, ncell)
# values      : 0, 0  (min, max)

plot(hr_raster1, legend=FALSE) # yellow
length(hr_raster1)
#[1] 69345 # number of cells

freq(hr_raster1)
#value count
#[1,]     0 30274 # TOTAL EXTENSION oF THE MAP
#[2,]    NA 39071 # TOTAL EXTENSION OF THE RASTER


#### MASK THEM to make the shape more perfect ####
hr_raster2 <- mask(hr_raster1, land.raster)
hr_raster2

plot(hr_raster2, legend=FALSE)
# Very little terrtory dissapears near the top.

freq.hr2 <- freq(hr_raster2)
print(freq.hr2)
# [1,]     0 30063 # TOTAL EXTENSION oF THE MAP
# [2,]    NA 39282 # TOTAL EXTENSION OF THE RASTER

## Save area of WE
area.we <- freq.hr2[1,2]


####  Rasterize now with the rabies cases points
hr_raster3 <- rasterize(foxrabies, hr_raster2, field=1,
                        update=TRUE, updateValue=0)
hr_raster3
plot(hr_raster3)
plot(hr_raster3, col=c("yellow", "red"), colNA="blue", legend=FALSE)
# Country in blue and rabies cases in red

## Save frecuency of infected
freq.rabies <- freq(hr_raster3)
#value count
# [1,]     0 18846
# [2,]     1 11217
# [3,]    NA 39282
infected <- freq.rabies[2,2] # 11217

## Obtain and save percentage of area occupied by fox rabies
per.infected <- infected/area.we*100
per.infected # 37.31165 %
# HR of rabies is WE is 37.3%



#### Rasterize the fli.raster with WE countries shp
hr_raster1 <- rasterize(countries_zero, fli.raster, field=0) # onyl 0 or NA
hr_raster1
# dimensions  : 207, 335, 69345  (nrow, ncol, ncell)
# values      : 0, 0  (min, max)

plot(hr_raster1, legend=FALSE) # yellow
length(hr_raster1)
#[1] 69345 # number of cells

freq(hr_raster1)
#value count
#[1,]     0 30274 # TOTAL EXTENSION oF THE MAP
#[2,]    NA 39071 # TOTAL EXTENSION OF THE RASTER


#### MASK THEM to make the shape more perfect ####
hr_raster2 <- mask(hr_raster1, fli.raster)
hr_raster2

plot(hr_raster2, legend=FALSE)
# Very little terrtory dissapears near the top.

freq.hr2 <- freq(hr_raster2)
print(freq.hr2)
# [1,]     0 29887 # TOTAL EXTENSION oF THE MAP
# [2,]    NA 39458 # TOTAL EXTENSION OF THE RASTER

## Save area of WE
area.we <- freq.hr2[1,2]


####  Rasterize now with the rabies cases points
hr_raster3 <- rasterize(foxrabies, hr_raster2, field=1,
                        update=TRUE, updateValue=0)
hr_raster3
plot(hr_raster3)
plot(hr_raster3, col=c("yellow", "red"), colNA="blue", legend=FALSE)
# Country in blue and rabies cases in red

## Save frecuency of infected
freq.rabies <- freq(hr_raster3)
#value count
# [1,]     0 18846
# [2,]     1 11217
# [3,]    NA 39282
infected <- freq.rabies[2,2] # 11217

## Obtain and save percentage of area occupied by fox rabies
per.infected <- infected/area.we*100
per.infected # 37.15662  %
# HR of rabies is WE is 37.2%


#####################################
####  HR OF FOX RABIES BY YEAR  #####

## Create the variable for the loop
year.i <- sort(unique(foxrabies$YEAR), decreasing = FALSE)
year.i

## Create a data frame to save results
result <- data.frame(YEAR=character(), FREE=character(),
                     stringsAsFactors=FALSE)

## Create the loop
# Save maps of grids with rabies in WE maps per years
#pdf("HR.we.raster.cases.pdf", onefile = TRUE)
par(mfrow=c(1,1))

# Set the loop
for (i in seq_along(year.i))
  {
  data.rab.i <- foxrabies[foxrabies@data$YEAR==year.i[i],]

  # Rasterize rabies cases points
  hr_raster3.i <- rasterize(data.rab.i, hr_raster2, field=1,
                          update=TRUE, updateValue=0)
  #hr_raster3.i <- rasterize(data.rab.i, hr_raster2, mask=TRUE)

  # Calculate percentage of infected area
  freq.rabies.i <- freq(hr_raster3.i)
  infected.i <- freq.rabies.i[2,2]          # save number of infected grids
  per.infected.i <- infected.i/area.we*100  # save percentage of infected grids
  print(per.infected.i)
  per.infected.i.df <- as.data.frame(per.infected.i) # convert to dataframe
  result <- rbind(result, per.infected.i.df) # bind it with previous loop data

  # Plot the infected area
  plot(countries_zero)                    # plot countries boundaries
  plot(hr_raster3.i, col=c("NA", "red2"), # plot infected grids
       legend=FALSE, add=TRUE)
  title(paste("WE", year.i[i]))
  }
#dev.off()



####    SUMMARY PLOTS    ####

## PLOT 1 -  LINEPLOT with percentage of area
# SHOWS HOW TOTAL of INFECTED PERCENTAGE CHANGES WITH THE YEARS
plot(result$per.infected.i, type="o", ann=FALSE, axes=FALSE,
     col="red", lwd=2, pch=19)
box()
axis(1, at=1:length(year.i), lab=year.i, cex=1.5)
axis(2, cex=1.5)
title(main="Percentage of area with confirmed fox rabies cases",
      xlab="Years", ylab="Percentage (%)")


## PLOT 2 -  LINEPLOT with area in km2
# SHOWS HOW TOTAL AREA INFECTED CHANGES WITH THE YEARS
result$infected.i <- (result$per.infected.i)/100*area.we
result$infected.i <- (result$infected.i)*85

plot(result$infected.i, type="o", ann=FALSE, axes=FALSE,
     col="red", lwd=2, pch=19)
box()
axis(1, at=1:length(year.i), lab=year.i, cex=1.5)
axis(2, cex=1.5)
title(main="Area in (km2) with confirmed fox rabies cases",
      xlab="Years", ylab="Area (km2)")



########################################################
#########   HR OF FOX RABIES BY COUNTRY    ############

## Set the variable for the loop
country.i <- sort(unique(foxrabies$COUNTRY))
country.i

## Create a dataframe
result.country <- data.frame(country=character(),
                     AREA=character(),
                     stringsAsFactors=FALSE)

## Set the loop
for (i in levels(country.i))
{
  data.rab.i <- foxrabies[foxrabies@data$COUNTRY==i,] # subset country data

  # Rasterize rabies cases points
  hr_raster3.i <- rasterize(data.rab.i, hr_raster2, field=1,
                            update=TRUE, updateValue=2)

  # Percentage of infected area
  freq.rabies.i <- freq(hr_raster3.i)
  infected.i <- freq.rabies.i[2,2]
  per.infected.i <- infected.i/area.we*100
  per.infected.i
  print(per.infected.i)
  per.infected.c.df <- as.data.frame(per.infected.i, i)
  result.country <- rbind(result.country, per.infected.c.df)
}


## PLOT 3 - Pie Chart with Percentages
result.country$per.infected.i <- result.country$per.infected.i*100/per.infected
# Calculate percentage of each country corresponding to total area infected.
result.country$per.infected.i <- round(result.country$per.infected.i,
                                       digits = 1) # only one decimal

lbls <- country.i # set country labels
percent<- c(result.country$per.infected.i) # set percentage labels
lbls <- paste(lbls, percent) # mix percentages and labels in the same one
lbls <- paste(lbls,"%",sep="") # add % symbol to labels

# Plot the pie
pie(result.country$per.infected.i,labels = lbls, col=rainbow(length(lbls)),
    main="Contribution of each country \nto total infected area in WE")
# Reference: http://www.statmethods.net/graphs/pie.html


#### SAVE RABIES RASTER
#writeRaster(hr_raster3, filename="output/rabies_raster.grd", overwrite=TRUE)

#writeRaster(hr_raster3, filename="output/rabies_raster.tif", format="GTiff", overwrite=TRUE)
########## DONT KNOW IF THESE ARE NECESSARY!