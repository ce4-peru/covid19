##'**MRES PROJECT**
##'
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'
##'**MICAELA DE LA PUENTE L.**
##'
##'**This script creates maps with rabies cases distribution**
##############################################################

######################################################
# FIRST SET WORKING DIRECTORY IN RSTUDIO
# i.e. Session > Set Working Directory > To Source File Location
rm(list=ls())

######################################################
library(maptools)
library(maps)
library(mapdata)
library(mapproj)
library(rworldmap)
library(ggmap)
library(ggplot2)
library(Rcpp)
library(foreign)
library(devtools)
library(animation)
######################################################

## Call the script of rabies dataset preparation.
foxrabies.we <-read.csv("output/foxrabies_final_full.csv")


########    WORLDHIREs MAP   #########

#### Create a plot for Western European countries

## Select the Western European countries
wemap <- map("worldHires", regions=c("Germany", "Austria", "Belgium",
                                     "France", "Luxembourg", "The Netherlands",
                                     "Switzerland", "Monaco", "Liechtenstein"),
             xlim=c(-5.65,17.35), ylim=c(41,55.5), # to look in the right  size
             fill=TRUE, # state if countries are colored
             col="grey", #resolution=1,
             plot=FALSE) # state map resolution



## Plot a map of the section of Europe we like to show
#png("output/we.map_wh.png", width=4, height=4, units="in", res=300)

map("worldHires", xlim=c(-5.5,17), ylim=c(41.5,55))

# Color the Western European countries
map(wemap, col="light green", fill=TRUE, add=TRUE)
map.axes() # add axes
title(main="Western Europe")  # add title
# NO LABELS FOR COUNTRIES
#dev.off()

# Add data of fox rabies
png("output/we.maprabies_wh.png", width=4, height=4, units="in", res=300)

map("worldHires", xlim=c(-5.5,17), ylim=c(41.5,55))
map(wemap, col="light green", fill=TRUE, add=TRUE)
map.axes() # add axes
title(main="Fox rabies in Western Europe")
points(foxrabies.we$x, foxrabies.we$y, pch=21, bg="red", cex=0.01)
dev.off()



#####  CREATE PLOTS BY COUNTRIES
## Create an object with the countries
co.i <- unique(foxrabies.we$COUNTRY)
levels(co.i)
# [1] "Austria"         "Belgium"         "France"          "Germany"
# [5] "Luxembourg"      "Switzerland"     "The Netherlands"

## Set the for loop function
for(i in levels(co.i)){
  png(paste("output/", i, "_rabies_wh", ".png", sep=""),
      width=4, height=4, units="in", res=300)

  co.i.map <- map("worldHires", regions = co.i, fill=TRUE, col="grey", plot=FALSE)

  map("worldHires", xlim=c(-5.5,17), ylim=c(41.5,55)) # Plot the map of WE territory
  map(wemap, col="grey", fill=TRUE, add=TRUE) # Color the Western European countries
  map(co.i.map, col="light green", fill=TRUE, add=TRUE) # Color the country with another color

  # Add the fox rabies data
  foxrabies.i <- foxrabies.we[which(foxrabies.we$COUNTRY==i),]
  points(foxrabies.i$x, foxrabies.i$y, pch=21, bg="red", cex=0.01)
  title(main=i)  # add title
  map.axes() # add axes
 dev.off()
}
# No map of Netherlands because the spelling is different in the dataset and
# for worldhires.
# THIS?:
# Error in map.poly(database, regions, exact, xlim, ylim, boundary, interior,  :
#                     no recognized region names


########################################################################
#########      MAP IN GOOGLE MAPS WITH LONG-LAT DATA        ############


#### Map with full rabies cases

#pdf("output/WEggplot.pdf", onefile = TRUE)

## Map of Europe focus on our study area
map <- get_map(location = c(lon = 8, lat = 48),     # select Europe
               zoom = 5,                # 5 to set a higher scale.
               maptype = "terrain",     # setting easy to visualise
               source = "google")       # the source to load the map from
ggmap(map) # ggmap function to load map of Europe - plots map as a layer over a base graphic


pdf("output/WEggplot_Full.pdf", onefile = TRUE)
## Map of our study are with rabies cases
ggmap(map) + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = foxrabies.we, # Add points for sites using latitude and longitude.
             aes(x = x, y = y), # Set longitudes and latitudes on axes.
             alpha = 0.5, # Define the type of point to be used on the map.
             color = "red", # Set colour of the points
             size = 0.2,
             position = position_jitter(width = 0.1, height = 0.1))
dev.off()



#### Maps with fox rabies cases by years in WE
## Select years
yearwe.i<-sort((unique(foxrabies.we$YEAR)), decreasing = FALSE)

## by year
#pdf("output/WEyears_ggplot.pdf", onefile = TRUE)
for(i in seq_along(yearwe.i)){

  western.ggplot <- foxrabies.we[which(foxrabies.we$YEAR==yearwe.i[i]),] # Create the map with fox rabies data

  maps <- ggmap(map) + xlab("Longitude") + ylab("Latitude") +
    geom_point(data = western.ggplot,
               aes(x = x, y = y),
               alpha = 0.5, color = "red", size = 0.5,
               position = position_jitter(width = 0.1, height = 0.1)) + ggtitle(paste("WesternEu", yearwe.i[i],sep="")) # add a title
  # Plot the map
  plot(maps)

  # Save plots in png files
  ggsave(paste("output/WE", yearwe.i[i],  ".png",sep=""), dpi=300, width=6, height=5) # too heavy
}
#dev.off()



#### Maps with fox rabies cases by countries
# Countries are agrouped according to the zoom level that they need to visualize
# better the country.

### MAPS OF FRANCE
foxrabies.f <- foxrabies.we[which(foxrabies.we$COUNTRY=="France"),] # Subset the country data
maps <- ggmap(map) + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = foxrabies.f,aes(x = X, y = Y), # Create a map with fox rabies data
             alpha = 0.5, color = "red", size = 0.05,
             position = position_jitter(width = 0.1, height = 0.1))
plot(maps)
#ggsave(paste("output/France/France",  ".png",sep=""), dpi=300, width=6, height=5)
#Comments: In France, clusters are in the northern, northeaster and central eastern part of the country. Only isolated cases were in the southeastern part, but not extended totally to the south.


## Maps with rabies cases by years in France

# Subset data
yearfran.i<-sort((unique(foxrabies.f$YEAR)), decreasing = FALSE)

# Set the for loop function
for(i in seq_along(yearfran.i))
{
  fran.gg <- foxrabies.f[which(foxrabies.f$YEAR==yearfran.i[i]),]
  maps <-   ggmap(map) + xlab("Longitude") + ylab("Latitude") +
    geom_point(data = fran.gg, aes(x = X, y = Y), alpha = 0.5,
               color = "red", size = 0.5,
               position = position_jitter(width = 0.1, height = 0.1)) +
    ggtitle(paste("France", yearfran.i,sep=" "))
  plot(maps)
  #ggsave(paste("output/France/Fr"
  #             , yearfran.i[i],  ".png",sep=""), dpi=300, width=6, height=5)
}


### MAPS OF GERMANY AND AUSTRIA
co.i <- c("Germany", "Austria")
print(co.i)

for(i in co.i){
  foxrabies.i <- foxrabies.we[which(foxrabies.we$COUNTRY==i),]
  maps <- ggmap(map) + xlab("Longitude") + ylab("Latitude") +
    geom_point(data = foxrabies.i, aes(x = x, y = y),
               alpha = 0.5, color = "red", size = 0.5,
               position = position_jitter(width = 0.1, height = 0.1))
  plot(maps)
  ggsave(paste("output/"
               , i, "/", i, ".png",sep=""), dpi=300, width=6, height=5)

  ## Maps by years
  year.i<-sort((unique(foxrabies.i$YEAR)), decreasing = FALSE)

  for(j in seq_along(year.i)){
    year.gg <- foxrabies.i[which(foxrabies.i$YEAR==year.i[j]),]
    maps <- ggmap(map) + xlab("Longitude") + ylab("Latitude") +
      geom_point(data = year.gg, aes(x = x, y = y), alpha = 0.5,
                 color = "red", size = 0.5,
                 position = position_jitter(width = 0.1, height = 0.1)) +
      ggtitle(paste(i, year.i[j],sep=" "))
    plot(maps)
    ggsave(paste("output/",
                i , "/", i ,year.i[j],  ".png",sep=""), dpi=300, width=6, height=5)
  }}

# Comments: Fox rabies is distributed in almost all Austria territory.
# In Germany there is a similar situation, except northeastern areas some southern eastern and souther western areas.



### MAPS OF SWITZERLAND, BELGIUM AND THE NETHERLANDS
# select the countries
co.i <- c("Switzerland", "Belgium", "The Netherlands")

for(i in co.i){
  foxrabies.i <- foxrabies.we[which(foxrabies.we$COUNTRY==i),]
  maps <- ggmap(map) + xlab("Longitude") + ylab("Latitude") +
    geom_point(data = foxrabies.i, aes(x = X, y = Y),
               alpha = 0.5, color = "red", size = 0.5,
               position = position_jitter(width = 0.1, height = 0.1))
  plot(maps)
  #ggsave(paste("output/", i, "/", i,  ".png",sep=""), dpi=300, width=6, height=5)

  ## Maps by years
  year.i<-sort((unique(foxrabies.i$YEAR)), decreasing = FALSE)

  for(j in seq_along(year.i)){
    year.gg <- foxrabies.i[which(foxrabies.i$YEAR==year.i[j]),]
    maps <- ggmap(map) + xlab("Longitude") + ylab("Latitude") +
      geom_point(data = year.gg, aes(x = X, y = Y), alpha = 0.5,
                 color = "red", size = 0.5,
                 position = position_jitter(width = 0.1, height = 0.1)) +
      ggtitle(paste(i, year.i[j],sep=" "))
    plot(maps)
    #ggsave(paste("output/",
    #             i , "/", i, year.i[j],  ".png",sep=""), dpi=300, width=6, height=5)
  }
}

# Comments Switzerland : Most cases are limited to the nortwestern side of
# the country, bordering with France and Germany. Less cases are present in
# central and central western Switzerland from the north to the south.
# A small cluster of cases is in Geneva, one of the main cities in Switzerland.

# Comment Belgium: There are cluster of infected cases in southern corner of Belgium up to the whole border with Luxembourg and the southwestern border with France. There are two other clusters at central and central-western areas, between Charleroi and Namur and Liege and Mastriche, respectively.

# Comments The Netherlands: Only a few cases are limited to two cluster
# in Southern area part, limiting with Germany and Belgium.


###  MAP OF LUXEMBOURG
foxrabies.l <- foxrabies.we[which(foxrabies.we$COUNTRY=="Luxembourg"),]
maps <- ggmap(map) + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = foxrabies.l, aes(x = X, y = Y),
             alpha = 0.5, color = "red", size = 0.7,
             position = position_jitter(width = 0.1, height = 0.1))

plot(maps)
#ggsave(paste("output/Luxembourg/Luxembourg",  ".png",sep=""), dpi=300, width=6, height=5)

## Comments: Cases are distributed along the territory. There is absence of cases only in the most eastern areas. .

## Map of Luxembourg by years
yearluxe.i<-sort((unique(foxrabies.l$YEAR)), decreasing = FALSE) # Subset data

for(i in seq_along(yearluxe.i)) {
  luxe.gg <- foxrabies.l[which(foxrabies.l$YEAR==yearluxe.i[i]),]
  maps <- ggmap(map) + xlab("Longitude") + ylab("Latitude") +
      geom_point(data = luxe.gg, aes(x = x, y = y), alpha = 0.5,
               color = "red", size = 0.7,
               position = position_jitter(width = 0.1, height = 0.1)) +
    ggtitle(paste("Luxembourg", yearluxe.i[i],sep=""))
  plot(maps)
  ggsave(paste("output/Luxembourg/Luxembourg"
                 , yearluxe.i[i],  ".png",sep=""), dpi=300, width=6, height=5)
}



###############################################################################
##############  CREATE THE LOOP for RABIES QUARTERLY CASES  ###################

## Subset the timesteps of rabies cases in WE
qua.i <-sort(unique(foxrabies.we$Qs), decreasing = FALSE)
print(qua.i)
# [1] 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46
#[31] 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76
#[61] 77 78 80 81 82 83 84 85 86 87 88 89 90 91 92 93 95

## Create a vector that comprises all timesteps (even those that had no cases)
quarter.first <- qua.i[1] # 17
quarter.last <- tail(qua.i, 1) # 95
qua.i.full <- c(quarter.first:quarter.last)
print(unique(qua.i.full))
#  [1] [1] 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46
# [31] 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76
# [61] 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95


## Select the Europe map
map <- get_map(location = c(lon = 8, lat = 48),
               zoom = 5,
               maptype = "terrain", source = "google")

# Plot the map
plotmap <- ggmap(map) + xlab("Longitude") + ylab("Latitude")

## Create a HTML video of following maps
setwd("output") # set saving address
saveHTML({

# Set the loop and print individual png. quarter maps
for(i in qua.i.full){

  if (i %in% qua.i) {

    # Subset the dataset by timesteps
    RC.i <- foxrabies.we[which(foxrabies.we$Qs==i),]

    # Add the rabies cases to the map
    plotmap1 <- plotmap +
      geom_point(data = RC.i, aes(x = x, y = y),
                 alpha = 0.5, color = "red", size = 0.4,
                 position = position_jitter(width = 0.1,
                                            height = 0.1)) +
      ggtitle(paste("Timestep", i, "-",
                    RC.i$YEAR, "Quarter", RC.i$QUARTER))

    # Plot the final map for each timestep
    plot(plotmap1)
    ggsave(paste("QUARTER_", i,".png", sep=""), dpi=300, width=6, height=5)

  } else {

    plotmap1 <- plotmap +
      ggtitle(paste("Timestep", i, "-",
                    RC.i$YEAR, "No rabies cases this quarter"))
    plot(plotmap1)
    ggsave(paste("QUARTER_", i,".png", sep=""), dpi=300, width=6, height=5)
  }
}
  }, img.name = "RCquarters__", imgdir = "RCquarters__",
htmlfile = "RCquarters__.html", #outdir = "output",
autobrowse = FALSE,
ani.height = 400, ani.width = 600, verbose = FALSE, autoplay = TRUE,
title = "Fox rabies cases per quarters")

# It worked
#animation option 'nmax' changed: 50 --> 79
#animation option 'nmax' changed: 79 --> 50
#HTML file created at: RCquarters.html



##############  CREATE THE LOOP for RABIES SEMESTER CASES  ###################

## Subset semesters of rabies cases in WE
sem.i <-sort(unique(foxrabies.we$Ss), decreasing = FALSE)
print(sem.i)
sem.i <- as.numeric(sem.i)
## PRINT SEMESTRAL RABIES CASES

## Set the loop and print individual png. quarter maps
saveHTML({

for(i in sem.i){
  RC.i <- foxrabies.we[which(foxrabies.we$Ss==i),]

  plotmap1 <- ggmap(map) + xlab("Longitude") + ylab("Latitude")

  plotmap1 <- plotmap1 +
    geom_point(data = RC.i, aes(x = x, y = y),
               alpha = 0.5, color = "red", size = 0.4,
               position = position_jitter(width = 0.1,
                                          height = 0.1)) +
    ggtitle(paste(RC.i$YEAR, "Semester", RC.i$Ss))

  plot(plotmap1)
  ggsave(paste("SEMESTER", i,".png", sep=""), dpi=300, width=6, height=5)
}
}, img.name = "RCsemesters1", imgdir = "RCsemesters1",
htmlfile = "RCsemesters1.html", #outdir = "output",
autobrowse = FALSE,
ani.height = 400, ani.width = 600, verbose = FALSE, autoplay = TRUE,
title = "Fox rabies cases per semesters")
# animation option 'nmax' changed: 50 --> 55
#animation option 'nmax' changed: 55 --> 50
#HTML file created at: RCsemesters1.html