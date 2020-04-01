##'**MRES PROJECT**
##'
##'**FOX RABIES RETREAT IN WESTERN EUROPE**
##'
##'**MICAELA DE LA PUENTE L.**
##'
##'**This script shows the ORV extension per year and quarter**
###############################################################

## Load the ggplot2, ggmap and mapproj packages from the R library
library(ggplot2)
library(ggmap)
library(mapproj)
library(Rcpp)
library(foreign)
library(maptools)
library(maps)
library(mapdata)
library(devtools)
library(animation)
library(rgdal)

## Call the shp where we subset and correct the projections of the dataframe.
ORV_we <-readOGR("output/ORV_we_final1.shp","ORV_we_final1")

###########      PLOT GGMAPS OF ORV    ###########

### ORV in Western Europe ###

## fortify to restructre SPDF
eu <- ORV_we
eu@data$id <- row.names(eu)
eu_df <- fortify(eu, region="id")
eu.f <- eu_df

## Plot the polygons on their own coordinates.
ggplot(eu.f, aes(long, lat)) + geom_polygon(colour='blue', fill='white')

## Plot the Europe map with the polygons on the top.
map <- get_map(location = c(lon = 8, lat = 48),
               zoom = 5, # 5 to set the map a country.
               maptype = "terrain",     # setting easy to visualise
               source = "google")       # the source to load the map from

mymap <- ggmap(map) + xlab("Longitude") + ylab("Latitude")
mymap <- mymap + geom_polygon(aes(x = long, y = lat),
                              fill = "blue", size=.2,
                              color="black",  data = eu.f, alpha=.4)
mymap
## Not useful. Looks wierd because R goes crazy with layers of maps over maps.
## We will obtain the map we need later.


############### Plot ORV in Western Europe by years ######################

summary(ORV_we)
yearwe.i <-sort((unique(ORV_we$JAHR)), decreasing = FALSE)
yearwe.i

setwd('C:/Users/Micaela/Dropbox/Fox Rabies_MD/Scripts/output/movie/')

for(i in seq_along(yearwe.i))
{
  ORV.c <- ORV_we[which(ORV_we$JAHR==yearwe.i[i]),]

  # fortify to restructre SPDF
  eu <- ORV.c
  eu@data$id <- row.names(eu)
  eu_df <- fortify(eu, region="id")
  eu.f <- eu_df

  # Plot ggmap
  mymap <- ggmap(map) + xlab("Longitude") + ylab("Latitude")
  mymap <- mymap + geom_polygon(aes(x = long, y = lat, group=group),
                                fill = "blue", size=.2,
                                color="black",
                                data = eu.f, alpha=.4) +
    ggtitle(paste("ORV", yearwe.i[i]))

  plot(mymap)
  #ggsave(paste("output/WE/ORV_WE"
   #            , yearwe.i[i],  ".png",sep=""), dpi=300, width=6, height=5)
  ggsave(paste("ORV_WE"
              , yearwe.i[i],  ".png",sep=""), dpi=300, width=6, height=5)
}

# install package installr
# install package installr 2
library(installr)
#library(installr2)
library(animation)
#library(ImageMagick)
#install.packages("ImageMagick")
#install.packages("magick")
require(installr) - install.ImageMagick()

#$ convert *.png -delay 3  binom.gif
system("convert -delay 3 -loop 0 ORV_WE*.png example1.gif")
# Invalid Parameter - 3
#Warning message:
#  running command 'convert -delay 3 -loop 0 ORV_WE*.png example1.gif' had status 4 

#convert *.png -delay 3 -loop 0 binom.gif

system('"C:\\Program Files\\ImageMagick-7.0.3-Q16\\convert.exe" -delay 20 -loop 0 ORV_WE*.png animation.gif')
#Warning message:
#  running command '"C:\Program Files\ImageMagick-7.0.3-Q16\convert.exe" -delay 20 -loop 0 ORV_WE*.png animation.gif' had status 127 

ani.options('convert = "C:/Program Files/ImageMagick-7.0.3-Q16/convert.exe" -delay 3 ORV_WE*.png example1.gif')
#null

########### Plot ORV by quarters/season/timesteps #############

quarter <-sort(unique(ORV_we$VAC_LIST), decreasing = FALSE)
quarter

quarter.first <- quarter[1]
quarter.last <- tail(quarter, 1)
quarter.i <- c(quarter.first:quarter.last)
print(unique(quarter.i)) # 1 to 79
class(quarter.i)
#[1] "integer"

map <- get_map(location = c(lon = 8, lat = 48),
               zoom = 5, maptype = "terrain", source = "google")

plotmap <- ggmap(map) + xlab("Longitude") + ylab("Latitude")
for(i in quarter.i)
{
  if (i %in% quarter) {
    ORV.i <- ORV_we[which(ORV_we$VAC_LIST==quarter.i[i]),]

    # fortify to restructre SPDF
    eu.we <- ORV.i
    eu.we$id <- row.names(eu.we)
    eu.we_df <- fortify(eu.we, region="id")
    eu.we.f <- eu.we_df

    plotmap1 <- plotmap + geom_polygon(aes(x=long, y=lat, group=group),
                                       fill = "blue", size=.2,
                                       color="black",
                                       data = eu.we.f, alpha=0.5) +
      ggtitle(paste("Timestep", quarter.i[i], "-",
                    "ORV Campaign", unique(ORV.i$JAHR),
                    "Quarter", unique(ORV.i$VAC)))

        plot(plotmap1)
    ggsave(paste("output/WE/ORV_we_ts", i,".png", sep=""), dpi=300, width=6, height=5)

  } else {
    plotmap1 <- plotmap +
      ggtitle(paste("Timestep", quarter.i[i], "-",
                                        "No ORV Campaign this quarter of",
                    unique(ORV.i$JAHR)))
    plot(plotmap1)
    ggsave(paste("output/WE/ORV_we_ts", i,".png", sep=""), dpi=300, width=6, height=5)
    }}

# Works ok. This show the ORV campaings done each quarter.




########### ORV  by quarters - accumulated #############

# wHEN THEY OVERLAP THEY TURN DARKER.

map <- get_map(location = c(lon = 8, lat = 48),
               zoom = 5, # 5 to set the map a country.
               maptype = "terrain",     # setting easy to visualise
               source = "google")       # the source to load the map from

plotmap <- ggmap(map) + xlab("Longitude") + ylab("Latitude")

# Create the video
setwd("output")

saveHTML({

  # Set the loop
  for(i in quarter.i)
  {
    if (i %in% quarter)
    {
    ORV.i <- ORV_we[which(ORV_we$VAC_LIST==quarter.i[i]),]

    # fortify to restructre SPDF
    eu.we <- ORV.i
    eu.we$id <- row.names(eu.we)
    eu.we_df <- fortify(eu.we, region="id")
    eu.we.f <- eu.we_df

    plotmap1 <- plotmap + geom_polygon(aes(x=long, y=lat, group=group),
                                       fill = "blue", size=.2,
                                       color="black",
                                       data = eu.we.f, alpha=0.5) +
      ggtitle(paste("Timestep", quarter.i[i], "-",
                    "ORV Campaign", unique(ORV.i$JAHR),
                    "Quarter", unique(ORV.i$VAC)))

    plotmap <- plotmap1
    plot(plotmap)
  # ggsave(paste("ORV_we_acc", i,".png", sep=""), dpi=300, width=6, height=5)

  } else {
    plotmap1 <- plotmap + ggtitle(paste("Timestep", quarter.i[i], "-",
                                        "No ORV Campaign this quarter of",
                                        unique(ORV.i$JAHR)))
    plotmap <- plotmap1
    plot(plotmap)
   # ggsave(paste("ORV_we_acc", i,".png", sep=""), dpi=300, width=6, height=5)
  }}
   }, img.name = "ORVquarters3_ac", imgdir = "ORVquarters3_ac",
htmlfile = "ORVquarters3_ac.html", outdir = getwd(), autobrowse = FALSE,
ani.height = 400, ani.width = 600, verbose = FALSE, autoplay = TRUE,
title = "ORV per quarters - accumulated")

# Works ok.
## The last map shows all the ORV campaings polygons from 1987 to 2006.
