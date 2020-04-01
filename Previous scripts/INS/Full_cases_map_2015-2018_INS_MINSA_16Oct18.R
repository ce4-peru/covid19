### TAKE POINTS OUT ###
library(tidyr)
library(Stack)
library(SearchTrees)
library(rgdal)
library(sp)
#Packages
library(ggplot2)
library(data.table)
library(raster)
#library(car)
library(dplyr)
library(plyr)
#install.packages("tmap")
library(tmap)
library(ggmap)
library(rgeos)
library(rgdal)
library(maps)
library(GISTools)


setwd("~/Rabies/")
# case1_2018 <- readOGR("/Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case2_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case3_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case4_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case5_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case6_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case7_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case8_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case9_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case10_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case11_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case12_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case13_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case14_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case15_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case16_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case17_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case18_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case19_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case20_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC")
# case21_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case22_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case23_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case24_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case25_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case26_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case27_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case28_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case29_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case30_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case31_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case32_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case33_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case34_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case35_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case36_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case37_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case38_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# case39_2018 <- readOGR("Roberto/Caso 1 Rabia Canina CC.kml", "Caso 1 Rabia Canina CC") 
# 
#Leer los archivos
data18 <- read.csv("Casos_INS/Scripts/data/Casos_de_Rabia_2018_07_09_18.csv",
                   sep= ";")
data17 <- read.csv("Casos_INS/Scripts/data/Casos_de_Rabia_2017.csv",
                   sep= ";")
data16 <- read.csv("Casos_INS/Scripts/data/Casos_de_Rabia_2016.csv",
                   sep= ";")
data15 <- read.csv("Casos_INS/Scripts/data/Casos_de_Rabia_2015.csv",
                   sep= ";")
puentes<-read.csv("rabia/bd/PUENTES.csv")
torrenteras<-read.csv("rabia/bd/TORRENTERAS.csv")
puntos_torrenteras<-read.csv("rabia/bd/TORRENTERAS_A_ESCALA.csv")
rio<-read.csv("rabia/bd/Rio Chili.csv")
houses <- read.csv("rabia/bd/distritos_aqp.csv")

setwd("~/Rabies/Casos_INS/Scripts/data/Roberto")
cases_2018 <- readOGR("cases2018.shp", "cases2018")

# join dataset
plot(cases_2018, pch=21)

cases2018_coords <- as.data.frame(coordinates(cases_2018))
str(cases2018_coords)
cases2018_coords$ANIO <- 2018
cases2018_coords <- cases2018_coords[,c(1,2,4)]

cases_2018_attbutes <- cases_2018@data  # mami + mami = 130
cases2018 <- cbind(cases_2018_attbutes, cases2018_coords)
names(cases2018)
cases2018 <- cases2018[,c(14,13,12,1)] # columns
colnames(cases2018)[2] <- "long"
colnames(cases2018)[3] <- "lat"
colnames(cases2018)[4] <- "observ"
cases2018$observ
cases2018 <- cases2018[c(33,25,12,26,37, 
                          24,27,21,17,10,
                          36,18, 1,32,16,
                          11,30,34,19,7,
                          9,4,39,13,23,
                          15,2, 35,22,5,
                          14,28,29,8,38,
                          3,31,6,20),]
cases2018$ident <- 1:39
cases2018$ident <- paste("2018_", cases2018$ident)
cases2018 <- cases2018[,c(5,1:4)] # columns

# filas 35 y 15 tienen el mismo punto: son o casos 26 o 28

# Bind 2015 to 2017
#data <- rbind(data18, data17, data16, data15)
data17$ANIO <- 2017
data17$ident <- paste("2017_", data17$ident)

data16$ANIO <- 2016
data16$ident <- paste("2016_", data16$ident)

data15$ANIO <- 2015
data15$ident <- paste("2017_", data15$ident)

data <- rbind(data17, data16, data15)
names(data)

sum(is.na(data$lat)) # 0 NAs
sum(is.na(data$long)) # 0 NAs

data <- data[,c(2,34,4,3,7)]
colnames(data)[5] <- "observ"

# Add ident 30 del 2017
# Ulti we find the real gps
empty_data <- data.frame(ident = c(30, 361), ANIO = c(2017, 2017),
                         long = c(NA, NA), lat = c(NA, NA), 
                         observ = c("not found yet", "not found yet"))

# bind again
data <- rbind(data, empty_data)

#### join finally #### 
names(data)
names(cases2018)
data <- rbind(data, cases2018)
write.csv(data, "rabiescases_fullcoords15-18_16oct18.csv")

# Map
#Convirtiendo a NUMERIC
houses$long <- as.numeric(levels(houses$LONGITUDE)[houses$LONGITUDE])
houses$lat <- as.numeric(levels(houses$LATITUDE)[houses$LATITUDE])
#Almacenando los campos "LONGITUD" y"LATITUD"
puentes <- puentes[, c("long","lat")]
casos_rabiaXY <- data[, c("ident", "long","lat")]
torrenteras <- torrenteras[, c("ident","long","lat")]
rio <- rio[, c("ident","long","lat")]
puntos_torrenteras <- puntos_torrenteras[, c("long","lat")]
houses <- houses[, c("long", "lat")]

#=================================================================================>
#                                 MAPPING THE DATA                                     -|
#=================================================================================>

# Regular parameters
wdt <- 840
hgt <- 660

#--------------------------------------------------------------------------------->

#Only houses in Arequipa
png("All_houses_Arequipa_2018.png", width=wdt, height=hgt)
plot(houses$long, houses$lat, col = "lightgray", pch = ".", ann=FALSE, xaxt='n',
     xlim=c(-71.60,-71.45), ylim=c(-16.52, -16.29), yaxt='n')
legend(x=-71.506, y=-16.32, legend = "  Houses", col="lightgray", pch=20, bty="n", cex=1.2)
map.scale(x=-71.57, y=-16.5, len=0.05, ndivs = 5, units="km")
arrows(x0=-71.46,y0=-16.358,y1=-16.346, lwd=3, length=0.15)
text(-71.46,-16.365, "N", cex=1.2)
dev.off()  

#--------------------------------------------------------------------------------->

#Houses and RIVER in Arequipa
png("All_houses_Arequipa_river.png", width=wdt, height=hgt)
plot(houses$long, houses$lat, col = "lightgray", pch = ".", ann=FALSE, xaxt='n',
     xlim=c(-71.60,-71.45), ylim=c(-16.52, -16.29), yaxt='n')
#Chili River
x<-NULL
y<-NULL
n_row <- nrow(rio)+1
for (i in 2:n_row) {
  if (!is.na(rio[i,2])) {
    x<-c(x,rio[i,2])
    y<-c(y,rio[i,3])
  }
  else{
    lines(x,y,col="cornflowerblue",lwd=4) #deepskyblue
    x<-NULL
    y<-NULL
  }
}
legend(x=-71.51, y=-16.31, legend = "River", col = "cornflowerblue", lwd=4, bty="n", cex=1.2)
legend(x=-71.506, y=-16.32, legend = "  Houses", col="lightgray", pch=20, bty="n", cex=1.2)
map.scale(x=-71.57, y=-16.5, len=0.05, ndivs = 5, units="km")
arrows(x0=-71.46,y0=-16.358,y1=-16.346, lwd=3, length=0.15)
text(-71.46,-16.365, "N", cex=1.2)
dev.off()  

#--------------------------------------------------------------------------------->

#Houses, River and Channels Arequipa
png("All_houses_Arequipa_river_channels.png", width=wdt, height=hgt)
plot(houses$long, houses$lat, col = "lightgray", pch = ".", ann=FALSE, xaxt='n',
     xlim=c(-71.60,-71.45), ylim=c(-16.52, -16.29), yaxt='n')
#Channels
x<-NULL
y<-NULL
n_row <- nrow(torrenteras)+1
for (i in 2:n_row) {
  if (!is.na(torrenteras[i,2])) {
    x<-c(x,torrenteras[i,2])
    y<-c(y,torrenteras[i,3])
  }
  else{
    lines(x,y,col="gray30", lwd=4)
    x<-NULL
    y<-NULL
  }
}

# White interior of channels
x<-NULL
y<-NULL
n_row <- nrow(torrenteras)+1
for (i in 2:n_row) {
  if (!is.na(torrenteras[i,2])) {
    x<-c(x,torrenteras[i,2])
    y<-c(y,torrenteras[i,3])
  }
  else{
    lines(x,y,col="white", lwd=1)
    x<-NULL
    y<-NULL
  }
}

#Chili River
x<-NULL
y<-NULL
n_row <- nrow(rio)+1
for (i in 2:n_row) {
  if (!is.na(rio[i,2])) {
    x<-c(x,rio[i,2])
    y<-c(y,rio[i,3])
  }
  else{
    lines(x,y,col="cornflowerblue",lwd=4) #deepskyblue
    x<-NULL
    y<-NULL
  }
}
legend(x=-71.51, y=-16.30, legend = "Water channel", col="gray30", lwd=4, bty="n", cex=1.2)
legend(x=-71.51, y=-16.30, legend = " ", col="white", lwd=1.2, bty="n", cex=1.2)    
legend(x=-71.51, y=-16.31, legend = "River", col = "cornflowerblue", lwd=4, bty="n", cex=1.2)
legend(x=-71.506, y=-16.32, legend = "  Houses", col="lightgray", pch=20, bty="n", cex=1.2)
map.scale(x=-71.57, y=-16.5, len=0.05, ndivs = 5, units="km")
arrows(x0=-71.46,y0=-16.358,y1=-16.346, lwd=3, length=0.15)
text(-71.46,-16.365, "N", cex=1.2)
dev.off()  


#--------------------------------------------------------------------------------->

#Houses, River, Channels and Dogs in Arequipa
wdt <- 1000

png("All_houses_Arequipa_river_channels_rabid_dogs_30_timeline_new2_2018_2.png", width=wdt, height=hgt)
plot(houses$long, houses$lat, col = "lightgray", pch = ".", ann=FALSE, xaxt='n',
     #xlim=c(-71.60,-71.45),
     xlim=c(-71.65,-71.45), ylim=c(-16.52, -16.29), yaxt='n')

#Channels
x<-NULL
y<-NULL
n_row <- nrow(torrenteras)+1
for (i in 2:n_row) {
  if (!is.na(torrenteras[i,2])) {
    x<-c(x,torrenteras[i,2])
    y<-c(y,torrenteras[i,3])
  }
  else{
    lines(x,y,col="gray30", lwd=4)
    x<-NULL
    y<-NULL
  }
}

# White interior of channels
x<-NULL
y<-NULL
n_row <- nrow(torrenteras)+1
for (i in 2:n_row) {
  if (!is.na(torrenteras[i,2])) {
    x<-c(x,torrenteras[i,2])
    y<-c(y,torrenteras[i,3])
  }
  else{
    lines(x,y,col="white", lwd=1)
    x<-NULL
    y<-NULL
  }
}

#Bridges
#   points(puentes, col = "darkgreen", pch=2, cex = 1)

#Chili River
x<-NULL
y<-NULL
n_row <- nrow(rio)+1
for (i in 2:n_row) {
  if (!is.na(rio[i,2])) {
    x<-c(x,rio[i,2])
    y<-c(y,rio[i,3])
  }
  else{
    lines(x,y,col="cornflowerblue",lwd=4) #deepskyblue
    x<-NULL
    y<-NULL
  }
}
# Rabies cases
points(casos_rabiaXY$long, casos_rabiaXY$lat, col = "red", 
       pch=19, cex = 1.2, cex.main=0.8, ylab="Latitude", 
       main = "Casos de rabia en Arequipa", xlab="Longitude")
#Legend
legend(x=-71.506, y=-16.29, legend = "  Rabid dog", col="red", pch=19, cex=1.2, bty="n")
legend(x=-71.51, y=-16.30, legend = "Water channel", col="gray30", lwd=4, bty="n", cex=1.2)
legend(x=-71.51, y=-16.30, legend = " ", col="white", lwd=1.2, bty="n", cex=1.2)    
legend(x=-71.51, y=-16.31, legend = "River", col = "cornflowerblue", lwd=4, bty="n", cex=1.2)
legend(x=-71.506, y=-16.32, legend = "  Houses", col="lightgray", pch=20, bty="n", cex=1.2)
map.scale(x=-71.47, y=-16.45, len=0.04, ndivs = 4, units="km")
arrows(x0=-71.46,y0=-16.358,y1=-16.346, lwd=3, length=0.15)
text(-71.46,-16.365, "N", cex=1.2)

dev.off()
#  dev.print(device=pdf, "map_channels.pdf", width=wdt, height=hgt)


## FOR GRANT
wdt <- 840
# plot
pdf("All_houses_Arequipa_river_channels_rabid_dogs_30_timeline_new2_2018_1.pdf", 
    width=wdt, height=hgt, onefile = T, paper="a4r")
#png("Casos_INS/Scripts/outputs/All_houses_Arequipa_river_channels_rabid_dogs_30_timeline_new2_2018_11.png", width=wdt, height=hgt)
plot(houses$long, houses$lat, col = "lightgray", pch = ".", ann=FALSE, xaxt='n',
     #xlim=c(-71.60,-71.45),
     xlim=c(-71.65,-71.475), ylim=c(-16.477, -16.29), yaxt='n')
#Channels
x<-NULL
y<-NULL
n_row <- nrow(torrenteras)+1
for (i in 2:n_row) {
  if (!is.na(torrenteras[i,2])) {
    x<-c(x,torrenteras[i,2])
    y<-c(y,torrenteras[i,3])
  }
  else{
    lines(x,y,col="gray30", lwd=4)
    x<-NULL
    y<-NULL
  }
}

# White interior of channels
x<-NULL
y<-NULL
n_row <- nrow(torrenteras)+1
for (i in 2:n_row) {
  if (!is.na(torrenteras[i,2])) {
    x<-c(x,torrenteras[i,2])
    y<-c(y,torrenteras[i,3])
  }
  else{
    lines(x,y,col="white", lwd=1)
    x<-NULL
    y<-NULL
  }
}

#Bridges
#   points(puentes, col = "darkgreen", pch=2, cex = 1)

#Chili River
x<-NULL
y<-NULL
n_row <- nrow(rio)+1
for (i in 2:n_row) {
  if (!is.na(rio[i,2])) {
    x<-c(x,rio[i,2])
    y<-c(y,rio[i,3])
  }
  else{
    lines(x,y,col="cornflowerblue",lwd=4) #deepskyblue
    x<-NULL
    y<-NULL
  }
}
# Rabies cases
points(casos_rabiaXY$long, casos_rabiaXY$lat, col = "red", 
       pch=19, cex = 1.5, cex.main=0.8, ylab="Latitude", 
       main = "Casos de rabia en Arequipa", xlab="Longitude")
#Legend
legend(x=-71.649, y=-16.402, legend = "  Rabid dog", col="red", pch=19, cex=2.7, bty="n")
legend(x=-71.659, y=-16.417, legend = "Water channel", col="gray30", lwd=4, bty="n", cex=2.7)
legend(x=-71.659, y=-16.417, legend = " ", col="white", lwd=1.2, bty="n", cex=2.7)    
legend(x=-71.659, y=-16.432, legend = "River", col = "cornflowerblue", lwd=4, bty="n", cex=2.7)
legend(x=-71.649, y=-16.447, legend = "  Houses", col="lightgray", pch=20, bty="n", cex=2.7)
map.scale(x=-71.502, y=-16.451, len=0.04, ndivs=4, tcol = "white", 
          units="km")
arrows(x0=-71.497,y0=-16.320,y1=-16.308, lwd=3, length=0.15)
text(-71.497,-16.329, "N", cex=1.9)
text(-71.522,-16.456, "0", cex=1.9)
text(-71.512,-16.456, "1", cex=1.9)
text(-71.502,-16.456, "2", cex=1.9)
text(-71.492,-16.456, "3", cex=1.9)
text(-71.482,-16.456, "4", cex=1.9)
text(-71.475,-16.456, "km", cex=1.9)
dev.off()
