
#Ruta
  setwd('/Users/ricardocasney/Documents/Papers/Rabies/Spatial models/Channels/bd')
  
#Packages
  library(maps)
  library(GISTools)
  
#Leer los archivos
  puentes<-read.csv("PUENTES.csv")
  #casos_rabia<-read.csv("Casos_Rabia_2015_16.csv")
  casos_rabia<-read.csv("Casos_Rabia_1year.csv")
  torrenteras<-read.csv("TORRENTERAS.csv")
  puntos_torrenteras<-read.csv("TORRENTERAS_A_ESCALA.csv")
  rio<-read.csv("Rio Chili.csv")
  houses <- read.csv("distritos_aqp.csv")
  
  #Convirtiendo a NUMERIC
  houses$long <- as.numeric(levels(houses$LONGITUDE)[houses$LONGITUDE])
  houses$lat <- as.numeric(levels(houses$LATITUDE)[houses$LATITUDE])
  
#Almacenando los campos "LONGITUD" y"LATITUD"
  puentes <- puentes[, c("long","lat")]
  casos_rabiaXY <- casos_rabia[, c("ident", "long","lat")]
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
    png("All_houses_Arequipa.png", width=wdt, height=hgt)
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
      png("All_houses_Arequipa_river_channels_rabid_dogs_30_timeline_new2.png", width=wdt, height=hgt)
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
  points(casos_rabiaXY$long, casos_rabiaXY$lat, col = "red", pch=19, cex = 1, cex.main=0.8, ylab="Latitude", 
         main = "Casos de rabia en Arequipa", xlab="Longitude")
  text(casos_rabiaXY$long[-c(24,21,5,15,14,16,18,28)], casos_rabiaXY$lat[-c(24,21,5,15,14,16,18,28)]+0.0038, 
       labels=casos_rabiaXY$ident[-c(24,21,5,15,14,16,18,28)], cex=0.8)
  text(casos_rabiaXY$long[c(24,21,5,15,14,16,18,28)], casos_rabiaXY$lat[c(24,21,5,15,14,16,18,28)]-0.0038, 
       labels=casos_rabiaXY$ident[c(24,21,5,15,14,16,18,28)], cex=0.8)
    #Legend
      #En la parte inferior izquierda  
        #legend(x=-71.605, y=-16.44, legend = "Torrenteras", col = "blue", lty=1, bty="n")
        #legend(x=-71.596, y=-16.447, bty="n",c("Puentes","Casos de rabia"),col=c("green","red"),pch=c(2,19))
      #En la parte superior derecha
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
  
