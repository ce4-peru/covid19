###### incidencia lima metropolitana ccasos COVID19#####

library(ggplot2)
library(ggmap)
library(data.table)
library(maptools)
library(rgeos)
library(Cairo)
library(scales)
library(RColorBrewer)
library(rgdal)
library(sp)
library(plyr)
library(tmap)
library(dplyr)


setwd("~/covid19PE-LIEZ/")

caso_covid <- fread("~/covid19PE-LIEZ/data/COVID19 casos - Distritos_Lima.csv")
setnames(caso_covid,old = "Distrito",new = "NAME_3")
caso_covid$Pob_distrito<-as.numeric(caso_covid$Pob_distrito)
caso_covid$Casos<-as.numeric(caso_covid$Casos)
caso_covid$incidencia<-caso_covid$Casos/caso_covid$Pob_distrito

options(digits = 2)

caso_covid$incidencerate_100<-(caso_covid$incidencia*100000)

unique(caso_covid$NAME_3)
# [1] "Santiago de Surco"       "Miraflores"             
# [3] "Jesus Maria"             "Lima"                   
# [5] "San Isidro"              "San Miguel"             
# [7] "San Martin de Porres"    "Villa El Salvador"      
# [9] "San Juan de Lurigancho"  "Pueblo Libre"           
# [11] "El Agustino"             "Comas"                  
# [13] "Carabayllo"              "San Borja"              
# [15] "Surquillo"               "Independencia"          
# [17] "Rimac"                   "Chorrillos"             
# [19] "San Juan de Miraflores"  "Puente Piedra"          
# [21] "Villa Maria del Triunfo" "Magdalena del Mar"      
# [23] "La Molina"               "Ate"                    
# [25] "Lince"                   "Bre-a"                  
# [27] "Callao"                  "Los Olivos"             
# [29] "Ancon"                   "La Punta"               
# [31] "La Victoria"             "San Luis"               
# [33] "Barranco"                "Cieneguilla"            
# [35] "Chaclacayo"              "Lurin"                  
# [37] "No precisan"

# Cambiar "Bre-a". Que sea Brena.
caso_covid$NAME_3 <- recode(caso_covid$NAME_3, `Bre-a` = "Brena")
unique(caso_covid$NAME_3)

# Read the neighborhood shapefile data and plot
shpfile_lima <- "data/lima_shapefiles/LIM_province_district_nosigns.shp"
sh_lima <- rgdal::readOGR(shpfile_lima)
plot(sh_lima)

## Merge data
tmp1 <- sh_lima 
tmp1_map <- tmp1
tmp1_map@data <- join(sh_lima@data,caso_covid, by="NAME_3") # shp with data
tmp1_map_df <- tmp1_map@data
tmp1_map@data$Casos[is.na(tmp1_map@data$Casos)]<-0 #
# tmp1_map@data$brks <- cut(tmp1_map@data$Casos, 
#                           breaks=c(-0.1, 0.9, 1.9, 5, 10, 20, 30, 35), 
#                           labels=c("0", "1", "2 - 5", "6 - 10", 
#                                    "11 - 20", "21 - 30", "> 30"))
names(tmp1_map@data) # check variables
tmp1_map_df <- tmp1_map@data

# Create the map qtm
col <- c("ivory2", "lightcyan1", "lightblue2", #"lightskyblue", 
         "royalblue", "blue2", "navyblue", "black")
# try snow2 too

# interactive view
tmap_mode("view")

# crear mapa
qtm <- qtm(tmp1_map, fill = "incidencerate_100", 
             fill.palette = col,
             fill.title="Incidencia por 100,000 Hab") + 
  # tm_text("NAME_3", size="0.008", scale=1, root=4, 
  #         size.lowerbound = .1, bg.color="white", bg.alpha = .15, 
  #         legend.size.show = FALSE) +
    tm_text("NAME_3",size = 0.85)+
    tm_layout(paste("Lima Metropiltana casos COVID19 "), legend.title.size = 1.1, 
            legend.text.size = .8, legend.bg.color = "white") #+ 
    #tm_view(set.view = c(lon = 15, lat = 48, zoom = 10))
   # tm_view(set.view = c(zoom = 10))
  
print(qtm)
tmap_save(qtm, paste("20202403_lima_COVID19_Incidencerate_v3",".png", sep=""), 
        width=1920, height=1380)
# 












