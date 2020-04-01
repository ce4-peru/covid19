###### incidencia de Peru casos COVID19#####

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

setwd("~/ce4-peru.github.io/")

## Call data
# shapefile de peru por regiones
shpfile_peru <- "data/gadm0-1-2-3/peru_1.shp"
sh_peru <- rgdal::readOGR(shpfile_peru)

# casos de covid
caso_covid <- fread("~/ce4-peru.github.io/data/modificadas/covidPE_IND_20200327_MD_clean.csv")

# tabla de poblacion por regiones
pob_region <- read.csv("data/Poblacion por region.csv")

## Check the neighborhood shapefile data and plot
sh_peru@data$NAME_1
# Lima and Lima Province should be the same.

# Que quede solo una Lima
sh_peru$NAME_1 <- recode(sh_peru$NAME_1, `Lima Province` = "Lima")
unique(sh_peru$NAME_1)
sh_peru$NAME_1 <- factor(sh_peru$NAME_1)
levels(sh_peru@data$NAME_1)

# Change names
str(sh_peru@data)
levels(sh_peru$NAME_1) <- c(levels(sh_peru$NAME_1), 
                            c("Apurimac", "Huanuco", 
                              "Junin", "San Martin"))
sh_peru_df <- sh_peru@data
sh_peru[3,6] <- "Apurimac"
sh_peru[9,6] <- "Huanuco"
sh_peru[12,6] <- "Junin"
sh_peru[23,6] <- "San Martin"
sh_peru_df <- sh_peru@data

sh_peru@data$NAME_1 <- factor(sh_peru@data$NAME_1)
levels(sh_peru@data$NAME_1)
# [1] "Amazonas"      "Ancash"        "Arequipa"      "Ayacucho"     
# [5] "Cajamarca"     "Callao"        "Cusco"         "Huancavelica" 
# [9] "Ica"           "La Libertad"   "Lambayeque"    "Lima"         
# [13] "Loreto"        "Madre de Dios" "Moquegua"      "Pasco"        
# [17] "Piura"         "Puno"          "Tacna"         "Tumbes"       
# [21] "Ucayali"       "Apurimac"      "Huanuco"       "Junin"        
# [25] "San Martin"  

## plotting shapefiles
plot(sh_peru)

# Transformar a mayusculas
sh_peru[[6]] <- toupper(sh_peru[[6]])
str(sh_peru@data$NAME_1)
unique(sh_peru@data$NAME_1)
# [1] "AMAZONAS"      "ANCASH"        "APURIMAC"      "AREQUIPA"      "AYACUCHO"     
# [6] "CAJAMARCA"     "CALLAO"        "CUSCO"         "HUANUCO"       "HUANCAVELICA" 
# [11] "ICA"           "JUNIN"         "LA LIBERTAD"   "LAMBAYEQUE"    "LIMA"         
# [16] "LORETO"        "MADRE DE DIOS" "MOQUEGUA"      "PASCO"         "PIURA"        
# [21] "PUNO"          "SAN MARTIN"    "TACNA"         "TUMBES"        "UCAYALI" 


## Check data of cases
names(caso_covid)
colnames(caso_covid)[1] <- "CE4_ID"
colnames(caso_covid)[7] <- "NAME_1"
unique(caso_covid$NAME_1)
# [1] "LIMA"          "AREQUIPA"      "HUANUCO"       "ICA"           "CUSCO"        
# [6] "ANCASH"        "CALLAO"        "LA LIBERTAD"   "LAMBAYEQUE"    "PIURA"        
# [11] "LORETO"        "MADRE DE DIOS" "SAN MARTIN"    "JUNIN"         "TUMBES"       
# [16] "CAJAMARCA"

# Eliminar NAs
#caso_covid <- caso_covid[caso_covid$NAME_1 != ""]
#caso_covid$NAME_1[caso_covid$Caso_ID %in% c(10,11)] <- "Huanuco"
#caso_covid$NAME_1[caso_covid$NAME_1 == "La libertad"] <- "La Libertad"

casos_region <- data.frame(table(caso_covid$NAME_1))
colnames(casos_region)[1] <- "REGION"
colnames(casos_region)[2] <- "NUMERO_CASOS"

## Checar csv de poblaciones regionales
names(pob_region) # "REGION"    "POBLACION"
pob_region[[1]] <- toupper(pob_region[[1]])
unique(pob_region$REGION) # todo ok

## Merge cases with populations
region_values <- merge(casos_region, pob_region, by="REGION", 
                       all.x=T, all.y=T)


## Calcula incidencias
region_values$NUMERO_CASOS <- as.numeric(region_values$NUMERO_CASOS)
region_values$POBLACION <- as.numeric(as.character(region_values$POBLACION))

region_values$incidencia <- region_values$NUMERO_CASOS/region_values$POBLACION
options(digits = 2)

region_values$incidencerate_100 <-(region_values$incidencia*100000)

## recode the names of the region_values data frame
setnames(region_values, old = "REGION",new = "NAME_1")
levels(region_values$NAME_1)

## Merge data with shapefile 
tmp1 <- sh_peru
tmp1_map <- tmp1
tmp1_map@data <- join(sh_peru@data,region_values, by="NAME_1") # shp with data
tmp1_map_df <- tmp1_map@data
tmp1_map@data$NUMERO_CASOS[is.na(tmp1_map@data$NUMERO_CASOS)]<-0 #
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
             fill.title="Incidencia por \n100,000 Hab") + 
  # tm_text("NAME_3", size="0.008", scale=1, root=4, 
  #         size.lowerbound = .1, bg.color="white", bg.alpha = .15, 
  #         legend.size.show = FALSE) +
    tm_text("NAME_1",size = 0.45)+
    tm_layout(paste("Casos por Departamento COVID19 "), 
              legend.title.size = 0.8, 
            legend.text.size = .6, legend.bg.color = "white",
            legend.position = c("left","bottom")) #+ 
    #tm_view(set.view = c(lon = 15, lat = 48, zoom = 10))
   # tm_view(set.view = c(zoom = 10))
  
print(qtm)
 tmap_save(qtm, 
          paste("outputs_covid19/20200327_Dep_COVID19_Incidencerate_v2",".png", sep=""), 
         width=2300, height=1380)

# Loop para plotear por dia
unique(caso_covid$FECHA)

# eliminar fechas ""
# caso_covid <- caso_covid[caso_covid$Fecha != "", ]

# crear vector
date.i <- unique(sort(caso_covid$FECHA))

# crear una fecha para titulo
#caso_covid$Fecha_titulo <- format(as.Date(caso_covid$Fecha, 
#                                   format = "%d/%m/%Y"), "%d-%m-%Y")

pdf("outputs_covid19/20200327_Dep_Incidencerate_31mar20.pdf", onefile = TRUE)

for(i in date.i){
  print(i)
  fecha.i <- caso_covid[caso_covid$FECHA == i,]

  casos_region <- data.frame(table(fecha.i$NAME_1))
  colnames(casos_region)[1] <- "REGION"
  colnames(casos_region)[2] <- "NUMERO_CASOS"
  
  ## Merge cases with populations
  region_values <- merge(casos_region, pob_region, 
                         by="REGION", 
                         all.x=T, all.y=T)
  
  # NAs en numero de casos son 0s
  region_values$NUMERO_CASOS[is.na(region_values$NUMERO_CASOS)] <- 0
  
  ## Calcula incidencias
  region_values$NUMERO_CASOS <- as.numeric(region_values$NUMERO_CASOS)
  region_values$POBLACION <- as.numeric(as.character(region_values$POBLACION))
  region_values$incidencia <- region_values$NUMERO_CASOS/region_values$POBLACION
  options(digits = 2)
  region_values$incidencerate_100<-(region_values$incidencia*100000)
  
  ## recode the names of the region_values data frame
  setnames(region_values, old = "REGION",new = "NAME_1")
  levels(region_values$NAME_1)
  
  ## Merge data with shapefile 
  tmp1 <- sh_peru
  tmp1_map <- tmp1
  tmp1_map@data <- join(sh_peru@data,region_values, by="NAME_1") # shp with data
  tmp1_map_df <- tmp1_map@data
  tmp1_map@data$NUMERO_CASOS[is.na(tmp1_map@data$NUMERO_CASOS)]<-0 # check if this is true
  names(tmp1_map@data) # check variables
  tmp1_map_df <- tmp1_map@data
  
  # Create the map qtm
  col <- c("ivory2", "lightcyan1", "lightblue2", #"lightskyblue", 
           "royalblue", "blue2", "navyblue", "black")
  
  # interactive view
  # tmap_mode("view")
  
  # crear mapa
  qtm <- qtm(tmp1_map, fill = "incidencerate_100", 
             fill.palette = col,
             fill.title="Incidencia por \n100,000 Hab") + 
    tm_text("NAME_1",size = 0.45) +
    tm_layout(paste("Casos por Departamento COVID19 ", i), 
              legend.title.size = .8, 
              legend.text.size = .6, legend.bg.color = "white",
              legend.position = c("left","bottom")) #+ 
# legend.postion is used for plot mode. 
# Use view.legend.position in tm_view to set the legend position in view mode.
  
  print(qtm)
  tmap_save(qtm, 
             paste("outputs_covid19/20200327_Dep_Incidencerate_v2_", 
                   fecha.i$Fecha_titulo, ".png", sep=""), 
             width=2000, height=1380)
}  
dev.off()