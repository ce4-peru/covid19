## 11 june 2018 ##
## This script explore the samples analised at the INS-AQP

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

# Import the csv files.
setwd("C:/Users/Micaela/Documents/Rabies")


## FULL DATA ##
data2014_full <- read.csv("Casos_INS/Scripts/data/Muestras_INS_2014.csv")
data2015_full <- read.csv("Casos_INS/Scripts/data/Muestras_INS_2015A.csv")
data2016_full <- read.csv("Casos_INS/Scripts/data/Muestras_INS_2016.csv")
data2017_full <- read.csv("Casos_INS/Scripts/data/Muestras_INS_2017.csv")
data2018_full <- read.csv("Casos_INS/Scripts/data/Muestras_INS_2018.csv")

#data <- read.csv("Casos_INS/Scripts/data/Base_perrospositivos_2015-2018_20Feb18.csv")
# i added new cases. check information.
data <- read.csv("Casos_INS/Scripts/data/Base_perrospositivos_2015-2018_23Mar18.csv")
paper.data <- read.csv("Casos_INS/Scripts/outputs/casos_papertorrenteras_2coord.csv")


##################################### FULL 2014 - 2017 ##########################################

# Check variables
names(data2014_full)
# [1] "CODIGO"       "F.INGRESO"    "DEPARTAMENTO" "PROVINCIA"    "DISTRITO"    
# [6] "LOCALIDAD"    "ESPECIE"      "EESS"         "IFD"          "OBSERVACION" 
dim(data2014_full) # [1] 265  10
colnames(data2014_full)[9] <- "RESULTADO.IFD"
data2014_full$F.OBTENCION <- NA
data2014_full$FECHA.RESULTADO <- NA
data2014_full$RESULTADO.INS.IFD <- NA
data2014_full$INOC.RATONES <- NA
data2014_full$FECHA.RECEP.INS <- NA
data2014_full$FECHA.RESULTADO.INS <- NA
data2014_full <- data2014_full[c(1,2,11,3:9,12,15,13,14,16,10)]
names(data2014_full)
# [1] "CODIGO"              "F.INGRESO"           "F.OBTENCION"        
# [4] "DEPARTAMENTO"        "PROVINCIA"           "DISTRITO"           
# [7] "LOCALIDAD"           "ESPECIE"             "EESS"               
# [10] "RESULTADO.IFD"       "FECHA.RESULTADO"     "FECHA.RECEP.INS"    
# [13] "RESULTADO.INS.IFD"   "INOC.RATONES"        "FECHA.RESULTADO.INS"
# [16] "OBSERVACION"

names(data2015_full)
# [1] "CODIGO"            "F..RECEPCION"      "F..OBTENCION"      "DEPARTAMENTO"     
# [5] "PROVINCIA"         "DISTRITO"          "X"                 "ESPECIE"          
# [9] "EESS"              "IFD"               "FECHA.RESULTADO"   "RESULTADO.INS.IFD"
dim(data2015_full) # [1]   737  12

# Correct 496 DEPARTAMENTO
data2015_full[496,4] <- "AREQUIPA"
data2015_full$DEPARTAMENTO <- factor(data2015_full$DEPARTAMENTO)
table(data2015_full$DEPARTAMENTO)

colnames(data2015_full)[2] <- "F.INGRESO"
colnames(data2015_full)[3] <- "F.OBTENCION"
colnames(data2015_full)[7] <- "LOCALIDAD"
colnames(data2015_full)[10] <- "RESULTADO.IFD"
data2015_full$INOC.RATONES <- NA
data2015_full$FECHA.RECEP.INS <- NA
data2015_full$FECHA.RESULTADO.INS <- NA
data2015_full$OBSERVACION <- NA
data2015_full <- data2015_full[c(1:11,14,12,13,15,16)]
names(data2015_full)
# [1] "CODIGO"              "F.INGRESO"           "F.OBTENCION"        
# [4] "DEPARTAMENTO"        "PROVINCIA"           "DISTRITO"           
# [7] "LOCALIDAD"           "ESPECIE"             "EESS"               
# [10] "RESULTADO.IFD"       "FECHA.RESULTADO"     "FECHA.RECEP.INS"    
# [13] "RESULTADO.INS.IFD"   "INOC.RATONES"        "FECHA.RESULTADO.INS"
# [16] "OBSERVACION" 

names(data2016_full)
# [1] "CODIGO"              "F..RECEPCION"        "F..OBTENCION"        "DEPARTAMENTO"       
# [5] "PROVINCIA"           "DISTRITO"            "LOCALIDAD"           "ESPECIE"            
# [9] "MICRORED"            "IFD"                 "FECHA.RESULTADO"     "RESULTADO.INS.IFD"  
# [13] "INOC"                "FECHA.RECEP.INS"     "FECHA.RESULTADO.INS"
dim(data2016_full) # [1] 582  15
colnames(data2016_full)[2] <- "F.INGRESO"
colnames(data2016_full)[3] <- "F.OBTENCION"
colnames(data2016_full)[9] <- "EESS"
colnames(data2016_full)[10] <- "RESULTADO.IFD"
colnames(data2016_full)[13] <- "INOC.RATONES"
data2016_full$OBSERVACION <- NA
data2016_full <- data2016_full[c(1:11,14,12,13,15,16)]
names(data2016_full)
# [1] "CODIGO"              "F.INGRESO"           "F.OBTENCION"         "DEPARTAMENTO"       
# [5] "PROVINCIA"           "DISTRITO"            "LOCALIDAD"           "ESPECIE"            
# [9] "EESS"                "RESULTADO.IFD"       "FECHA.RESULTADO"     "FECHA.RECEP.INS"    
# [13] "RESULTADO.INS.IFD"   "INOC.RATONES"       "FECHA.RESULTADO.INS" "OBSERVACION" 

names(data2017_full)
# [1] "CODIGO"            "F..RECEPCION"      "F..OBTENCION"      "DEPARTAMENTO"     
# [5] "PROVINCIA"         "DISTRITO"          "LOCALIDAD"         "ESPECIE"          
# [9] "MICRORED"          "IFD"               "FECHA.RESULTADO"   "RESULTADO.INS.IFD"
# [13] "INOC"              "FECHA.RECEP.INS" 
dim(data2017_full) # [1] 413  14
colnames(data2017_full)[2] <- "F.INGRESO"
colnames(data2017_full)[3] <- "F.OBTENCION"
colnames(data2017_full)[9] <- "EESS"
colnames(data2017_full)[10] <- "RESULTADO.IFD"
colnames(data2017_full)[13] <- "INOC.RATONES"
data2017_full$FECHA.RESULTADO.INS <- NA
data2017_full$OBSERVACION <- NA
data2017_full <- data2017_full[c(1:11,14,12,13,15,16)]
names(data2017_full)
# [1] "CODIGO"              "F.INGRESO"           "F.OBTENCION"        
# [4] "DEPARTAMENTO"        "PROVINCIA"           "DISTRITO"           
# [7] "LOCALIDAD"           "ESPECIE"             "EESS"               
# [10] "RESULTADO.IFD"       "FECHA.RESULTADO"     "FECHA.RECEP.INS"    
# [13] "RESULTADO.INS.IFD"   "INOC.RATONES"        "FECHA.RESULTADO.INS"
# [16] "OBSERVACION"  

names(data2018_full)
# [1] "CODIGO"          "F..RECEPCION"    "F..OBTENCION"    "DEPARTAMENTO"   
# [5] "PROVINCIA"       "DISTRITO"        "LOCALIDAD"       "ESPECIE"        
# [9] "MICRORED"        "IFD"             "FECHA.RESULTADO"

dim(data2018_full) # [1] 226  11
colnames(data2018_full)[2] <- "F.INGRESO"
colnames(data2018_full)[3] <- "F.OBTENCION"
colnames(data2018_full)[9] <- "EESS"
colnames(data2018_full)[10] <- "RESULTADO.IFD"
data2018_full$FECHA.RECEP.INS <- NA
data2018_full$RESULTADO.INS.IFD <- NA
data2018_full$INOC.RATONES <- NA
data2018_full$FECHA.RESULTADO.INS <- NA
data2018_full$OBSERVACION <- NA

names(data2018_full)
# [1] "CODIGO"              "F.INGRESO"           "F.OBTENCION"        
# [4] "DEPARTAMENTO"        "PROVINCIA"           "DISTRITO"           
# [7] "LOCALIDAD"           "ESPECIE"             "EESS"               
# [10] "RESULTADO.IFD"       "FECHA.RESULTADO"     "FECHA.RECEP.INS"    
# [13] "RESULTADO.INS.IFD"   "INOC.RATONES"        "FECHA.RESULTADO.INS"
# [16] "OBSERVACION"


####################################################################################

## Merge them ##
data2014_18_full <- rbind(data2014_full, data2015_full, 
                          data2016_full, data2017_full, data2018_full) 
names(data2014_18_full)
# [1] "CODIGO"              "F.INGRESO"           "F.OBTENCION"        
# [4] "DEPARTAMENTO"        "PROVINCIA"           "DISTRITO"           
# [7] "LOCALIDAD"           "ESPECIE"             "EESS"               
# [10] "RESULTADO.IFD"       "FECHA.RESULTADO"     "FECHA.RECEP.INS"    
# [13] "RESULTADO.INS.IFD"   "INOC.RATONES"        "FECHA.RESULTADO.INS"
# [16] "OBSERVACION"

## Check the complete full dataset
dim(data2014_18_full) # 2223   16
head(data2014_18_full)

levels(data2014_18_full$DEPARTAMENTO)
# [1] "AREQUIPA"            "MOQUEGUA"            "                   "
# [4] "AREQUIPA "           "CUZCO"
table(data2014_18_full$DEPARTAMENTO, useNA = "ifany")
# AREQUIPA            MOQUEGUA                     
#  1996                   1                   1 
# AREQUIPA                CUZCO 
#   222                   3

data2014_18_full$DEPARTAMENTO[data2014_18_full$DEPARTAMENTO == "AREQUIPA "] <- "AREQUIPA"
data2014_18_full$DEPARTAMENTO[data2014_18_full$DEPARTAMENTO == "                   "] <- NA
table(data2014_18_full$DEPARTAMENTO)
data2014_18_full$DEPARTAMENTO <- factor(data2014_18_full$DEPARTAMENTO)
table(data2014_18_full$DEPARTAMENTO)
# AREQUIPA MOQUEGUA    CUZCO 
#   2218        1        3

table(data2014_18_full$PROVINCIA)
# AREQUIPA       CAMANA     CARAVELI     CAYLLOMA   CONDESUYOS 
#  1845          166           37          144            7 
# ISLAY     CASTILLA     M. NIETO    AREQUIPA  CHUMBIVILCAS 
#  13            5            1            2            3 

class(data2014_18_full$DISTRITO)
sort(levels(data2014_18_full$DISTRITO), decreasing = F) # 94 niveles
# [1] " MIRAFLORES"                                                                                                                     
# [2] "|LLUTA"                                                                                                                          
# [3] "A. PAUCARPATA"                                                                                                                   
# [4] "A. S. A"                                                                                                                         
# [5] "A.S.A"                                                                                                                           
# [6] "A.S.A."                                                                                                                          
# [7] "ACARI"                                                                                                                           
# [8] "ACHOMA"                                                                                                                          
# [9] "ACHOMA "                                                                                                                         
# [10] "APLAO"                                                                                                                           
# [11] "ARCATA CAYARANI"                                                                                                                 
# [12] "ASA"                                                                                                                             
# [13] "BELLA UNION"                                                                                                                     
# [14] "BUSTAMANTE"                                                                                                                      
# [15] "C. COLORADO"                                                                                                                     
# [16] "CABANACONDE"                                                                                                                     
# [17] "CAHUACHO"                                                                                                                        
# [18] "CALLALI"                                                                                                                         
# [19] "CAMANA"                                                                                                                          
# [20] "CARAVELI"                                                                                                                        
# [21] "CAYARANI"                                                                                                                        
# [22] "CAYLLOMA"                                                                                                                        
# [23] "CAYMA"                                                                                                                           
# [24] "CERCADO"                                                                                                                         
# [25] "CERCADO "                                                                                                                        
# [26] "CHACHAS"                                                                                                                         
# [27] "CHAPACOCO"                                                                                                                       
# [28] "CHARACATO"                                                                                                                       
# [29] "CHIGUATA"                                                                                                                        
# [30] "CHIVAY"                                                                                                                          
# [31] "CHIVAY "                                                                                                                         
# [32] "COCACHACRA"                                                                                                                      
# [33] "COPORAQUE"                                                                                                                       
# [34] "HUNTER"                                                                                                                          
# [35] "ICHUPAMPA"                                                                                                                       
# [36] "IV CENTENARIO"                                                                                                                   
# [37] "J. L. B Y R"                                                                                                                     
# [38] "J.L.B Y R"                                                                                                                       
# [39] "J.L.B. Y R"                                                                                                                      
# [40] "JAQUI"                                                                                                                           
# [41] "LA JOYA"                                                                                                                         
# [42] "LARI"                                                                                                                            
# [43] "LIVITACA"                                                                                                                        
# [44] "LLUTA"                                                                                                                           
# [45] "LOMAS"                                                                                                                           
# [46] "M.  N. VARCARCEL"                                                                                                                
# [47] "M. CACERES"                                                                                                                      
# [48] "M. MELGAR"                                                                                                                       
# [49] "M. N. VALCARCEL"                                                                                                                 
# [50] "M. NICOLAS VALCARCEL"                                                                                                            
# [51] "MACA"                                                                                                                            
# [52] "MACA "                                                                                                                           
# [53] "MADRIGAL"                                                                                                                        
# [54] "MAJES"                                                                                                                           
# [55] "MATARANI"                                                                                                                        
# [56] "MIRAFLORES"                                                                                                                      
# [57] "MOLLEBAYA"                                                                                                                       
# [58] "MOLLEBAYA "                                                                                                                      
# [59] "MOLLENDO"                                                                                                                        
# [60] "N. DE PIEROLA"                                                                                                                   
# [61] "N.P"                                                                                                                             
# [62] "OCOÑA"                                                                                                                           
# [63] "PAUCARPATA"                                                                                                                      
# [64] "PAUCARPATA                                                                                                                      "
# [65] "POCSI"                                                                                                                           
# [66] "POLOBAYA"                                                                                                                        
# [67] "PUNTA DE BOMBON"                                                                                                                 
# [68] "QUEQUEÑA"                                                                                                                        
# [69] "QUILCA"                                                                                                                          
# [70] "RIO GRANDE"                                                                                                                      
# [71] "S. ISABEL DE SIGUAS"                                                                                                             
# [72] "S. JUA DE SIGUAS"                                                                                                                
# [73] "S. JUAN DE SIGUAS"                                                                                                               
# [74] "S. JUAN DE SIGUAS "                                                                                                              
# [75] "S. PASTOR"                                                                                                                       
# [76] "S. PASTOR LA PAMPA"                                                                                                              
# [77] "SABANDIA"                                                                                                                        
# [78] "SACHACA"                                                                                                                         
# [79] "SAMEGUA"                                                                                                                         
# [80] "SAMUEL PASTOR"                                                                                                                   
# [81] "SAN GREGORIO"                                                                                                                    
# [82] "SAN JOSE"                                                                                                                        
# [83] "SAN JUAN DE SIGUAS"                                                                                                              
# [84] "SANTA RITA DE SIGUAS"                                                                                                            
# [85] "SIBAYO"                                                                                                                          
# [86] "SOCABAYA"                                                                                                                        
# [87] "STA ISABEL"                                                                                                                      
# [88] "STA ISABEL DE S."                                                                                                                
# [89] "TIABAYA"                                                                                                                         
# [90] "TISCO "                                                                                                                          
# [91] "TUTI"                                                                                                                            
# [92] "UCHUMAYO"                                                                                                                        
# [93] "URACA"                                                                                                                           
# [94] "URASQUI"                                                                                                                         
# [95] "VITOR"                                                                                                                           
# [96] "YANAHUARA"                                                                                                                       
# [97] "YANQUE"                                                                                                                          
# [98] "YARABAMBA"                                                                                                                       
# [99] "YAUCA"                                                                                                                           
# [100] "YURA"                                                                                                                            
# [101] "YURA "

data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == " MIRAFLORES"] <- "MIRAFLORES"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "|LLUTA"] <- "LLUTA"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "ACHOMA " ] <- "ACHOMA" 
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "A.S.A"] <- "A.S.A."
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "A. S. A"] <- "A.S.A."
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "ASA"] <- "A.S.A."
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "CERCADO "] <- "CERCADO"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "CHIVAY "] <- "CHIVAY" 
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "J.L.B Y R"] <- "J.L.B. Y R" 
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "J. L. B Y R"] <- "J.L.B. Y R" 
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "MACA "] <- "MACA" 
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "M. NICOLAS VALCARCEL"] <- "M. N. VALCARCEL" 
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "M.  N. VARCARCEL"] <- "M. N. VALCARCEL" 
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "MOLLEBAYA "] <- "MOLLEBAYA"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "N.P"] <- "N. DE PIEROLA"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "PAUCARPATA                                                                                                                      "] <- "PAUCARPATA"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "PUNTA DE B"] <- "PUNTA DE BOMBON"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "S. JUA DE SIGUAS"] <- "S. JUAN DE SIGUAS"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "SAN JUAN DE SIGUAS"] <- "S. JUAN DE SIGUAS"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "S. JUAN DE SIGUAS "] <- "S. JUAN DE SIGUAS"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "STA ISABEL"] <- "S. ISABEL DE SIGUAS"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "STA ISABEL DE S."] <- "S. ISABEL DE SIGUAS"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "S. PASTOR"] <- "S. PASTOR LA PAMPA"
data2014_18_full$DISTRITO[data2014_18_full$DISTRITO == "YURA "] <- "YURA"
data2014_18_full$DISTRITO[is.na(data2014_18_full$DISTRITO)] <- factor("LLUTA")
sum(is.na(data2014_18_full$DISTRITO)) # 0

# Check levels of districts again
data2014_18_full$DISTRITO <- factor(data2014_18_full$DISTRITO)
sort(levels(data2014_18_full$DISTRITO), decreasing = F) # 78 niveles

# Especies
table(data2014_18_full$ESPECIE, useNA = "ifany") # 
# CAN         GATO    MONO       NO PUSO   MURCIELAGO 
# 2020        195          1          4          3 
data2014_18_full$ESPECIE[data2014_18_full$ESPECIE == "NO PUSO"] <- NA
data2014_18_full$ESPECIE <- factor(data2014_18_full$ESPECIE)
sort(levels(data2014_18_full$ESPECIE), decreasing = F) # "CAN"  "GATO" "MONO" "MURCIELAGO"
sum(is.na(data2014_18_full$ESPECIE)) # 4 NAs

# RESULTADO.IFD
table(data2014_18_full$RESULTADO.IFD)
#     NEGATIVO POSITIVO 
# 37     2043      143 
levels(data2014_18_full$RESULTADO.IFD) # ""         "NEGATIVO" "POSITIVO"
data2014_18_full$RESULTADO.IFD[data2014_18_full$RESULTADO.IFD == ""] <- NA
data2014_18_full$RESULTADO.IFD <- factor(data2014_18_full$RESULTADO.IFD)
table(data2014_18_full$RESULTADO.IFD) # ok

# RESULTADO.INS.IFD
table(data2014_18_full$RESULTADO.INS.IFD)
#          NEG NEGATIVO      POS 
# 10       1245      358      119 
unique(data2014_18_full$RESULTADO.INS.IFD) 
# NA         "NEG"      "POS"      "NEGATIVO" ""
data2014_18_full$RESULTADO.INS.IFD[data2014_18_full$RESULTADO.INS.IFD == ""] <- NA
data2014_18_full$RESULTADO.INS.IFD[data2014_18_full$RESULTADO.INS.IFD == "NEG"] <- "NEGATIVO"
data2014_18_full$RESULTADO.INS.IFD[data2014_18_full$RESULTADO.INS.IFD == "POS"] <- "POSITIVO"
table(data2014_18_full$RESULTADO.INS.IFD)
# NEGATIVO POSITIVO 
#  1603      119 

# INOC.RATONES
table(data2014_18_full$INOC.RATONES)
#         POS 
# 947   1  47
unique(data2014_18_full$INOC.RATONES) 
# NA    ""    "POS" "  " 
data2014_18_full$INOC.RATONES[data2014_18_full$INOC.RATONES == ""] <- NA
data2014_18_full$INOC.RATONES[data2014_18_full$INOC.RATONES == "  "] <- NA
data2014_18_full$INOC.RATONES[data2014_18_full$INOC.RATONES == "POS"] <- "POSITIVO"
table(data2014_18_full$INOC.RATONES)
# POSITIVO 
# 47

# OBSERVACION
unique(data2014_18_full$OBSERVACION)
# [1] NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         

data2014_18_full$OBSERVACION[data2014_18_full$OBSERVACION == ""] <- NA
data2014_18_full$OBSERVACION[data2014_18_full$OBSERVACION == "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         "] <- NA
data2014_18_full$OBSERVACION <- factor(data2014_18_full$OBSERVACION)
levels(data2014_18_full$OBSERVACION)
# [1] "CLINICA P."            "GERSA"                 "MUEST DE OTRO DISRITO" "PORONGOCHE"           
# [5] "POSITIVO INS"          "SAN GREGORIO"
# to check later with previous dataset.

# TO CLEAN LATER
table(data2014_18_full$LOCALIDAD) # 94 niveles
table(data2014_18_full$EESS) # 94 niveles
# dates 
# check later

##### CREATE A FINAL POSITIVE COLUMN ######
data2014_18_full$RESULTADO_F [data2014_18_full$RESULTADO.IFD == "POSITIVO"|
                                data2014_18_full$RESULTADO.INS.IFD == "POSITIVO"|
                                data2014_18_full$INOC.RATONES == "POSITIVO"|
                                data2014_18_full$OBSERVACION == "POSITIVO INS"] <- "POSITIVO"

# Complete negative observations
table(data2014_18_full$RESULTADO_F, useNA = "ifany")
# POSITIVO     <NA> 
#   149     2074 
data2014_18_full$RESULTADO_F[is.na(data2014_18_full$RESULTADO_F)] <- "NEGATIVO"
table(data2014_18_full$RESULTADO_F, useNA = "ifany")
# NEGATIVO POSITIVO 
#  2074      149 


## Create variables ##
# Create year and month variables
# unique(data2014_17_full$F.INGRESO)
data2014_18_full[1394,2] # 26|/07/2016
data2014_18_full[1394,2] <- "26/07/2016" # replace for info in column 3: F.INGRESO

data2014_18_full[1395,2] # 26|/07/2016
data2014_18_full[1395,2] <- "26/07/2016" # replace for info in column 3: F.INGRESO

data2014_18_full[1396,3] # 26|/07/2016
data2014_18_full[1396,3] <- "26/07/2016" # replace for info in column 3: F.INGRESO

data2014_18_full[1397,3] # 26|/07/2016
data2014_18_full[1397,3] <- "26/07/2016" # replace for info in column 3: F.INGRESO

data2014_18_full[1421,2] # 26/08/2z016
data2014_18_full[1421,2] <- "26/08/2016" # replace for info in column 3: F.INGRESO

data2014_18_full$year <- substr(data2014_18_full$F.INGRESO, 7, 10)
data2014_18_full$month <- substr(data2014_18_full$F.INGRESO, 4, 5)
class(data2014_18_full$year) # "character"
class(data2014_18_full$month) # "character"

# Create numeric variables
data2014_18_full$nyear <- as.numeric(data2014_18_full$year)
sum(is.na(data2014_18_full$nyear)) # 0
data2014_18_full$nmonth <- as.numeric(data2014_18_full$month)
sum(is.na(data2014_18_full$nmonth)) # 0

# Create monthly Timesteps
data2014_18_full$nmonth_s <- (12*(data2014_18_full$nyear-2014)) + data2014_18_full$nmonth

# Create quarters
data2014_18_full$quarter<- data2014_18_full$nmonth
class(data2014_18_full$quarter) # [1] "numeric"
data2014_18_full$quarter[data2014_18_full$quarter == 2] <- 1
data2014_18_full$quarter[data2014_18_full$quarter == 3] <- 1
data2014_18_full$quarter[data2014_18_full$quarter == 4] <- 2
data2014_18_full$quarter[data2014_18_full$quarter == 5] <- 2
data2014_18_full$quarter[data2014_18_full$quarter == 6] <- 2
data2014_18_full$quarter[data2014_18_full$quarter == 7] <- 3
data2014_18_full$quarter[data2014_18_full$quarter == 8] <- 3
data2014_18_full$quarter[data2014_18_full$quarter == 9] <- 3
data2014_18_full$quarter[data2014_18_full$quarter == 10] <- 4
data2014_18_full$quarter[data2014_18_full$quarter == 11] <- 4
data2014_18_full$quarter[data2014_18_full$quarter == 12] <- 4

# Create quarterly timesteps
data2014_18_full$quarter_s <- (4*(data2014_18_full$nyear-2014)) + data2014_18_full$quarter

# Quadrimester
data2014_18_full$term <- data2014_18_full$nmonth
data2014_18_full$term[data2014_18_full$term == 1] <- "Dic-Mar"
data2014_18_full$term[data2014_18_full$term == 2] <- "Dic-Mar"
data2014_18_full$term[data2014_18_full$term == 3] <- "Dic-Mar"
data2014_18_full$term[data2014_18_full$term == 4] <- "Abr-Jul"
data2014_18_full$term[data2014_18_full$term == 5] <- "Abr-Jul"
data2014_18_full$term[data2014_18_full$term == 6] <- "Abr-Jul"
data2014_18_full$term[data2014_18_full$term == 7] <- "Abr-Jul"
data2014_18_full$term[data2014_18_full$term == 8] <- "Ago-Nov"
data2014_18_full$term[data2014_18_full$term == 9] <- "Ago-Nov"
data2014_18_full$term[data2014_18_full$term == 10] <-"Ago-Nov"
data2014_18_full$term[data2014_18_full$term == 11] <-"Ago-Nov"
data2014_18_full$term[data2014_18_full$term == 12] <-"Dic-Mar"

# Season
data2014_18_full$season <- data2014_18_full$nmonth
data2014_18_full$season[data2014_18_full$season == 1] <- "Verano" # 
data2014_18_full$season[data2014_18_full$season == 2] <- "Verano"
data2014_18_full$season[data2014_18_full$season == 3] <- "Otoño"
data2014_18_full$season[data2014_18_full$season == 4] <- "Otoño"
data2014_18_full$season[data2014_18_full$season == 5] <- "Otoño"
data2014_18_full$season[data2014_18_full$season == 6] <- "Invierno"
data2014_18_full$season[data2014_18_full$season == 7] <- "Invierno"
data2014_18_full$season[data2014_18_full$season == 8] <- "Invierno"
data2014_18_full$season[data2014_18_full$season == 9] <- "Primavera"
data2014_18_full$season[data2014_18_full$season == 10] <-"Primavera"
data2014_18_full$season[data2014_18_full$season == 11] <-"Primavera"
data2014_18_full$season[data2014_18_full$season == 12] <-"Verano"

# season2
data2014_18_full$season2 <- data2014_18_full$nmonth
data2014_18_full$season2[data2014_18_full$season2 == 1] <- "Lluvias" # 
data2014_18_full$season2[data2014_18_full$season2 == 2] <- "Lluvias"
data2014_18_full$season2[data2014_18_full$season2 == 3] <- "Lluvias"
data2014_18_full$season2[data2014_18_full$season2 == 4] <- "Seca"
data2014_18_full$season2[data2014_18_full$season2 == 5] <- "Seca"
data2014_18_full$season2[data2014_18_full$season2 == 6] <- "Seca"
data2014_18_full$season2[data2014_18_full$season2 == 7] <- "Seca"
data2014_18_full$season2[data2014_18_full$season2 == 8] <- "Seca"
data2014_18_full$season2[data2014_18_full$season2 == 9] <- "Seca"
data2014_18_full$season2[data2014_18_full$season2 == 10] <-"Lluvias"
data2014_18_full$season2[data2014_18_full$season2 == 11] <-"Lluvias"
data2014_18_full$season2[data2014_18_full$season2 == 12] <-"Lluvias"


##################################### POSITIVE DATA 2015 - 2018  ##########################################
names(data2014_18_full)
# [1] "CODIGO"              "F.INGRESO"           "F.OBTENCION"        
# [4] "DEPARTAMENTO"        "PROVINCIA"           "DISTRITO"           
# [7] "LOCALIDAD"           "ESPECIE"             "EESS"               
# [10] "RESULTADO.IFD"       "FECHA.RESULTADO"     "FECHA.RECEP.INS"    
# [13] "RESULTADO.INS.IFD"   "INOC.RATONES"        "FECHA.RESULTADO.INS"
# [16] "OBSERVACION"         "RESULTADO_F"         "year"               
# [19] "month"               "nyear"               "nmonth"             
# [22] "nmonth_s"            "quarter"             "quarter_s"          
# [25] "term"                "season"              "season2"

names(data)
# [1] "CODIGO"          "F..RECEPCION"    "F..OBTENCION"    "F..MUERTE"      
# [5] "DEPARTAMENTO"    "PROVINCIA"       "DISTRITO"        "LOCALIDAD"      
# [9] "ESPECIE"         "EESS"            "IFD"             "FECHA.RESULTADO"
# [13] "RESULTADO.INS"   "INOC.RATONES"   

head(data)
str(data) # 140 obs. of  14 variables:

# Rename variables
colnames(data)[2] <- "F.INGRESO"
colnames(data)[3] <- "F.OBTENCION"
colnames(data)[4] <- "F.MUERTE"
colnames(data)[11] <- "RESULTADO.IFD"
colnames(data)[13] <- "RESULTADO.INS.IFD"
colnames(data)[14] <- "INOC.RATONES"
names(data)
# [1] "CODIGO"            "F.INGRESO"         "F.OBTENCION"       "F.MUERTE"         
# [5] "DEPARTAMENTO"      "PROVINCIA"         "DISTRITO"          "LOCALIDAD"        
# [9] "ESPECIE"           "EESS"              "RESULTADO.IFD"     "FECHA.RESULTADO"  
# [13] "RESULTADO.INS.IFD" "INOC.RATONES"

# Create year and month variables
# solve an issue
data[25,2] # 17/01/1900
data[25,2] <- "17/02/2016" # replace for info in column 3: F..OBTENCION

data$year <- substr(data$F.INGRESO, 7, 10)
data$month <- substr(data$F.INGRESO, 4, 5)
class(data$year)
class(data$month)

# Create numeric variables
data$nyear <- as.numeric(data$year)
data$nmonth <- as.numeric(data$month)

# Create monthly Timesteps
data$nmonth_s <- (12*(data$nyear-2014)) + data$nmonth

# Create quarters
data$quarter<- data$nmonth
class(data$quarter) # [1] "numeric"
data$quarter[data$quarter == 2] <- 1
data$quarter[data$quarter == 3] <- 1
data$quarter[data$quarter == 4] <- 2
data$quarter[data$quarter == 5] <- 2
data$quarter[data$quarter == 6] <- 2
data$quarter[data$quarter == 7] <- 3
data$quarter[data$quarter == 8] <- 3
data$quarter[data$quarter == 9] <- 3
data$quarter[data$quarter == 10] <- 4
data$quarter[data$quarter == 11] <- 4
data$quarter[data$quarter == 12] <- 4

# Create quarterly timesteps
data$quarter_s <- (4*(data$nyear-2014)) + data$quarter


# Quadrimester
data$term <- data$nmonth
data$term[data$term == 1] <- "Dic-Mar"
data$term[data$term == 2] <- "Dic-Mar"
data$term[data$term == 3] <- "Dic-Mar"
data$term[data$term == 4] <- "Abr-Jul"
data$term[data$term == 5] <- "Abr-Jul"
data$term[data$term == 6] <- "Abr-Jul"
data$term[data$term == 7] <- "Abr-Jul"
data$term[data$term == 8] <- "Ago-Nov"
data$term[data$term == 9] <- "Ago-Nov"
data$term[data$term == 10] <-"Ago-Nov"
data$term[data$term == 11] <-"Ago-Nov"
data$term[data$term == 12] <-"Dic-Mar"

# Season
data$season <- data$nmonth
data$season[data$season == 1] <- "Verano" # 
data$season[data$season == 2] <- "Verano"
data$season[data$season == 3] <- "Otoño"
data$season[data$season == 4] <- "Otoño"
data$season[data$season == 5] <- "Otoño"
data$season[data$season == 6] <- "Invierno"
data$season[data$season == 7] <- "Invierno"
data$season[data$season == 8] <- "Invierno"
data$season[data$season == 9] <- "Primavera"
data$season[data$season == 10] <-"Primavera"
data$season[data$season == 11] <-"Primavera"
data$season[data$season == 12] <-"Verano"

# season2
data$season2 <- data$nmonth
data$season2[data$season2 == 1] <- "Lluvias" # 
data$season2[data$season2 == 2] <- "Lluvias"
data$season2[data$season2 == 3] <- "Lluvias"
data$season2[data$season2 == 4] <- "Seca"
data$season2[data$season2 == 5] <- "Seca"
data$season2[data$season2 == 6] <- "Seca"
data$season2[data$season2 == 7] <- "Seca"
data$season2[data$season2 == 8] <- "Seca"
data$season2[data$season2 == 9] <- "Seca"
data$season2[data$season2 == 10] <-"Lluvias"
data$season2[data$season2 == 11] <-"Lluvias"
data$season2[data$season2 == 12] <-"Lluvias"


### CHECK DISTRITO ###
table(data$DISTRITO) # some repeated
levels(data$DISTRITO)
# [1] " LOS GIRASOLES Q13 ZNAA"    "A.S.A"                      "A.S.A."                    
# [4] "C. COLORADO"                "CAYMA"                      "CERCADO"                   
# [7] "HUNTER"                     "J. L. B Y R"                "M. MELGAR"                 
# [10] "MIRAFLORES"                 "PAUCARPATA"                 "SACHACA"                   
# [13] "SOCABAYA"                   "UCHUMAYO"                   "URB C. DE JESUS H9SOCABAYA"
# [16] "YURA"                       "YURA "                

# Obtains rows of  1 and 15. 1 should be plot and 15 is SOCABAYA
names(data)
issue1 <- data[data$DISTRITO == " LOS GIRASOLES Q13 ZNAA",]
issue2 <- data[data$DISTRITO == "URB C. DE JESUS H9SOCABAYA",]

# Change issue 1
data[8,7] <- "SOCABAYA"

# Change issue 2
issue2.xy <- paper.data[7,]
# find out district
map <- get_map(location = c(lon = -71.5, lat = -16.375),     
               zoom = 14,                # 5 to set a higher scale.
               maptype = "roadmap",     # setting easy to visualise
               source = "google")       # the source to load the map from
ggmap(map) + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = issue2.xy, # Add points for sites using latitude and longitude.
             aes(x = long, y = lat), # Set longitudes and latitudes on axes.
             color = "red", size = 5)
# It belongs to Miraflores
data[7,7] <- "MIRAFLORES"
data$DISTRITO <- factor(data$DISTRITO)

# Replace when necessary
levels(data$DISTRITO)
# [1] "A.S.A"       "A.S.A."      "C. COLORADO" "CAYMA"       "CERCADO"     "HUNTER"     
# [7] "J. L. B Y R" "M. MELGAR"   "MIRAFLORES"  "PAUCARPATA"  "SACHACA"     "SOCABAYA"   
# [13] "UCHUMAYO"    "YURA"        "YURA " 

# change
data$DISTRITO[data$DISTRITO == "A.S.A"] <- "A.S.A."
data$DISTRITO[data$DISTRITO == "YURA "] <- "YURA"
data$DISTRITO <- factor(data$DISTRITO)
levels(data$DISTRITO)
# [1] "A.S.A."      "C. COLORADO" "CAYMA"       "CERCADO"     "HUNTER"     
# [6] "J. L. B Y R" "M. MELGAR"   "MIRAFLORES"  "PAUCARPATA"  "SACHACA"    
# [11] "SOCABAYA"    "UCHUMAYO"    "YURA"

# POSITIVITY - IFD AQP
table(data$RESULTADO.IFD)
#      NEGATIVO POSITIVO 
# 19        2      119 
levels(data$RESULTADO.IFD) # [1] ""         "NEGATIVO" "POSITIVO"
data$RESULTADO.IFD[data$RESULTADO.IFD == ""] <- NA
data$RESULTADO.IFD <- factor(data$RESULTADO.IFD)
levels(data$RESULTADO.IFD) # [1] "NEGATIVO" "POSITIVO"

# POSITIVITY - IFD INS
table(data$RESULTADO.INS.IFD)
      #     NEGATIVO      POS 
      # 83        2       55
levels(data$RESULTADO.INS.IFD) # [1] ""         "NEGATIVO" "POS"  
data$RESULTADO.INS.IFD[data$RESULTADO.INS.IFD == ""] <- NA
#data$RESULTADO.INS.IFD[data$RESULTADO.INS.IFD == "POS"] <- "POSITIVO"
data$RESULTADO.INS.IFD <- factor(data$RESULTADO.INS.IFD)
levels(data$RESULTADO.INS.IFD) # [1] "NEGATIVO" "POS"

# INOC. RATONES
table(data$INOC.RATONES)
#             POS       POS INS POS INS (PCR) 
#92            46             1             1
levels(data$INOC.RATONES) 
# [1] ""              "POS"           "POS INS"       "POS INS (PCR)" 
data$INOC.RATONES[data$INOC.RATONES == ""] <- NA
data$INOC.RATONES <- factor(data$INOC.RATONES)
levels(data$INOC.RATONES) # [1] "POS"           "POS INS"       "POS INS (PCR)"
data$INOC.RATONES[data$INOC.RATONES == "POS INS"] <- "POS"
data$INOC.RATONES[data$INOC.RATONES == "POS INS (PCR)"] <- "POS"

##### CREATE A FINAL POSITIVE COLUMN ######
data$RESULTADO_F [data$RESULTADO.IFD == "POSITIVO"|
                                data$RESULTADO.INS.IFD == "POS"|
                                data$INOC.RATONES == "POS"] <- "POSITIVO"
data[136:140, 25] <- "POSITIVO"
data[1:19, 25] <- "POSITIVO"
###########################################################################
# CHECK POSITIVES. IF THERE IS CONGRUENCE.
unique(data$RESULTADO_F)
table(data$RESULTADO_F)
sum(is.na(data$RESULTADO_F)) # 19

table(data$year)
# 2015 2016 2017 2018 
# 19   59   48   14

table(data2014_18_full$year, data2014_18_full$RESULTADO_F)

data2016_1 <- data2014_18_full[(data2014_18_full$nyear == 2016),]
data2016_2 <- data[data$nyear == 2016,]
# 2015 UNTIL 6aUG18