###### limpieza casos por indartamento #####

library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
## set directory
setwd("~/covid19")

## Call data
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200326_MD.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200327_MD.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200331_MD.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200401_MD.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200402_MD.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200403_MD.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200404_MD.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200405_CA.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200406_MD.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200407_CA.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200408_MD.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200409_CA.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200410_MD.csv")
# covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200411_CA.csv")
#covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200412_MD.csv")

## Checar base
str(covid_ind) # 6838 obs. of  20 variables
names(covid_ind)
# [1] "CASO_ID"              "FECHA"                "REGION"              
# [4] "PROVINCIA"            "DISTRITO"             "DIRECCION"           
# [7] "EDAD_A"               "SEXOM1H0"             "HOSPITALIZADOS1N0"   
# [10] "AISLADODOMICILS1N0"   "CONTACTO"             "IMPORTADO"           
# [13] "ORIGEN_INTERNACIONAL" "CASO_CONTACTO"        "RELACION_CONTACTO"   
# [16] "ORIGEN_NACIONAL"      "ENLACES1"             "ENLACES2"            
# [19] "ENLACES3"             "OBSERVACIONES"  

# Mas adelante  Sacar los "S1N0 del nombre de la variable

head(covid_ind)
# Transformar todo texto a mayusculas

# Transform to character
covid_ind <- mutate_if(covid_ind, is.character, toupper)
sapply(covid_ind, class)
# CASO_ID                FECHA               REGION            PROVINCIA 
# "integer"          "character"          "character"          "character" 
# DISTRITO            DIRECCION               EDAD_A             SEXOM1H0 
# "character"            "logical"          "character"            "integer" 
# HOSPITALIZADOS1N0   AISLADODOMICILS1N0             CONTACTO            IMPORTADO 
# "integer"            "integer"          "character"            "integer" 
# ORIGEN_INTERNACIONAL        CASO_CONTACTO    RELACION_CONTACTO      ORIGEN_NACIONAL 
# "character"            "integer"          "character"          "character" 
# ENLACES1             ENLACES2             ENLACES3        OBSERVACIONES 
# "character"          "character"          "character"          "character"


### variableby variable
### Caso_ID ###
table(covid_ind$CASO_ID)
# 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22 
# 1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
# 23  24  25  26  27  28  44  69  71  73 106 145 
# 1   1   1   1   1   1   1   1   1   1   1   1

### Fecha ####
table(covid_ind$FECHA)
# 01/04/2020 02/04/2020 03/04/2020 04/04/2020 05/04/2020 06/03/2020 06/04/2020 
# 258         91        183        152        525          1        280 
# 07/03/2020 07/04/2020 08/04/2020 09/03/2020 09/04/2020 10/03/2020 10/04/2020 
# 5        393       1387          3        914          2        641 
# 11/03/2020 11/04/2020 12/03/2020 13/03/2020 14/03/2020 15/03/2020 16/03/2020 
# 6        951          5         16          5         28         15 
# 17/03/2020 18/03/2020 19/03/2020 20/03/2020 21/03/2020 22/03/2020 23/03/2020 
# 31         29         89         29         55         43         32 
# 24/03/2020 25/03/2020 26/03/2020 27/03/2020 28/03/2020 29/03/2020 30/03/2020 
# 21         64         99         55         36        176        103 
# 31/03/2020 
# 115

#### separando caracteres en varias columnas ######
covid_ind$FECHA2 <- covid_ind$FECHA
covid_ind <- separate(covid_ind,
         col = "FECHA2",
         into = c("DIA", "MES","ANO"),
         sep = "/")

##### fecha to as.date format ####
covid_ind<-covid_ind %>%
  mutate(FECHA = as.Date(as.character(FECHA), format = "%d/%m/%y"))

### ordenar base ###
names(covid_ind)
# [1] "CASO_ID"              "FECHA"                "REGION"              
# [4] "PROVINCIA"            "DISTRITO"             "DIRECCION"           
# [7] "EDAD_A"               "SEXOM1H0"             "HOSPITALIZADOS1N0"   
# [10] "AISLADODOMICILS1N0"   "CONTACTO"             "IMPORTADO"           
# [13] "ORIGEN_INTERNACIONAL" "CASO_CONTACTO"        "RELACION_CONTACTO"   
# [16] "ORIGEN_NACIONAL"      "ENLACES1"             "ENLACES2"            
# [19] "ENLACES3"             "OBSERVACIONES"        "DIA"                 
# [22] "MES"                  "ANO"  

covid_ind <- covid_ind[,c(1, 2, 21:23, 3:20)]


### Check NAs and inconsistencies ###

# ID del caso
str(covid_ind$CASO_ID) # int
unique(covid_ind$CASO_ID)
### Para manejo nuestro, completar n[umero de casos en google spreadsheet.

## Fecha 
str(covid_ind$FECHA) # date
unique(covid_ind$FECHA)
# ok

## Region
str(covid_ind$REGION) # character
# Cambiar nombre de variable en googlesheet
unique(covid_ind$REGION)
# [1] "LIMA"          "AREQUIPA"      "HUáNUCO"      "ICA"          
# [5] "CUSCO"         "ANCASH"        "CALLAO"        "LA LIBERTAD"  
# [9] "LAMBAYEQUE"    "PIURA"         "LORETO"        "MADRE DE DIOS"
# [13] "SAN MARTIN"    "JUNIN"         "TUMBES"        "CAJAMARCA"    
# [17] "HUANUCO"       "PASCO"         "AYACUCHO"      "TACNA"        
# [21] "HUANCAVELICA"  "APURIMAC"      "MOQUEGUA"      "PUNO"         
# [25] "AMAZONAS"      "UCAYALI"  
# # "Huánuco" con tilde

# Cambiar Huanuco
levels(covid_ind$REGION) <- c(levels(covid_ind$REGION), 
                            c("APURIMAC", "HUANUCO", 
                              "JUNIN", "SAN MARTIN"))

covid_ind$REGION[covid_ind$REGION == "HUáNUCO"] <- "HUANUCO"
covid_ind$REGION[covid_ind$REGION == "HUÁNUCO"] <- "HUANUCO"
covid_ind$REGION[covid_ind$REGION == ""] <- NA
unique(covid_ind$REGION) # ok
# [1] "LIMA"          "AREQUIPA"      "HUANUCO"       "ICA"          
# [5] "CUSCO"         "ANCASH"        "CALLAO"        "LA LIBERTAD"  
# [9] "LAMBAYEQUE"    "PIURA"         "LORETO"        "MADRE DE DIOS"
# [13] "SAN MARTIN"    "JUNIN"         "TUMBES"        "CAJAMARCA"    
# [17] "PASCO"         "AYACUCHO"      "TACNA"         "HUANCAVELICA" 
# [21] "APURIMAC"      "MOQUEGUA"      "PUNO"          "AMAZONAS"     
# [25] "UCAYALI

## Provincia
str(covid_ind$PROVINCIA) # chr 
unique(covid_ind$PROVINCIA)
# [1] "LIMA"                   "AREQUIPA"               "HUANUCO"                "CHINCHA"               
# [5] "CUSCO"                  ""                       "SANTA"                  "CALLAO"                
# [9] "TRUJILLO"               "CHICLAYO"               "PIURA"                  "MAYNAS"                
# [13] "TAMBOPATA"              "SAN MARTIN"             "HUANCAYO"               "FERRENAFE"             
# [17] "SULLANA"                "ICA"                    "TUMBES"                 "CAJAMARCA"             
# [21] "PAITA"                  "DANIEL ALCIDES CARRION" "ZARUMILLA"              "MARISCAL CACERES"      
# [25] "HUAMANGA"               "CONVENCION"             "FERREÑAFE"              "TACNA"                 
# [29] "MOYOBAMBA"              "RIOJA"                  "LEONCIO PRADO"          "LAMBAYEQUE"            
# [33] "HUARAZ"                 "JAEN"                   "ACOBAMABA"              "ASCOPE"                
# [37] "SECHURA"                "CONTRALMIRANTE VILLAR"  "LORETO"                 "CAYLLOMA"              
# [41] "ANDAHUAYLAS"            "OXAPAMPA"               "PARINACOCHAS"           "GENERAL SANCHEZ CERRO" 
# [45] "PISCO"                  "CHANCHAMAYO"            "PACASMAYO"              "PASCO"                 
# [49] "HUARI"                  "DATEM"                  "UCAYALI"                "REQUENA"               
# [53] "HUAYLAS"                "BAGUA"                  "CHACHAPOYAS"            "BONGARA"               
# [57] "HUARMEY"                "CAMANA"                 "CORORNEL PORTILLO"      "MARISCAL NIETO"        
# [61] "ILO"                    "HUANCABAMBA"            "MORROPON" 
# Hay  ""
#  "FERREñAFE" con n.

levels(covid_ind$PROVINCIA) <- c(levels(covid_ind$PROVINCIA), 
                              c("APURIMAC", "HUANUCO", 
                                "JUNIN", "SAN MARTIN", "FERRENAFE"))

covid_ind$PROVINCIA[covid_ind$PROVINCIA == "FERREñAFE"] <- "FERRENAFE"
covid_ind$PROVINCIA[covid_ind$PROVINCIA == ""] <- NA
unique(covid_ind$PROVINCIA)

## Distrito
str(covid_ind$DISTRITO) # chr
unique(covid_ind$DISTRITO)
# [1] "SURCO"                         "CERRO COLORADO"               
# [3] "LINCE"                         ""                             
# [5] "AMARILIS"                      "CHINCHA BAJA"                 
# [7] "VILLA MARIA DEL TRIUNFO"       "CUSCO"                        
# [9] "NUEVO CHIMBOTE"                "LA PUNTA"                     
# [11] "HUANCHACO"                     "LA VICTORIA"                  
# [13] "CASTILLA"                      "LAS LOMAS"                    
# [15] "IQUITOS"                       "TRUJILLO"                     
# [17] "INDIANA"                       "TAMBOPATA"                    
# [19] "LA BANDA DE SHILCAYO"          "SANTA"                        
# [21] "JOSE LUIS BUSTAMANTE Y RIBERO" "TAMBO"                        
# [23] "HUANCAYO"                      "YANAHUARA"                    
# [25] "CHILCA"                        "MARCAVELICA"                  
# [27] "VEINTISEIS DE OCTUBRE"         "PIURA"                        
# [29] "MARIANO MELGAR"                "CARMEN DE LA LEGUA"           
# [31] "ICA"                           "LAREDO"                       
# [33] "ANDRES ARUAJO MORAN"           "CERCADO DE TUMBES"            
# [35] "CAJAMARCA"                     "ATE VITARTE"                  
# [37] "TAMBO REAL"                    "CHINCHA ALTA"                 
# [39] "EL PORVENIR"                   "PAITA"                        
# [41] "HUANUCO"                       "FERRENAFE"                    
# [43] "TAPUC"                         "AGUAS VERDES"                 
# [45] "PUEBLO NUEVO"                  "JUANJUI"                      
# [47] "CARMEN ALTO"                   "SANTA ANA"                    
# [49] "LA ESPERANZA"                  "CHICLAYO"                     
# [51] "JOSE LEONARDO ORTIZ"           "SANTA ROSA"                   
# [53] "TARAPOTO"                      "GREGORIO ALBARRACIN"          
# [55] "ZARUMILLA"                     "MOYOBAMBA"                    
# [57] "NUEVA CAJAMARCA"               "RUPA RUPA"                    
# [59] "VICTOR LARCO"                  "FERREñAFE"                   
# [61] "REQUE"                         "LAMBAYEQUE"                   
# [63] "CIUDAD ETEN"                   "TACNA"                        
# [65] "INDEPENDENCIA"                 "POMACOCHA"                    
# [67] "RAZURI"                        "MONSEFU"                      
# [69] "SECHURA"                       "TUMBES"                       
# [71] "ZORRITOS"                      "NAUTA"                        
# [73] "CORRALES"                      "MAJES"                        
# [75] "KAQUIABAMBA"                   "PUNCHANA"                     
# [77] "SAN JUAN BAUTISTA"             "ILLIMO"                       
# [79] "BELEN"                         "VILLA RICA"                   
# [81] "CHIMBOTE"                      "CORACORA"                     
# [83] "PUQUINA"                       "AYACUCHO"                     
# [85] "LS ESPERANZA"                  "PACASMAYO"                    
# [87] "CHAUPIMARCA"                   "SAN MARCOS"                   
# [89] "BARRANCA"                      "INAHUAYA"                     
# [91] "REQUENA"                       "HUARAZ"                       
# [93] "NEPEñA"                       "COISHCO"                      
# [95] "HUARI"                         "PAMPAROMAS"                   
# [97] "SULLANA"                       "HUARMEY"                      
# [99] "OCOñA"                        "MORALES"                      
# [101] "PAIJAN"                        "PUCALLPA"                     
# [103] "YARINACOCHA"                   "ANDRES AVELINO CACERES"       
# [105] "SANTIAGO"                      "SUBTANJALLA"                  
# [107] "PISCO"                         "SAN ANDRES"                   
# [109] "ILO"                           "CATACAOS"                     
# [111] "HUARMACA"                      "LA MATANZA"                   
# [113] "SALITRAL" 
# # Hay  ""
covid_ind$DISTRITO[covid_ind$DISTRITO == "FERREñAFE"] <- "FERRENAFE"
covid_ind$DISTRITO[covid_ind$DISTRITO == "NEPEñA"] <- "NEPENA"
covid_ind$DISTRITO[covid_ind$DISTRITO == "OCOñA"] <- "OCONA"
covid_ind$DISTRITO[covid_ind$DISTRITO == ""] <- NA
unique(covid_ind$DISTRITO)

## Direccion
str(covid_ind$DIRECCION) # logi 
unique(covid_ind$DIRECCION) # NA
# Todos NAs
# [1] NA 50
## Edad
str(covid_ind$EDAD_A) # chr
covid_ind$EDAD_A_n <- as.numeric(covid_ind$EDAD_A) 
str(covid_ind$EDAD_A_n)# ok
table(covid_ind$EDAD_A_n) # ok, from 2 to 96.

covid_ind$EDAD_A[covid_ind$EDAD_A == ""] <- NA

## Sexo
str(covid_ind$SEXOM1H0) # int
table(covid_ind$SEXOM1H0) # 0 and 1
# 0   1 
# 208 150   

## Hospitalizado
str(covid_ind$HOSPITALIZADOS1N0) # int
table(covid_ind$HOSPITALIZADOS1N0) # 0 and 1
# 
# 0   1 
# 253  54

## AisladoDomicil
str(covid_ind$AISLADODOMICILS1N0) # int
table(covid_ind$AISLADODOMICILS1N0) # 0 and 1
# 0   1 
# 58 248  

str(covid_ind$CONTACTO) # chr
unique(covid_ind$CONTACTO) 
# ya est? desagregado en m?s variables

# Dividir en 3 o 4 columnas esta inforamci[on como se indic[o en el googlesheet.
covid_ind$CONTACTO[covid_ind$CONTACTO == ""] <- NA

## IMPORTADO
str(covid_ind$IMPORTADO) # int
unique(covid_ind$IMPORTADO) 
# [1] "1"    "0"    NA
covid_ind$IMPORTADO[covid_ind$IMPORTADO == "LINK"] <- NA
covid_ind$IMPORTADO[covid_ind$IMPORTADO == ""] <- NA

## ORIGEN_INTERNACIONAL
str(covid_ind$ORIGEN_INTERNACIONAL) # chr
unique(covid_ind$ORIGEN_INTERNACIONAL) 
# [1] "EUROPA"        ""              "UK"            "ESPANA"       
# [5] "ITALIA"        "WASHINGTON DC" "EEUU"          "HOLANDA"      
# [9] "ALEMANIA"      "ECUADOR"       "FRANCIA"       "EN RESERVA"   
# [13] "COLOMBIA"      "MEXICO"        "SUDAMERICA"    "CARIBE"       
# [17] "CHINA"         "CHILE"    
# # Hay ""  
covid_ind$ORIGEN_INTERNACIONAL[covid_ind$ORIGEN_INTERNACIONAL == ""] <- NA
unique(covid_ind$ORIGEN_INTERNACIONAL) 
covid_ind$ORIGEN_INTERNACIONAL[covid_ind$ORIGEN_INTERNACIONAL == "LINK 2"] <- NA


## CASO_CONTACTO
str(covid_ind$CASO_CONTACTO) # int
unique(covid_ind$CASO_CONTACTO)
# [1] NA  1 
# Hay NA

## RELACION_CONTACTO
str(covid_ind$RELACION_CONTACTO) # chr
unique(covid_ind$RELACION_CONTACTO)
# [1] NA            "FAMILIAR"    "AMIGO"       ""            "TURISTA"    
# [6] "RELACIONADO" "COMUNITARIO"
# Hay ""  
covid_ind$RELACION_CONTACTO[covid_ind$RELACION_CONTACTO == ""] <- NA

## ORIGEN_NACIONAL
str(covid_ind$ORIGEN_NACIONAL) # chr
unique(covid_ind$ORIGEN_NACIONAL)
# [1] "LIMA"                 "AREQUIPA"             "HUANUCO"             
# [4] "CHINCHA"              ""                     "CUSCO"               
# [7] "NUEVO CHIMBOTE"       "HUANCHACO"            "LA VICTORIA"         
# [10] "PIURA"                "CHICLAYO"             "IQUITOS"             
# [13] "TRUJILLO"             "TAMBOPATA"            "LA BANDA DE SHILCAYO"
# [16] "SANTA"     
# Hay ""  
covid_ind$ORIGEN_NACIONAL[covid_ind$ORIGEN_NACIONAL == ""] <- NA


## Double check "" and count NAs
unique(covid_ind$CASO_ID)
sum(is.na(covid_ind$CASO_ID)) # 6804 NAs

unique(covid_ind$FECHA)
sum(is.na(covid_ind$FECHA)) # 0 NAs

unique(covid_ind$DIA)
sum(is.na(covid_ind$DIA)) # 0 NAs

unique(covid_ind$MES)
sum(is.na(covid_ind$MES)) # 0 NAs

unique(covid_ind$ANO)
sum(is.na(covid_ind$ANO)) # 0 NAs

unique(covid_ind$REGION)
sum(is.na(covid_ind$REGION)) # 0 NAs

unique(covid_ind$PROVINCIA)
sum(is.na(covid_ind$PROVINCIA)) # 5741 NAs

unique(covid_ind$DISTRITO)
sum(is.na(covid_ind$DISTRITO)) # 6155 NAs

unique(covid_ind$DIRECCION)
sum(is.na(covid_ind$DIRECCION)) # 6838NAs

unique(covid_ind$EDAD_A_n)
sum(is.na(covid_ind$EDAD_A_n)) # 6547 NAs

unique(covid_ind$SEXOM1H0)
sum(is.na(covid_ind$SEXOM1H0)) # 6480 NAs

unique(covid_ind$HOSPITALIZADOS1N0)
sum(is.na(covid_ind$HOSPITALIZADOS1N0)) # 6531 NAs

unique(covid_ind$AISLADODOMICILS1N0)
sum(is.na(covid_ind$AISLADODOMICILS1N0)) # 6532 NAs

unique(covid_ind$CONTACTO)
sum(is.na(covid_ind$CONTACTO)) # 6558 NAs

unique(covid_ind$IMPORTADO)
sum(is.na(covid_ind$IMPORTADO)) # 6749 NAs

unique(covid_ind$ORIGEN_INTERNACIONAL)
sum(is.na(covid_ind$ORIGEN_INTERNACIONAL)) # 6798 NAs

unique(covid_ind$CASO_CONTACTO)
sum(is.na(covid_ind$CASO_CONTACTO)) #  6795 NAs

unique(covid_ind$RELACION_CONTACTO)
sum(is.na(covid_ind$RELACION_CONTACTO)) #  6795 NAs

unique(covid_ind$ORIGEN_NACIONAL)
sum(is.na(covid_ind$ORIGEN_NACIONAL)) # 6755 NAs


# Save dataset
names(covid_ind)
# [1] "CASO_ID"              "FECHA"                "DIA"                 
# [4] "MES"                  "ANO"                  "REGION"              
# [7] "PROVINCIA"            "DISTRITO"             "DIRECCION"           
# [10] "EDAD_A"               "SEXOM1H0"             "HOSPITALIZADOS1N0"   
# [13] "AISLADODOMICILS1N0"   "CONTACTO"             "IMPORTADO"           
# [16] "ORIGEN_INTERNACIONAL" "CASO_CONTACTO"        "RELACION_CONTACTO"   
# [19] "ORIGEN_NACIONAL"      "ENLACES1"             "ENLACES2"            
# [22] "ENLACES3"             "OBSERVACIONES"        "EDAD_A_n"      

covid_ind <- covid_ind[,c(1:10,24,11:23)]
names(covid_ind)
# [1] "CASO_ID"              "FECHA"                "DIA"                 
# [4] "MES"                  "ANO"                  "REGION"              
# [7] "PROVINCIA"            "DISTRITO"             "DIRECCION"           
# [10] "EDAD_A"               "EDAD_A_n"             "SEXOM1H0"            
# [13] "HOSPITALIZADOS1N0"    "AISLADODOMICILS1N0"   "CONTACTO"            
# [16] "IMPORTADO"            "ORIGEN_INTERNACIONAL" "CASO_CONTACTO"       
# [19] "RELACION_CONTACTO"    "ORIGEN_NACIONAL"      "ENLACES1"            
# [22] "ENLACES2"             "ENLACES3"             "OBSERVACIONES" 

# Save csv.
# CAMBIA EL NOMBRE! NO TE OLVIDES!
#write.csv(covid_ind, "data/modificadas/covidPE_IND_20200403_MD_clean.csv")
#write.csv(covid_ind, "data/modificadas/covidPE_IND_20200404_MD_clean.csv")
#write.csv(covid_ind, "data/modificadas/covidPE_IND_20200405_CA_clean.csv")
#write.csv(covid_ind, "data/modificadas/covidPE_IND_20200406_CA_clean.csv")
#write.csv(covid_ind, "data/modificadas/covidPE_IND_20200407_CA_clean.csv")
#write.csv(covid_ind, "data/modificadas/covidPE_IND_20200408_MD_clean.csv")
#write.csv(covid_ind, "data/modificadas/covidPE_IND_20200409_CA_clean.csv")
# write.csv(covid_ind, "data/modificadas/covidPE_IND_20200410_MD_clean.csv")
write.csv(covid_ind, "data/modificadas/covidPE_IND_20200411_CA_clean.csv")

# Y PONLE UN # ANTES DE GUARDAR EL SCRIPT!

##########################################################################################