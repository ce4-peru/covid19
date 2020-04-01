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
#covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200331_MD.csv")
covid_ind <- fread("~/covid19/data/crudas/covidPE_IND_20200401_MD.csv")

## Checar base
str(covid_ind) # 1064 obs. of  20 variables
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
# 06/03/2020 07/03/2020 09/03/2020 10/03/2020 11/03/2020 12/03/2020 13/03/2020 
# 1          5          3          2          6          5         16 
# 14/03/2020 15/03/2020 16/03/2020 17/03/2020 18/03/2020 19/03/2020 20/03/2020 
# 5         28         15         31         29         89         29 
# 21/03/2020 22/03/2020 23/03/2020 24/03/2020 25/03/2020 26/03/2020 27/03/2020 
# 55         43         32         21         64         99         55 
# 28/03/2020 29/03/2020 
# 36        181 

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
# [1] "LIMA"          "AREQUIPA"      "HUáNUCO"      "ICA"           "CUSCO"        
# [6] "ANCASH"        "CALLAO"        "LA LIBERTAD"   "LAMBAYEQUE"    "PIURA"        
# [11] "LORETO"        "MADRE DE DIOS" "SAN MARTIN"    "JUNIN"         "TUMBES"       
# [16] "CAJAMARCA"    
# "Huánuco" con tilde

# Cambiar Huanuco
levels(covid_ind$REGION) <- c(levels(covid_ind$REGION), 
                            c("APURIMAC", "HUANUCO", 
                              "JUNIN", "SAN MARTIN"))

covid_ind$REGION[covid_ind$REGION == "HUáNUCO"] <- "HUANUCO"
unique(covid_ind$REGION) # ok


## Provincia
str(covid_ind$PROVINCIA) # chr 
unique(covid_ind$PROVINCIA)
# [1] "LIMA"       "AREQUIPA"   "HUANUCO"    "CHINCHA"    "CUSCO"      ""          
# [7] "SANTA"      "CALLAO"     "TRUJILLO"   "CHICLAYO"   "PIURA"      "MAYNAS"    
# [13] "TAMBOPATA"  "SAN MARTIN" "HUANCAYO"   "FERREñAFE" "SULLANA"    "ICA"       
# [19] "TUMBES"     "CAJAMARCA"
# Hay  ""
#  "FERREñAFE" con n.

levels(covid_ind$PROVINCIA) <- c(levels(covid_ind$PROVINCIA), 
                              c("APURIMAC", "HUANUCO", 
                                "JUNIN", "SAN MARTIN", "FERRENAAFE"))

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

# Hay  ""
covid_ind$DISTRITO[covid_ind$DISTRITO == ""] <- NA
unique(covid_ind$DISTRITO)


## Direccion
str(covid_ind$DIRECCION) # logi 
unique(covid_ind$DIRECCION) # NA
# Todos NAs

## Edad
str(covid_ind$EDAD_A) # chr
covid_ind$EDAD_A_n <- as.numeric(covid_ind$EDAD_A) 
str(covid_ind$EDAD_A_n)# ok
table(covid_ind$EDAD_A_n) # ok, from 2 to 96.

covid_ind$EDAD_A[covid_ind$EDAD_A == ""] <- NA

## Sexo
str(covid_ind$SEXOM1H0) # int
table(covid_ind$SEXOM1H0) # 0 and 1
# 0  1 
# 78 54

## Hospitalizado
str(covid_ind$HOSPITALIZADOS1N0) # int
table(covid_ind$HOSPITALIZADOS1N0) # 0 and 1
# 0  1 
# 93 27 

## AisladoDomicil
str(covid_ind$AISLADODOMICILS1N0) # int
table(covid_ind$AISLADODOMICILS1N0) # 0 and 1
# 0  1 
# 26 93 

## cONTACTO
str(covid_ind$CONTACTO) # CHR
unique(covid_ind$CONTACTO) 
# [1] "EUROPA (RECORRIO FRANCIA, ESPAñA Y REPUBLICA CHECA)"
# [2] "FAMILIAR DEL CASO 1"                                 
# [3] "AMIGO DEL CASO 1"                                    
# [4] "REINO UNIDO (LONDRES, INGLATERRA)"                   
# [5] "ENTORNO DEL CASO 1"                                  
# [6] "ESPANA"                                              
# [7] "ESPANA E ITALIA"                                     
# [8] "FAMILIAR DEL CASO 6"                                 
# [9] ""                                                    
# [10] "ITALIA"                                              
# [11] "ALEMANIA Y ESPANA"                                   
# [12] "ESPANA Y LONDRES"                                    
# [13] "EEUU (WASHINGTON DC)"                                
# [14] "ESPANA (MADRID)"                                     
# [15] "ESPAñA E ITALIA"                                    
# [16] "ESPAñA"                                             
# [17] "ESTADOS UNIDOS"                                      
# [18] "CONTACTO CON EXTRANJEROS"                            
# [19] "CONTACTO CON TURISTAS"                               
# [20] "RELACIONADO AL PRIMER CASO DE ACASH"                 
# [21] "HERMANA DEL CASO DEL 17/03/2020"                     
# [22] "FAMILIAR DEL CASO DEL 17/03/2020"                    
# [23] "LIMA"                                                
# [24] "CONTACTO CON TURISTAS HOLANDESES"                    
# [25] "TURISTA ALEMAN"                                      
# [26] "TURISTA"                                             
# [27] "CONTACTO CON CASO DEL 17/03/2020"                    
# [28] "RELACIONADO AL CASO 3 DE ANCASH"                     
# [29] "ESPAñA Y FRANCIA"                                   
# [30] "ECUADOR"                                             
# [31] "EN INVESTIGACIóN"                                   
# [32] "FAMILIAR DEL CASO 1 DE LAS LOMAS"                    
# [33] "CONTACTO CON FAMILIAR QUE VINO DEL EXTRANGERO"       
# [34] "CONTACTO CON EL CASO 1 DE LAS LOMAS"                 
# [35] "MADRID Y BARCELONA"                                  
# [36] "FRANCIA (PARIS)"                                     
# [37] "CONTACTO CON TURISTAS EUROPEO"                       
# [38] "CONTACTO CON TURISTAS EXTRANGEROS"                   
# [39] "FAMILIAR DE CASO 3"                                  
# [40] "CONTACTO CON CIUDADANOS ESPAñOLES"                  
# [41] "COLOMBIA"                                            
# [42] "TURISTA MEXICANO"                                    
# [43] "TURISTA EUROPEO"                                     
# [44] "TURISTA SUDAMERICANO"

# Dividir en 3 o 4 columnas esta inforamci[on como se indic[o en el googlesheet.
covid_ind$CONTACTO[covid_ind$CONTACTO == ""] <- NA

## IMPORTADO
str(covid_ind$IMPORTADO) # int
unique(covid_ind$IMPORTADO) 
# [1] "1"    "0"    ""     "LINK"
covid_ind$IMPORTADO[covid_ind$IMPORTADO == "LINK"] <- NA
covid_ind$IMPORTADO[covid_ind$IMPORTADO == ""] <- NA

## ORIGEN_INTERNACIONAL
str(covid_ind$ORIGEN_INTERNACIONAL) # chr
unique(covid_ind$ORIGEN_INTERNACIONAL) 
# [1] "EUROPA"        ""              "UK"            "ESPANA"        "ITALIA"       
# [6] "WASHINGTON DC" "EEUU"    
# Hay ""  
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
# [1] NA     "FAMILIAR"    "AMIGO"       ""    "TURISTA"     "RELACIONADO"
# Hay ""  
covid_ind$RELACION_CONTACTO[covid_ind$RELACION_CONTACTO == ""] <- NA

## ORIGEN_NACIONAL
str(covid_ind$ORIGEN_NACIONAL) # chr
unique(covid_ind$ORIGEN_NACIONAL)
# [1] "LIMA"           "AREQUIPA"       "HUANUCO"        "CHINCHA"       
# [5] ""               "CUSCO"          "NUEVO CHIMBOTE" "HUANCHACO"     
# [9] "LA VICTORIA"    "PIURA"          "CHICLAYO"       "IQUITOS"       
# [13] "TRUJILLO"    
# Hay ""  
covid_ind$ORIGEN_NACIONAL[covid_ind$ORIGEN_NACIONAL == ""] <- NA


## Double check "" and count NAs
unique(covid_ind$CASO_ID)
sum(is.na(covid_ind$CASO_ID)) # 816 NAs

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
sum(is.na(covid_ind$PROVINCIA)) # 650 NAs

unique(covid_ind$DISTRITO)
sum(is.na(covid_ind$DISTRITO)) # 717 NAs

unique(covid_ind$DIRECCION)
sum(is.na(covid_ind$DIRECCION)) # 850 NAs

unique(covid_ind$EDAD_A_n)
sum(is.na(covid_ind$EDAD_A_n)) # 747 NAs

unique(covid_ind$SEXOM1H0)
sum(is.na(covid_ind$SEXOM1H0)) # 718 NAs

unique(covid_ind$HOSPITALIZADOS1N0)
sum(is.na(covid_ind$HOSPITALIZADOS1N0)) # 730 NAs

unique(covid_ind$AISLADODOMICILS1N0)
sum(is.na(covid_ind$AISLADODOMICILS1N0)) # 731 NAs

unique(covid_ind$CONTACTO)
sum(is.na(covid_ind$CONTACTO)) # 746 NAs

unique(covid_ind$IMPORTADO)
sum(is.na(covid_ind$IMPORTADO)) # 815 NAs

unique(covid_ind$ORIGEN_INTERNACIONAL)
sum(is.na(covid_ind$ORIGEN_INTERNACIONAL)) # 833 NAs

unique(covid_ind$CASO_CONTACTO)
sum(is.na(covid_ind$CASO_CONTACTO)) # 832 NAs

unique(covid_ind$RELACION_CONTACTO)
sum(is.na(covid_ind$RELACION_CONTACTO)) # 832 NAs

unique(covid_ind$ORIGEN_NACIONAL)
sum(is.na(covid_ind$ORIGEN_NACIONAL)) # 815 NAs


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
# CHANGE NAME! DONT FORGET!
write.csv(covid_ind, "data/modificadas/covidPE_IND_20200331_MD_clean.csv")
