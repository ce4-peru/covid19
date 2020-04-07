###### limpieza casos por indartamento #####

library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
## set directory
setwd("~/covid19")


## Call data
#covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200403_MD.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200404_MD.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200405_CA.csv")
covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200406_MD.csv")

## Checar base
str(covid_dia) # 34 obs. of  14 variables
names(covid_dia)
# [1] "FECHA"                     "TOTAL_POSITIVOS"           "TOTAL_PRUEBAS"            
# [4] "TOTAL_DESCARTADOS"         "POSITIVOS_DIA"             "PRUEBAS_DIA"              
# [7] "DESCARTADOS_DIA"           "RECUPERADOS"               "FALLECIDOS"               
# [10] "HOSPITALIZADOS"            "HOSPITALIZADOS_UCI"        "HOSPITALIZADOS_VENTILADOR"
# [13] "FUENTE"                    "ACTUALIZACION"             "OBSERVACION"

# Mas adelante  Sacar los "S1N0 del nombre de la variable

head(covid_dia)
# Transformar todo texto a mayusculas

# Transform to character
covid_dia <- mutate_if(covid_dia, is.character, toupper)
sapply(covid_dia, class)
# FECHA           TOTAL_POSITIVOS             TOTAL_PRUEBAS 
# "character"                 "integer"                 "integer" 
# TOTAL_DESCARTADOS             POSITIVOS_DIA               PRUEBAS_DIA 
# "integer"                 "integer"                 "integer" 
# DESCARTADOS_DIA               RECUPERADOS                FALLECIDOS 
# "integer"                 "integer"                 "integer" 
# HOSPITALIZADOS        HOSPITALIZADOS_UCI HOSPITALIZADOS_VENTILADOR 
# "integer"                 "integer"                 "integer" 
# FUENTE            ACTUALIZACI?"N 
# "character"               "character"

### variable by variable
### Fecha ####
# table(covid_dia$FECHA)
# 01/04/2020 02/04/2020 03/04/2020 06/03/2020 07/03/2020 08/03/2020 09/03/2020 10/03/2020 11/03/2020 
# 1          1          1          1          1          1          1          1          1 
# 12/03/2020 13/03/2020 14/03/2020 15/03/2020 16/03/2020 17/03/2020 18/03/2020 19/03/2020 20/03/2020 
# 1          1          1          1          1          1          1          1          1 
# 21/03/2020 22/03/2020 23/03/2020 24/03/2020 25/03/2020 26/03/2020 27/03/2020 28/03/2020 29/03/2020 
# 1          1          1          1          1          1          1          1          1 
# 30/03/2020 31/03/2020 
# 1          1


#### separando caracteres en varias columnas ######
covid_dia$FECHA2 <- covid_dia$FECHA
covid_dia <- separate(covid_dia,
         col = "FECHA2",
         into = c("DIA", "MES","ANO"),
         sep = "/")

##### fecha to as.date format ####
covid_dia<-covid_dia %>%
  mutate(FECHA = as.Date(as.character(FECHA), format = "%d/%m/%y"))

### ordenar base ###
names(covid_dia)
# [1] "FECHA"                     "TOTAL_POSITIVOS"           "TOTAL_PRUEBAS"            
# [4] "TOTAL_DESCARTADOS"         "POSITIVOS_DIA"             "PRUEBAS_DIA"              
# [7] "DESCARTADOS_DIA"           "RECUPERADOS"               "FALLECIDOS"               
# [10] "HOSPITALIZADOS"            "HOSPITALIZADOS_UCI"        "HOSPITALIZADOS_VENTILADOR"
# [13] "FUENTE"                    "ACTUALIZACION"             "OBSERVACION"              
# [16] "DIA"                       "MES"                       "ANO"   

covid_dia <- covid_dia[,c(1, 16:18, 2:15)]


### Check NAs and inconsistencies ###

# ID del caso
str(covid_dia$CASO_ID) # int
unique(covid_dia$CASO_ID)
### Para manejo nuestro, completar n[umero de casos en google spreadsheet.

## Fecha 
str(covid_dia$FECHA) # date
unique(covid_dia$FECHA)
sum(is.na(covid_dia$FECHA)) # 0 NAs

unique(covid_dia$TOTAL_POSITIVOS)
sum(is.na(covid_dia$TOTAL_POSITIVOS)) # 0 NAs

unique(covid_dia$TOTAL_PRUEBAS)
sum(is.na(covid_dia$TOTAL_PRUEBAS)) # 0 NAs

unique(covid_dia$TOTAL_DESCARTADOS)
sum(is.na(covid_dia$TOTAL_DESCARTADOS)) # 0 NAs

unique(covid_dia$POSITIVOS_DIA)
sum(is.na(covid_dia$POSITIVOS_DIA)) # 0 NAs

unique(covid_dia$PRUEBAS_DIA)
sum(is.na(covid_dia$PRUEBAS_DIA)) # 0 NAs

unique(covid_dia$DESCARTADOS_DIA)
sum(is.na(covid_dia$DESCARTADOS_DIA)) # 0

unique(covid_dia$RECUPERADOS)
sum(is.na(covid_dia$RECUPERADOS)) # 9 NAs

unique(covid_dia$FALLECIDOS)
sum(is.na(covid_dia$FALLECIDOS)) # 12 NAs

unique(covid_dia$HOSPITALIZADOS)
sum(is.na(covid_dia$HOSPITALIZADOS)) # 12 NAs

unique(covid_dia$HOSPITALIZADOS_UCI)
sum(is.na(covid_dia$HOSPITALIZADOS_UCI)) # 19 NAs

unique(covid_dia$HOSPITALIZADOS_VENTILADOR)
sum(is.na(covid_dia$HOSPITALIZADOS_VENTILADOR)) # 19 NAs

names(covid_dia)
# [1] "FECHA"                     "DIA"                       "MES"                      
# [4] "ANO"                       "TOTAL_POSITIVOS"           "TOTAL_PRUEBAS"            
# [7] "TOTAL_DESCARTADOS"         "POSITIVOS_DIA"             "PRUEBAS_DIA"              
# [10] "DESCARTADOS_DIA"           "RECUPERADOS"               "FALLECIDOS"               
# [13] "HOSPITALIZADOS"            "HOSPITALIZADOS_UCI"        "HOSPITALIZADOS_VENTILADOR"
# [16] "FUENTE"                    "ACTUALIZACION"             "OBSERVACION" 

# Save csv.
# CAMBIA EL NOMBRE! NO TE OLVIDES!
#write.csv(covid_dia, "data/modificadas/covidPE_PORdia_20200406_MD_clean.csv")

# Y PONLE UN # ANTES DE GUARDAR EL SCRIPT!
