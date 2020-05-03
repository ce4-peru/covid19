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
#covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200406_MD.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200407_CA.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200408_MD.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200409_CA.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200410_MD.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200411_CA.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200412_MD.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200413_CA.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200414_CA.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200415_GF.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200416_CA.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200417_AL.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200418_MD.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200419_GF.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200420_MD.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200421_CA.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200422_GF.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200423_CA.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200424_AL.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200425_MD.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200426_GF.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200427_MD.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200428_AL.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200429_GF.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200430_AL.csv")
# covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200501_AL.csv")
covid_dia <- fread("~/covid19/data/crudas/covidPE_pordia_20200502_MD.csv")

## Checar base
str(covid_dia) # 57 obs. of  14 variables
names(covid_dia)
# [1] "FECHA"              "TOTAL_POSITIVOS"    "TOTAL_PRUEBAS"      "TOTAL_DESCARTADOS" 
# [5] "POSITIVOS_DIA"      "PRUEBAS_DIA"        "DESCARTADOS_DIA"    "RECUPERADOS"       
# [9] "FALLECIDOS"         "HOSPITALIZADOS"     "HOSPITALIZADOS_UCI" "OBSERVACION"       
# [13] "FUENTE"             "ACTUALIZACION" 

# Mas adelante  Sacar los "S1N0 del nombre de la variable

head(covid_dia)
# Transformar todo texto a mayusculas

# Transform to character
covid_dia <- mutate_if(covid_dia, is.character, toupper)
sapply(covid_dia, class)
# FECHA             TOTAL_POSITIVOS     TOTAL_PRUEBAS       TOTAL_DESCARTADOS 
# "character"       "integer"           "integer"           "integer" 
# POSITIVOS_DIA     PRUEBAS_DIA         DESCARTADOS_DIA     RECUPERADOS 
# "integer"         "integer"           "integer"           "integer" 
# FALLECIDOS        HOSPITALIZADOS      HOSPITALIZADOS_UCI  OBSERVACION 
# "integer"         "integer"           "integer"           "character" 
# FUENTE            ACTUALIZACION 
# "character"       "character"

### variable by variable
### Fecha ####
table(covid_dia$FECHA)
# 01/04/2020 01/05/2020 02/04/2020 03/04/2020 04/04/2020 05/04/2020 06/03/2020 06/04/2020 
# 1          1          1          1          1          1          1          1 
# 07/03/2020 07/04/2020 08/03/2020 08/04/2020 09/03/2020 09/04/2020 10/03/2020 10/04/2020 
# 1          1          1          1          1          1          1          1 
# 11/03/2020 11/04/2020 12/03/2020 12/04/2020 13/03/2020 13/04/2020 14/03/2020 14/04/2020 
# 1          1          1          1          1          1          1          1 
# 15/03/2020 15/04/2020 16/03/2020 16/04/2020 17/03/2020 17/04/2020 18/03/2020 18/04/2020 
# 1          1          1          1          1          1          1          1 
# 19/03/2020 19/04/2020 20/03/2020 20/04/2020 21/03/2020 21/04/2020 22/03/2020 22/04/2020 
# 1          1          1          1          1          1          1          1 
# 23/03/2020 23/04/2020 24/03/2020 24/04/2020 25/03/2020 25/04/2020 26/03/2020 26/04/2020 
# 1          1          1          1          1          1          1          1 
# 27/03/2020 27/04/2020 28/03/2020 28/04/2020 29/03/2020 29/04/2020 30/03/2020 30/04/2020 
# 1          1          1          1          1          1          1          1 
# 31/03/2020 
# 1 

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
# [1] "FECHA"              "TOTAL_POSITIVOS"    "TOTAL_PRUEBAS"      "TOTAL_DESCARTADOS" 
# [5] "POSITIVOS_DIA"      "PRUEBAS_DIA"        "DESCARTADOS_DIA"    "RECUPERADOS"       
# [9] "FALLECIDOS"         "HOSPITALIZADOS"     "HOSPITALIZADOS_UCI" "OBSERVACION"       
# [13] "FUENTE"             "ACTUALIZACION"      "DIA"                "MES"               
# [17] "ANO" 

covid_dia <- covid_dia[,c(1, 15:17, 2:14)]


### Check NAs and inconsistencies ###

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

# unique(covid_dia$HOSPITALIZADOS_VENTILADOR)
# sum(is.na(covid_dia$HOSPITALIZADOS_VENTILADOR)) # 22 NAs

names(covid_dia)
# [1] "FECHA"              "DIA"                "MES"                "ANO"               
# [5] "TOTAL_POSITIVOS"    "TOTAL_PRUEBAS"      "TOTAL_DESCARTADOS"  "POSITIVOS_DIA"     
# [9] "PRUEBAS_DIA"        "DESCARTADOS_DIA"    "RECUPERADOS"        "FALLECIDOS"        
# [13] "HOSPITALIZADOS"     "HOSPITALIZADOS_UCI" "OBSERVACION"        "FUENTE"            
# [17] "ACTUALIZACION"   

# Save csv.
# CAMBIA EL NOMBRE! NO TE OLVIDES!
#write.csv(covid_dia, "data/modificadas/covidPE_PORdia_20200502_MD_clean.csv")
# Y PONLE UN # ANTES DE GUARDAR EL SCRIPT!
####