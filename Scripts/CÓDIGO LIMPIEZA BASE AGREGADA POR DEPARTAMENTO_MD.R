###### limpieza casos por indartamento #####

library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
## set directory
setwd("~/covid19")

## Call data
# covid_dep <- fread("~/covid19/data/crudas/covidPE_DEP_20200412_MD.csv")
covid_dep <- fread("~/covid19/data/crudas/covidPE_DEP_20200413_CA.csv")

## Checar base
str(covid_dep) # 39 obs. of  31 variables
names(covid_dep)
# [1] "Día"         "Fecha"       "LIM"         "ARE"         "HUC"         "ICA"        
# [7] "CUS"         "PIU"         "LAM"         "CAL"         "LAL"         "ANC"        
# [13] "LOR"         "SAM"         "MDD"         "JUN"         "TUM"         "CAJ"        
# [19] "PAS"         "TAC"         "AYA"         "HUA"         "APU"         "MOQ"        
# [25] "PUN"         "AMA"         "UCA"         "Total"       "Acumulado"   "Descartados"
# [31] "Fuente"     
# > 

# Eliminar primera columna
covid_dep <- covid_dep[-38,-c(1,28:31)]
# Mas adelante  Sacar los "S1N0 del nombre de la variable

head(covid_dep)
# Transformar todo texto a mayusculas

# Transform to character
# No me sale
covid_dep <- mutate_if(covid_dep, is.character, toupper)
sapply(covid_dep, class)
# Fecha         LIM         ARE         HUC         ICA         CUS         PIU 
# "character"   "integer"   "integer"   "integer"   "integer"   "integer"   "integer" 
# LAM         CAL         LAL         ANC         LOR         SAM         MDD 
# "integer"   "integer"   "integer"   "integer"   "integer"   "integer"   "integer" 
# JUN         TUM         CAJ         PAS         TAC         AYA         HUA 
# "integer"   "integer"   "integer"   "integer"   "integer"   "integer"   "integer" 
# APU         MOQ         PUN         AMA         UCA 
# "integer"   "integer"   "integer"   "integer"   "integer" 
# > 

### variableby variable
table(covid_dep$Fecha)
# 01/04/2020 02/04/2020 03/04/2020 04/04/2020 05/04/2020 06/03/2020 06/04/2020 07/03/2020 
# 1          1          1          1          1          1          1          1 
# 07/04/2020 08/03/2020 08/04/2020 09/03/2020 09/04/2020 10/03/2020 10/04/2020 11/03/2020 
# 1          1          1          1          1          1          1          1 
# 11/04/2020 12/03/2020 13/03/2020 14/03/2020 15/03/2020 16/03/2020 17/03/2020 18/03/2020 
# 1          1          1          1          1          1          1          1 
# 19/03/2020 20/03/2020 21/03/2020 22/03/2020 23/03/2020 24/03/2020 25/03/2020 26/03/2020 
# 1          1          1          1          1          1          1          1 
# 27/03/2020 28/03/2020 29/03/2020 30/03/2020 31/03/2020    TOTALES 
# 1          1          1          1          1          1 

#### separando caracteres en varias columnas ######
# covid_ind$FECHA2 <- covid_ind$FECHA
# covid_ind <- separate(covid_ind,
#          col = "FECHA2",
#          into = c("DIA", "MES","ANO"),
#          sep = "/")

# ##### fecha to as.date format ####
# covid_ind<-covid_ind %>%
#   mutate(FECHA = as.Date(as.character(FECHA), format = "%d/%m/%y"))

# ### ordenar base ###
# names(covid_dep)
# # [1] "CASO_ID"              "FECHA"                "REGION"              
# # [4] "PROVINCIA"            "DISTRITO"             "DIRECCION"           
# # [7] "EDAD_A"               "SEXOM1H0"             "HOSPITALIZADOS1N0"   
# # [10] "AISLADODOMICILS1N0"   "CONTACTO"             "IMPORTADO"           
# # [13] "ORIGEN_INTERNACIONAL" "CASO_CONTACTO"        "RELACION_CONTACTO"   
# # [16] "ORIGEN_NACIONAL"      "ENLACES1"             "ENLACES2"            
# # [19] "ENLACES3"             "OBSERVACIONES"        "DIA"                 
# # [22] "MES"                  "ANO"  
# 
# covid_dep <- covid_dep[,c(1, 2, 21:23, 3:20)]


# Tidy data to work with it
covid_dep2 <- gather(covid_dep,
                   key = "REGION",
                   value = "CASOS",
                   -Fecha)
covid_dep2

covid_dep2$CASOS[is.na(covid_dep2$CASOS)] <- 0


# Save csv.
# CAMBIA EL NOMBRE! NO TE OLVIDES!
# write.csv(covid_dep2, "data/modificadas/covidPE_IND_20200412_MD_clean.csv")
write.csv(covid_dep2, "data/modificadas/covidPE_DEP_20200413_CA_clean.csv", row.names = FALSE)

# Y PONLE UN # ANTES DE GUARDAR EL SCRIPT!

##########################################################################################