###### limpieza casos por indartamento #####

library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
## set directory
setwd("~/covid19")

## Call data
covid_dep <- fread("~/covid19/data/crudas/covidPE_DEP_20200412_MD.csv")

## Checar base
str(covid_dep) # 38 obs. of  31 variables
names(covid_dep)
# [1] "DÃ?a"        "Fecha"       "LIM"         "ARE"         "HUC"        
# [6] "ICA"         "CUS"         "PIU"         "LAM"         "CAL"        
# [11] "LAL"         "ANC"         "LOR"         "SAM"         "MDD"        
# [16] "JUN"         "TUM"         "CAJ"         "PAS"         "TAC"        
# [21] "AYA"         "HUA"         "APU"         "MOQ"         "PUN"        
# [26] "AMA"         "UCA"         "Total"       "Acumulado"   "Descartados"
# [31] "Fuente

# Eliminar primera columna
covid_dep <- covid_dep[-38,-c(1,28:31)]
# Mas adelante  Sacar los "S1N0 del nombre de la variable

head(covid_dep)
# Transformar todo texto a mayusculas

# Transform to character
# No me sale
covid_dep <- mutate_if(covid_dep, is.character, toupper)
sapply(covid_dep, class)
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
table(covid_dep$Fecha)
# 1-ABR  10-ABR  10-MAR  11-ABR  11-MAR  12-MAR  13-MAR  14-MAR  15-MAR  16-MAR  17-MAR  18-MAR 
# 1       1       1       1       1       1       1       1       1       1       1       1 
# 19-MAR   2-ABR  20-MAR  21-MAR  22-MAR  23-MAR  24-MAR  25-MAR  26-MAR  27-MAR  28-MAR  29-MAR 
# 1       1       1       1       1       1       1       1       1       1       1       1 
# 3-ABR  30-MAR  31-MAR   4-ABR 5 ABRIL   6-ABR   6-MAR   7-ABR   7-MAR   8-ABR   8-MAR   9-ABR 
# 1       1       1       1       1       1       1       1       1       1       1       1 
# 9-MAR TOTALES 
# 1       1 

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
write.csv(covid_dep2, "data/modificadas/covidPE_IND_20200412_MD_clean.csv")

# Y PONLE UN # ANTES DE GUARDAR EL SCRIPT!

##########################################################################################